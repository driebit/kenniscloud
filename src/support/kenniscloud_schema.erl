%% @author Driebit <tech@driebit.nl>
%% @copyright 2025 Driebit

%% Copyright 2025 Driebit
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @doc Kenniscloud data fixtures
-module(kenniscloud_schema).

-export([
    manage_schema/2,
    manage_data/2,

    example_event/0
]).

-include_lib("zotonic_core/include/zotonic.hrl").


manage_schema(install, Context) ->
    z_module_manager:reinstall(mod_crowdlink, Context),
    z_module_manager:reinstall(mod_crowdparticipant, Context),

    m_config:set_default_value(mod_authentication, is_one_step_logon, true, Context),

    % Signup defaults previously setup by 'mod_ginger_auth':
    m_config:set_default_value(mod_signup, username_equals_email, true, Context),
    m_config:set_default_value(mod_signup, request_confirm, true, Context),

    % By default, save and keep track of user activity
    m_config:set_default_value(mod_driebit_activity2, persist_activity, true, Context),

    Environment = m_site:environment(Context),
    [get_prod_data(), get_dev_data(Environment)];
manage_schema({upgrade, N}, _Context) when N >= 2 andalso N =< 17 ->
    ok;
manage_schema({upgrade, 18}, _Context) ->
    #datamodel{
        predicates=[
            {has_subgroup, [
                    {title, {trans, [{nl, <<"Subgroep">>},
                                     {en, <<"Subgroup">>}]}},
                    {language, [en,nl]}
                ], [
                    {acl_collaboration_group, acl_collaboration_group}
            ]}
        ]
    }.

manage_data(install, Context) ->
    case m_site:environment(Context) of
        development ->
            setup_dev_users(Context),
            install_main_menu(Context),
            install_footer_menu(Context);
        acceptance ->
            install_footer_menu(Context);
        _ ->
            ok
    end,
    ensure_title(keyword, {trans, [{nl, <<"Tag">>}, {en, <<"Tag">>}]}, Context),
    ensure_title(subject, {trans, [{nl, <<"Tag">>}, {en, <<"Tag">>}]}, Context),
    ensure_title(person, {trans, [{nl, <<"Lid">>}, {en, <<"Member">>}]}, Context),
    update_usergroup_hierarchy(Context),
    install_acl_rules(Context),
    setup_upload_rights(Context),
    ok;
manage_data({upgrade, N}, _Context) when N >= 2 andalso N =< 17 ->
    ok;
manage_data({upgrade, 18}, Context) ->
    % If there is still a 'project' category
    case m_category:name_to_id(project, Context) of
        {ok, CatId} ->
            ?zInfo("Data upgrade, converting projects to kennisgroepen", [], Context),
            % Convert each 'project' into an 'acl_collaboration_group' (kennisgroep)
            case m_category:foreach(CatId, fun migrate_project/2, Context) of
                ok ->
                    % And remove (merge) the 'project' category
                    ToCatId = m_rsc:rid(acl_collaboration_group, Context),
                    m_rsc:update(CatId, #{<<"is_protected">> => false}, Context),
                    m_category:delete(CatId, ToCatId, Context);
                Error ->
                    ?zError(
                        "Project to kennisgroep conversion failed, error: ~p",
                        [ Error ],
                        Context
                    ),
                    ok
            end;
        {error, _} ->
            ok
    end,
    remove_unused_predicate(hasproject, Context),
    remove_unused_predicate(hasprojectmanager, Context),
    remove_unused_predicate(hasexpert_pioneer, Context),
    remove_unused_predicate(hasexpert_speaker, Context).

migrate_project(ProjId, Context0) ->
    ?zInfo("Data upgrade, turning ~p into a kennisgroep", [ProjId], Context0),
    z_db:transaction(
        fun (Context) ->
            % First, turn the project into a kennisgroep:
            {ok, ProjId} = m_rsc:update(ProjId, #{<<"category">> => acl_collaboration_group}, Context),

            % convert 'hasproject' incoming edges:
            lists:foreach(
                fun (RscId) ->
                    IsA = m_rsc:is_a(RscId, Context),
                    IsPerson = lists:member(person, IsA),
                    IsEvent = lists:member(event, IsA),
                    IsKennisgroep = lists:member(acl_collaboration_group, IsA),
                    if
                        % from 'person' -> replace with 'hascollabmember' (outgoing)
                        IsPerson -> m_edge:insert(ProjId, hascollabmember, RscId, Context);
                        % from 'event': replace with content group
                        IsEvent -> m_rsc:update(RscId, #{<<"content_group_id">> => ProjId}, Context);
                        % from 'acl_collaboration_group': replace with 'has_subgroup' (outgoing)
                        IsKennisgroep -> m_edge:insert(ProjId, has_subgroup, RscId, Context);
                        % otherwise (unexpected), replace with content group
                        true -> m_rsc:update(RscId, #{<<"content_group_id">> => ProjId}, Context)
                    end,
                    ok = m_edge:delete(RscId, hasproject, ProjId, Context)
                end,
                m_edge:subjects(ProjId, hasproject, Context)
            ),

            % switch outgoing edges of 'hasprojectmanager' to 'hascollabmanager'
            Managers = m_edge:objects(ProjId, hasprojectmanager, Context),
            m_edge:replace(ProjId, hasprojectmanager, [], Context),
            m_edge:replace(ProjId, hascollabmanager, Managers, Context),

            % 'author', 'hasexpertjournalist', 'hasexpertscientist' and
            % 'hasprojectinitiator' outgoing edges remain valid and we keep them
            ok
        end,
        Context0
    ).

remove_unused_predicate(Predicate, Context) ->
    case m_predicate:is_used(Predicate, Context) of
        false ->
            case m_rsc:exists(Predicate, Context) of
                true ->
                    m_rsc:update(Predicate, #{<<"is_protected">> => false}, Context),
                    ok = m_rsc:delete(Predicate, Context);
                false ->
                    ok
            end;
        true ->
            ?zError(
                "Failed to remove predicate: still used",
                [],
                Context
            ),
            ok
    end.


-spec get_dev_data(atom()) -> #datamodel{}.
get_dev_data(production) -> #datamodel{};
get_dev_data(acceptance) -> #datamodel{};
get_dev_data(_Env) -> get_dev_data().

get_dev_data() ->
    ExampleEvent = example_event(),
    ExampleDaycrowdEvent = example_daycrowdevent(),
    #datamodel{
        resources=[
            {page_kennisgroepen, collection, [
                {title, <<"Kennisgroepen">>},
                {subtitle, <<"Sluit je aan bij een kennisgroep">>},
                {summary, <<"Ad debet similique ius, dicant inermis scribentur ut cum. Deleniti delicatissimi ea sit. Deleniti delicatissimi ea sit. Ei pro idque saperet expetendis, vim et illud prodesset.">>},
                {language, [nl]}
            ]},
            {page_subjects, query, [
                {title, <<"Wat speelt er nu?">>},
                {summary, <<"Ad debet similique ius, dicant inermis scribentur ut cum. Deleniti delicatissimi ea sit. Deleniti delicatissimi ea sit. Ei pro idque saperet expetendis, vim et illud prodesset.">>},
                {language, [nl]},
                {query, <<"cat=news\ncat=event\ncat=contribution\nauthoritative\nis_published\nsort=-rsc.publication_start">>}
            ]},
            {page_regions, query, [
                {title, <<"Regio’s"/utf8>>},
                {language, [nl]},
                {query, <<"cat_exact=region\nsort=rsc.pivot_title">>}
            ]},
            {page_agenda, text, [
                {title, <<"Agenda">>},
                {language, [nl]}
            ]},
            {page_community, query, [
                {title, <<"De community">>},
                {language, [nl]}
            ]},
            {page_about, text, [
                {title, <<"Over deze site">>},
                {summary, <<"De KennisCloud is het platform van de bibliotheek waar betrokken mensen en communities uit de regio samen hun kennis delen en creeeren.">>},
                {body, <<"Door de toenemende digitalisering, internet en social media worden we geconfronteerd met een overvloed aan informatie. Het wordt daarom steeds moeilijker de juiste informatie te vinden. Mensen hebben steeds meer behoefte aan informatie in context: informatie wordt daardoor kennis. De bibliotheek kan hier een rol in spelen, want een van de kernfuncties is het toegankelijk maken van informatie. Daarnaast staat de bibliotheek bekend als een neutrale en betrouwbare instelling met een groot netwerk. Door dit netwerk is het makkelijk om verbindingen te leggen. Daarom zijn wij van start gegaan met het project KennisCloud, dat mede ondersteund wordt door de provincie en de Brabantse Netwerkbibliotheek. De KennisCloud stimuleert het kennisdelen en samenwerken van personen, communities en organisaties in de regio. Het is een middel om mensen en organisaties lokaal met elkaar te verbinden: digitaal via eenwebsite en fysiek via bijvoorbeeld de proeftuinbibliotheek in de Spoorzone en verdiepingsbijeenkomsten als Kennis Werkplaatsen. Via de KennisCloud kun je o.a, als persoon, organisatie of community: vragen stellen die je eigen expertise te boven gaan presenteren aan de samenleving wat je qua expertise te bieden hebt partners vinden die je kunnen helpen je vragen te beantwoorden en projecten te realiseren zowel je eigen werk als dat van anderen inhoudelijk verrijken nieuwe ideeën waar nog geen organisatie of community zich mee bezig houdt een plek geven om te groeien en te landen De KennisCloud is nu nog in de pilot fase. De ontwikkeling van de KennisCloud wordt mede mogelijk gemaakt door de Provincie Noord-Brabant en de Brabantse Netwerkbibliotheek"/utf8>>},
                {language, [nl]}
            ]},
            {page_contact, text, [
                {title, <<"Contact">>},
                {body, <<"Heb je vragen of suggesties? Neem contact op met de KennisCloud.">>},
                {language, [nl]}
            ]},
            {page_nieuwestadmaken, acl_collaboration_group, [
                {title, <<"Het nieuwe stadmaken">>},
                {summary, <<"Ad debet similique ius, dicant inermis scribentur ut cum. Deleniti delicatissimi ea sit. Deleniti delicatissimi ea sit. Ei pro idque saperet expetendis, vim et illud prodesset.">>},
                {language, [nl]}
            ]},
            {page_anderekennisgroep, acl_collaboration_group, [
                {title, <<"De andere kennisgroep">>},
                {summary, <<"Ad debet semi similique ius, dicant inermis scribentur ut cum. Deleniti delicatissimi ea sit. Deleniti delicatissimi ea sit. Ei pro idque saperet expetendis, vim et illud prodesset.">>},
                {language, [nl]}
            ]},
            {page_project_collaboration_group, acl_collaboration_group, [
                {title, <<"Kennisgroep in project">>},
                {summary, <<"Ad debet similique ius, dicant inermis scribentur ut cum. Deleniti delicatissimi ea sit. Deleniti delicatissimi ea sit. Ei pro idque saperet expetendis, vim et illud prodesset.">>},
                {language, [nl]}
            ]},
            {contribution_topdown, contribution, [
                {title, <<"Topdown, bottom up, of iets daartussen in">>},
                {summary, <<"Vraagtekens bij de huidige manier van werken">>},
                {language, [nl]},
                {content_group_id, page_nieuwestadmaken}
            ]},
            {contribution_vue, contribution, [
                {title, <<"Vue of Elm">>},
                {summary, <<"Waar kiezen we voor om in te ontwikkelen">>},
                {language, [nl]},
                {content_group_id, page_nieuwestadmaken}
            ]},
            {contribution_other_content_group, contribution, [
                {title, <<"Bijdrage in de andere kennisgroep">>},
                {summary, <<"Vraagtekens en tekenen van uitroep">>},
                {language, [nl]},
                {content_group_id, page_anderekennisgroep}
            ]},
            {contribution_fetch, contribution, [
                {title, <<"Elm Fetch">>},
                {summary, <<"Een experimentje van Ruben Lie">>},
                {language, [nl]},
                {content_group_id, page_project_collaboration_group}
            ]},
            {contribution_acl, contribution, [
                {title, <<"Rechten in kenniscloud">>},
                {summary, <<"Waarom zo ingewikkeld?!">>},
                {language, [nl]},
                {content_group_id, page_project_collaboration_group}
            ]},
            {reference_invloed, reference, [
                {title, <<"Rol in de media">>},
                {summary, <<"Goed artikel waarin aan het eind nadrukkelijk wordt ingegaan op de rol van de invloed van de media.">>},
                {language, [nl]}
            ]},
            {reference_media, reference, [
                {title, <<"Ontwikkeling van de media">>},
                {summary, <<"Van vroeger tot nu">>},
                {language, [nl]}
            ]},
            {event_working, event, [
                {title, <<"Werken in de platformeconomie">>},
                {summary, <<"Hoe beschermen we freelancers bij platformen zoals Deliveroo en Uber.">>},
                {language, [nl]},
                {address_city, <<"Tilburg">>},
                {address_street_1, <<"Burgemeester Brokxlaan">>},
                {content_group_id, page_nieuwestadmaken}
            ]},
            {event_utrecht, event, [
                {title, <<"Utrechtse hoogvliegers">>},
                {summary, <<"The state of art van Rotterdamse architectuur, van FC Utrecht tot industriele speelruimte.">>},
                {language, [nl]}
            ]},
            {project_thermostaat, acl_collaboration_group, [
                {title, "Thermo-staat"}
            ]},
            {organisation_vpro, organisation, [
                {title, "VPRO"}
            ]},
            {organisation_de_waag, organisation, [
                {title, "De Waag"}
            ]},

            % Person
            {person_member, person, [
                {title, <<"Leo Lid">>},
                {summary, <<"Lid van KennisCloud">>},
                {language, [nl]}
            ]},
            {person_community_librarian, person, [
                {title, <<"Conny Community Librarian">>},
                {summary, <<"Community Librarian">>},
                {language, [nl]}
            ]},
            {person_dorien, person, [
                {title, <<"Dorien Drees">>},
                {summary, <<"Webdeveloper">>},
                {language, [nl]}
            ]},
            {person_dirk, person, [
                {title, <<"Dirk Geurs">>},
                {summary, <<"Webdeveloper">>},
                {language, [nl]}
            ]},
            {person_ruben_lie, person, [
                {title, <<"Rubenlie">>},
                {summary, <<"Webdeveloper">>},
                {language, [nl]}
            ]},
            {person_krista, person, [
                {title, <<"Krista">>},
                {summary, <<"Tester">>},
                {language, [nl]}
            ]},
            {person_frederike, person, [
                {title, <<"Frederike">>},
                {summary, <<"Visual designer met een focus op UX">>},
                {language, [nl]}
            ]},

            % Keywords
            {tag_nature, keyword, [
                {title, <<"Natuur en milieu">>},
                {language, [nl]}
            ]},
            {tag_sustainability, keyword, [
                {title, <<"Duurzaamheid">>},
                {language, [nl]}
            ]},
            {tag_innovation, keyword, [
                {title, <<"Innovatie">>},
                {language, [nl]}
            ]},
            {tag_architecture, keyword, [
                {title, <<"Architectuur">>},
                {language, [nl]}
            ]},
            {tag_building, keyword, [
                {title, <<"Bouw">>},
                {language, [nl]}
            ]},
            {tag_media, keyword, [
                {title, <<"Media">>},
                {language, [nl]}
            ]},
            {tag_welzijn, keyword, [
                {title, <<"Welzijn">>},
                {language, [nl]}
            ]},
            {tag_city, keyword, [
                {title, <<"Stedelijke ontwikkeling">>},
                {language, [nl]}
            ]}
        ] ++ element(1, ExampleEvent) ++ element(1, ExampleDaycrowdEvent),
        media=[
            %{default_share_image, schema:file("default-share-image.jpg", Context), []}
        ],
        edges = [
        	{page_nieuwestadmaken, subject, tag_nature},
        	{page_nieuwestadmaken, subject, tag_sustainability},
        	{page_nieuwestadmaken, subject, tag_innovation},
        	{page_anderekennisgroep, subject, tag_architecture},
        	{page_anderekennisgroep, subject, tag_sustainability},
        	{page_anderekennisgroep, subject, tag_welzijn},
        	{page_project_collaboration_group, subject, tag_nature},
        	{page_project_collaboration_group, subject, tag_sustainability},
        	{page_project_collaboration_group, subject, tag_innovation},
        	{contribution_topdown, subject, tag_innovation},
        	{contribution_topdown, subject, tag_sustainability},
        	{contribution_vue, subject, tag_innovation},
        	{reference_invloed, subject, tag_media},
        	{reference_invloed, subject, tag_city},
        	{reference_invloed, subject, tag_innovation},
        	{reference_media, subject, tag_media},
        	{reference_media, subject, tag_welzijn},
        	{reference_media, subject, tag_building},
        	{event_working, subject, tag_welzijn},
        	{event_working, subject, tag_city},
        	{event_utrecht, subject, tag_city},
        	{event_utrecht, subject, tag_innovation},
        	{event_utrecht, subject, tag_sustainability},
        	{event_utrecht, subject, tag_architecture},
        	{event_utrecht, subject, tag_building},

        	{page_nieuwestadmaken, hasregion, region_tilburg},
        	{page_anderekennisgroep, hasregion, region_den_bosch},
        	{event_utrecht, hasregion, region_utrecht},

            {project_thermostaat, has_subgroup, page_project_collaboration_group},

            {page_nieuwestadmaken, hascollabmember, person_member},
        	{page_nieuwestadmaken, hascollabmanager, person_dorien},
        	{page_nieuwestadmaken, hascollabmember, person_dirk},
        	{page_nieuwestadmaken, hascollabmember, person_ruben_lie},
        	{page_nieuwestadmaken, hascollabmember, person_frederike},

        	{page_project_collaboration_group, hascollabmanager, person_frederike},
        	{page_project_collaboration_group, hascollabmember, person_dorien},
        	{page_project_collaboration_group, hascollabmember, person_krista},

            {person_member, hasusergroup, acl_user_group_members},

            {person_community_librarian, hasregion, region_tilburg},
            {person_community_librarian, hasusergroup, acl_user_group_community_librarian},

            {person_dorien, hasregion, region_tilburg},
            {project_thermostaat, hascollabmember, person_dorien},
            {person_dorien, hasusergroup, acl_user_group_community_librarian},

            {person_krista, hasusergroup, acl_user_group_community_librarian},
            {person_dirk, hasusergroup, acl_user_group_project_manager},

            {project_thermostaat, hascollabmanager, person_dirk},
            {project_thermostaat, hasprojectinitiator, organisation_vpro},
            {project_thermostaat, hasprojectinitiator, organisation_de_waag}
        ] ++ element(2, ExampleEvent) ++ element(2, ExampleDaycrowdEvent)
    }.

get_prod_data() ->
    #datamodel{
    	categories=[
            {reference, text, [
                {title, {trans, [
                    {nl, <<"Bron">>},
                    {en, <<"Source">>}
                ]}},
                {language, [nl,en]}
            ]},
            {contribution, text, [
                {title, {trans, [
                    {nl, <<"Bijdrage">>},
                    {en, <<"Contribution">>}
                ]}},
                {language, [nl,en]}
            ]},
            {task, contribution, [
                {title, {trans, [
                    {nl, <<"Taak">>},
                    {en, <<"Task">>}
                ]}},
                {language, [nl,en]}
            ]},
            {anonymous_participant, person, [
                {title, {trans, [
                    {nl, <<"Anonieme deelnemer">>},
                    {en, <<"Anonymous participant">>}
                ]}},
                {language, [nl, en]}
            ]},
            {remark, text, [
                {title, {trans, [
                    {nl, <<"Opmerking">>},
                    {en, <<"Comment">>}
                ]}},
                {language, [nl,en]}
            ]},
            {frequently_asked_question, text, [
                {title, {trans, [
                    {nl, <<"FAQ">>},
                    {en, <<"FAQ">>}
                ]}},
                {language, [nl,en]},
                {is_feature_show_address, false}
            ]},
            {region, location, [
                {title, {trans, [
                    {nl, <<"Regio">>},
                    {en, <<"Region">>}
                ]}},
                {language, [nl,en]}
            ]},
            {library_keyword, keyword, [
                {title, {trans, [
                    {nl, <<"Bibliotheektrefwoord">>},
                    {en, <<"Library keyword">>}
                ]}},
                {language, [nl, en]}
            ]},
            {dcat_dataset, query, [
                {title, {trans, [
                    {nl, <<"DCAT dataset">>},
                    {en, <<"DCAT dataset">>}
                ]}},
                {language, [nl,en]}
            ]},
            {roadmap, undefined, [
                {title, {trans, [
                    {nl, <<"Stappenplan">>},
                    {en, <<"Roadmap">>}
                ]}},
                {language, [nl,en]},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {roadmap_step, undefined, [
                {title, {trans, [
                    {nl, <<"Stap">>},
                    {en, <<"Step">>}
                ]}},
                {language, [nl,en]},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_option, undefined, [
                {title, {trans, [
                    {nl, <<"Stap optie">>},
                    {en, <<"Step option">>}
                ]}},
                {language, [nl,en]},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {organisation, undefined, [
                {title, {trans, [
                    {nl, <<"Organisatie">>},
                    {en, <<"Organisation">>}
                ]}},
                {language, [nl,en]}
            ]},
            {daycrowdevent, event, [
                {title, {trans, [
                    {nl, <<"Dagcrowd-evenement">>},
                    {en, <<"Day Crowd event">>}
                ]}},
                {language, [nl,en]}
            ]}
        ],
        resources = [
            % Homepage
            {page_home, collection, [
                {language, [nl,en]},
                {title, {trans, [
                    {nl, <<"Homepage">>},
                    {en, <<"Homepage">>}
                ]}},
                {page_path, <<"/">>}
            ]},

            % Menus
            {footer_menu, menu, [
                {language, [nl,en]},
                {title, {trans, [
                    {nl, <<"Footer menu">>},
                    {en, <<"Footer menu">>}
                ]}}
            ]},

            % Temporary resource for hardcoded link on project pages
            {vpro_waag_collaboration, text, [
                {title, <<"Samenwerking VPRO en de Waag">>}
            ]},
            {page_image_copyright, text, [
                {title, <<"Over beeldrechten">>}
            ]},
            {acl_user_group_community_librarian, acl_user_group, [
                {title, {trans, [
                    {nl, <<"Community librarian">>},
                    {en, <<"Community librarian">>}
                ]}},
                {language, [nl,en]}
            ]},
            {acl_user_group_project_manager, acl_user_group, [
                {title, {trans, [
                    {nl, <<"Project manager">>},
                    {en, <<"Project manager">>}
                ]}},
                {language, [nl,en]}
            ]},
            {cg_deleted_persons, content_group, [
                {title, {trans, [
                    {nl, <<"Verwijderde profielen">>},
                    {en, <<"Deleted profiles">>}
                ]}},
                {language, [nl,en]}
            ]},
            {collection_expert_predicates, collection, [
                {title, <<"Expert/specialisatie-predicaten">>},
                {content_group_id, system_content_group}
            ]},

            % Regions
            {region_none, region, [
                {title, <<"Geen regio">>},
                {language, [nl]}
            ]},
            {region_utrecht, region, [
                {title, <<"Utrecht en omstreken">>},
                {address_city, <<"Utrecht">>},
                {language, [nl]}
            ]},
            {region_geldrop, region, [
                {title, <<"Dommeldal en omstreken">>},
                {address_city, <<"Geldrop">>},
                {language, [nl]}
            ]},
            {region_noordoost_brabant, region, [
                {title, <<"Noordoost-Brabant">>},
                {address_city, <<"Noordoost-Brabant">>},
                {language, [nl]}
            ]},
            {region_dordrecht, region, [
                {title, <<"Dordrecht en omstreken">>},
                {address_city, <<"Dordrecht">>},
                {language, [nl]}
            ]},
            {region_tilburg, region, [
                {title, <<"Tilburg en omstreken">>},
                {address_city, <<"Tilburg">>},
                {language, [nl]}
            ]},

            {dataset_regions, dcat_dataset, [
                {title, {trans, [
                    {en, <<"Regions DCAT dataset">>},
                    {nl, <<"Regio's DCAT dataset">>}
                ]}},
                {query, "cat=region"}
            ]},
            {dataset_knowledge_groups, dcat_dataset, [
                {title, {trans, [
                    {en, <<"Knowledge groups DCAT dataset">>},
                    {nl, <<"Kennisgroepen DCAT dataset">>}
                ]}},
                {query, "cat=acl_collaboration_group"}
            ]},
            {dataset_events, dcat_dataset, [
                {title, {trans, [
                    {en, <<"Meetups DCAT dataset">>},
                    {nl, <<"Meetups DCAT dataset">>}
                ]}},
                {query, "cat=event"}
            ]},
            {dataset_references, dcat_dataset, [
                {title, {trans, [
                    {en, <<"References DCAT dataset">>},
                    {nl, <<"Bronnen DCAT dataset">>}
                ]}},
                {query, "cat=reference"}
            ]},
            {dataset_contributions, dcat_dataset, [
                {title, {trans, [
                    {en, <<"Contributions DCAT dataset">>},
                    {nl, <<"Bijdragen DCAT dataset">>}
                ]}},
                {query, "cat=contribution"}
            ]},
            {dataset_remarks, dcat_dataset, [
                {title, {trans, [
                    {en, <<"Remarks DCAT dataset">>},
                    {nl, <<"Opmerkingen DCAT dataset">>}
                ]}},
                {query, "cat=remark"}
            ]},

            %% Kenniscloud Methodiek
            {cg_methodology, content_group, [
                {title, <<"Methodiek CG">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {home_methodology, collection, [
                {title, <<"Kenniscloud methodiek">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {page_methodology, text, [
                {title, <<"Methodiek">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {page_methodology_kenniscloud_nl, text, [
                {title, <<"Kenniscloud.nl">>},
                {content_group_id, cg_methodology},
                {website, <<"/">>},
                {is_website_redirect, true},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {page_methodology_working_forms, text, [
                {title, <<"Werkvormen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {page_methodology_tips, text, [
                {title, <<"Tips">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {page_methodology_glossary, text, [
                {title, <<"Begrippenlijst">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {page_methodology_contact, text, [
                {title, <<"Contact">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap group
            {roadmap_group, roadmap, [
                {title, <<"Stappenplan vanuit een groep">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            %% Roadmap need
            {roadmap_need, roadmap, [
                {title, <<"Stappenplan vanuit een behoefte">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            %% Roadmap steps
            {step_preliminary_interview, roadmap_step, [
                {title, <<"Voorgesprek: wat hebben ze nodig?">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_networking, roadmap_step, [
                {title, <<"Netwerken!">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_check_need, roadmap_step, [
                {title, <<"Behoefte checken en netwerken">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_search_partners_meeting, roadmap_step, [
                {title, <<"Samenwerkingspartners zoeken voor fysieke bijeenkomst">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_form_group, roadmap_step, [
                {title, <<"Groep vormen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_organise_meetings, roadmap_step, [
                {title, <<"Bijeenkomst(en) organiseren">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_communicating, roadmap_step, [
                {title, <<"Communiceren">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_meeting, roadmap_step, [
                {title, <<"Bijeenkomst">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_disclose, roadmap_step, [
                {title, <<"Ontsluiten">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_make_accessible, roadmap_step, [
                {title, <<"Toegankelijk maken">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_record_and_share_process, roadmap_step, [
                {title, <<"Proces vastleggen en delen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {step_cycle_on_or_offline, roadmap_step, [
                {title, <<"Cyclus on/offline">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Preliminary interview
            {option_meeting_location, step_option, [
                {title, <<"Locatie voor bijeenkomst">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_guidance_proces, step_option, [
                {title, <<"Begeleiding proces">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_substantive_guidance, step_option, [
                {title, <<"Inhoudelijke begeleiding">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_network, step_option, [
                {title, <<"Netwerk">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_thinker, step_option, [
                {title, <<"Meedenker">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_collaboration, step_option, [
                {title, <<"Samenwerking">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_online_collaboration, step_option, [
                {title, <<"Online platform om op samen te werken">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Networking
            {option_own_network, step_option, [
                {title, <<"Eigen netwerk aanboren">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_network_of_partner, step_option, [
                {title, <<"Netwerk van partner aanboren">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_network_of_kenniscloud, step_option, [
                {title, <<"Netwerk van KennisCloud aanboren">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_new_network, step_option, [
                {title, <<"Nieuw netwerk aanboren">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Check need
            {option_question_own_network, step_option, [
                {title, <<"Eigen netwerk bevragen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_ask_around, step_option, [
                {title, <<"Rondvragen bij organisaties / sleutelfiguren in de stad">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_networking_on_kenniscloud, step_option, [
                {title, <<"Netwerken op KennisCloud">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Organise meetings
            {option_set_goals, step_option, [
                {title, <<"Doelen bepalen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_working_forms, step_option, [
                {title, <<"Werkvormen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_speakers, step_option, [
                {title, <<"Sprekers">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_atmosphere_location, step_option, [
                {title, <<"Sfeer / locatie">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_link_collection, step_option, [
                {title, <<"Koppeling collectie">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Communicating
            {option_mobilise_networks_or_partners, step_option, [
                {title, <<"Netwerken / partners mobiliseren">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_specifically_invite_people, step_option, [
                {title, <<"Gericht mensen uitnodigen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_distribute_posters, step_option, [
                {title, <<"Posters / flyers verspreiden">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_social_media, step_option, [
                {title, <<"Social media">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_kenniscloud, step_option, [
                {title, <<"KennisCloud">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Meeting
            {option_capture_with_film_or_video, step_option, [
                {title, <<"Vastleggen met film / video of...">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Disclose
            {option_disclose_knowledge_gained, step_option, [
                {title, <<"Ontsluiten opgedane kennis / uitkomsten delen">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_capture, step_option, [
                {title, <<"Vastleggen (filmpjes, verslag, foto's)">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_newsletter, step_option, [
                {title, <<"Nieuwsbrief">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Make accessible
            {option_through_kenniscloud, step_option, [
                {title, <<"Op KennisCloud">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_website_own_library, step_option, [
                {title, <<"Op website eigen bibliotheek">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_through_social_media, step_option, [
                {title, <<"Via social media">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Record and share process
            {option_knowledge_group_community_librarians, step_option, [
                {title, <<"In kennisgroep Community Librarians">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},
            {option_with_colleagues, step_option, [
                {title, <<"Met collega's">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Roadmap steps - Cycle on / offline
            {option_followup_actions, step_option, [
                {title, <<"Vervolgacties">>},
                {content_group_id, cg_methodology},
                {is_unfindable, true},
                {seo_noindex, true}
            ]},

            %% Avatar fallback image
            {custom_avatar_fallback, image, [
                {title, "Avatar fallback image"}
            ]}
        ],
        media=[
                {fallback_avatar, "fallback_avatar.png", #{
                    <<"title">> => <<"Fallback Avatar">>
                }}
%             {default_share_image, schema:file("default-share-image.jpg", Context), [
%                 {title, "Default Share Image"},
%                 {is_unfindable, true},
%                 {seo_noindex, true}
%             ]}
        ],
        edges = [
            {collection_expert_predicates, haspart, hasexpertjournalist},
            {collection_expert_predicates, haspart, hasexpertscientist},

            {page_home, haspart, page_nieuwestadmaken},
            {page_home, haspart, page_anderekennisgroep},

            {home_methodology, haspart, page_methodology},
            {home_methodology, haspart, page_methodology},
            {home_methodology, haspart, page_methodology_kenniscloud_nl},
            {home_methodology, haspart, page_methodology_working_forms},
            {home_methodology, haspart, page_methodology_tips},
            {home_methodology, haspart, page_methodology_glossary},
            {home_methodology, haspart, page_methodology_contact},

            %% Roadmap group
            {roadmap_group, hasstep, step_preliminary_interview},
            {roadmap_group, hasstep, step_networking},
            {roadmap_group, hasstep, step_organise_meetings},
            {roadmap_group, hasstep, step_communicating},
            {roadmap_group, hasstep, step_meeting},
            {roadmap_group, hasstep, step_disclose},
            {roadmap_group, hasstep, step_make_accessible},
            {roadmap_group, hasstep, step_record_and_share_process},
            {roadmap_group, hasstep, step_cycle_on_or_offline},

            %% Reset Order
            {roadmap_group, resetorder, step_preliminary_interview},
            {roadmap_group, resetorder, step_networking},
            {roadmap_group, resetorder, step_organise_meetings},
            {roadmap_group, resetorder, step_communicating},
            {roadmap_group, resetorder, step_meeting},
            {roadmap_group, resetorder, step_disclose},
            {roadmap_group, resetorder, step_make_accessible},
            {roadmap_group, resetorder, step_record_and_share_process},
            {roadmap_group, resetorder, step_cycle_on_or_offline},

            %% Roadmap need
            {roadmap_need, hasstep, step_check_need},
            {roadmap_need, hasstep, step_search_partners_meeting},
            {roadmap_need, hasstep, step_form_group},
            {roadmap_need, hasstep, step_organise_meetings},
            {roadmap_need, hasstep, step_communicating},
            {roadmap_need, hasstep, step_meeting},
            {roadmap_need, hasstep, step_disclose},
            {roadmap_need, hasstep, step_make_accessible},
            {roadmap_need, hasstep, step_record_and_share_process},
            {roadmap_need, hasstep, step_cycle_on_or_offline},

            %% Reset Order
            {roadmap_need, resetorder, step_check_need},
            {roadmap_need, resetorder, step_search_partners_meeting},
            {roadmap_need, resetorder, step_form_group},
            {roadmap_need, resetorder, step_organise_meetings},
            {roadmap_need, resetorder, step_communicating},
            {roadmap_need, resetorder, step_meeting},
            {roadmap_need, resetorder, step_disclose},
            {roadmap_need, resetorder, step_make_accessible},
            {roadmap_need, resetorder, step_record_and_share_process},
            {roadmap_need, resetorder, step_cycle_on_or_offline},

            %% Roadmap steps - Preliminary interview
            {step_preliminary_interview, hasoption, option_meeting_location},
            {step_preliminary_interview, hasoption, option_guidance_proces},
            {step_preliminary_interview, hasoption, option_substantive_guidance},
            {step_preliminary_interview, hasoption, option_network},
            {step_preliminary_interview, hasoption, option_thinker},
            {step_preliminary_interview, hasoption, option_collaboration},
            {step_preliminary_interview, hasoption, option_online_collaboration},

            %% Roadmap steps - Networking
            {step_networking, hasoption, option_own_network},
            {step_networking, hasoption, option_network_of_partner},
            {step_networking, hasoption, option_network_of_kenniscloud},
            {step_networking, hasoption, option_new_network},

            %% Roadmap steps - Check need
            {step_check_need, hasoption, option_question_own_network},
            {step_check_need, hasoption, option_ask_around},
            {step_check_need, hasoption, option_networking_on_kenniscloud},

            %% Roadmap steps - Organise meetings
            {step_organise_meetings, hasoption, option_set_goals},
            {step_organise_meetings, hasoption, option_working_forms},
            {step_organise_meetings, hasoption, option_speakers},
            {step_organise_meetings, hasoption, option_atmosphere_location},
            {step_organise_meetings, hasoption, option_link_collection},

            %% Roadmap steps - Communicating
            {step_communicating, hasoption, option_mobilise_networks_or_partners},
            {step_communicating, hasoption, option_specifically_invite_people},
            {step_communicating, hasoption, option_distribute_posters},
            {step_communicating, hasoption, option_social_media},
            {step_communicating, hasoption, option_kenniscloud},

            %% Roadmap steps - Meeting
            {step_meeting, hasoption, option_capture_with_film_or_video},

            %% Roadmap steps - Disclose
            {step_disclose, hasoption, option_disclose_knowledge_gained},
            {step_disclose, hasoption, option_capture},
            {step_disclose, hasoption, option_newsletter},

            %% Roadmap steps - Make accessible
            {step_make_accessible, hasoption, option_through_kenniscloud},
            {step_make_accessible, hasoption, option_website_own_library},
            {step_make_accessible, hasoption, option_through_social_media},

            %% Roadmap steps - Record and share process
            {step_record_and_share_process, hasoption, option_knowledge_group_community_librarians},
            {step_record_and_share_process, hasoption, option_with_colleagues},

            %% Roadmap steps - Cycle on / offline
            {step_cycle_on_or_offline, hasoption, option_followup_actions},

            %% Custom avatar fallback
            {custom_avatar_fallback, depiction, fallback_avatar}

        ],
        predicates=[
            {like, [
                    {title, {trans, [{nl, <<"Waardering">>},
                                     {en, <<"Appreciation">>}]}},
                    {language, [en,nl]}
                ], [
                    {person, person},
                    {person, article},
                    {person, event},
                    {person, reference},
                    {person, contribution},
                    {person, remark}
            ]},
            {flag, [
                    {title, {trans, [{nl, <<"Ongepastmelding">>},
                                     {en, <<"Flag">>}]}},
                    {language, [en,nl]}
                ], [
                    {person, person},
                    {person, article},
                    {person, event},
                    {person, reference},
                    {person, contribution},
                    {person, remark}
            ]},
            {mention, [
                    {title, {trans, [{nl, <<"Vermelding">>},
                                     {en, <<"Mention">>}]}},
                    {language, [en,nl]}
                ], [
                    {remark, person}
            ]},
            {hasregion, [
                    {title, {trans, [{nl, <<"Regio">>},
                                     {en, <<"Region">>}]}},
                    {language, [en,nl]}
                ], [
                    {event, region},
                    {acl_collaboration_group, region},
                    {person, region}
            ]},
            {hasextra_faq, [
                    {title, {trans, [{nl, <<"Extra: FAQs">>},
                                     {en, <<"Extra: FAQs">>}]}},
                    {language, [en,nl]}
                ], [
                    {acl_collaboration_group, frequently_asked_question}
            ]},
            {hasextra_rsc, [
                    {title, {trans, [{nl, <<"Extra: Resources">>},
                                     {en, <<"Extra: Resources">>}]}},
                    {language, [en,nl]}
                ], [
                    {acl_collaboration_group, reference}
            ]},
            {hasextra_doc, [
                    {title, {trans, [{nl, <<"Extra: Documenten">>},
                                     {en, <<"Extra: Documents">>}]}},
                    {language, [en,nl]}
                ], [
                    {acl_collaboration_group, document}
            ]},
            {hasextra_img, [
                    {title, {trans, [{nl, <<"Extra: Afbeeldingen">>},
                                     {en, <<"Extra: Images">>}]}},
                    {language, [en,nl]}
                ], [
                    {acl_collaboration_group, image}
            ]},
            {has_subgroup, [
                    {title, {trans, [{nl, <<"Subgroep">>},
                                     {en, <<"Subgroup">>}]}},
                    {language, [en,nl]}
                ], [
                    {acl_collaboration_group, acl_collaboration_group}
            ]},
            % Different from hasinitiator because objects are organisations, not people
            {hasprojectinitiator, [
                    {title, {trans, [{nl, <<"Initiatiefnemer">>},
                                     {en, <<"Initiator">>}]}},
                    {language, [en,nl]}
                ], [
                    {acl_collaboration_group, organisation}
            ]},
            {hasreference, [
                    {title, {trans, [{nl, <<"Heeft bron">>},
                                     {en, <<"Has source">>}]}},
                    {language, [en,nl]},
                    {uri, <<"http://purl.org/dc/terms/references">>}
                ], [
                    {contribution, reference}
            ]},
            {relation, [
                    {title, {trans, [{nl, <<"Is gerelateerd aan">>},
                                     {en, <<"Is related to">>}]}},
                    {language, [en,nl]},
                    {uri, <<"http://purl.org/dc/terms/relation">>}
                ], [
                    {contribution, reference}
            ]},
            {hasinitiator, [
                    {title, <<"Initiator (kennisgroepbeheerder)">>}
                ], [
                    {acl_collaboration_group, person}
            ]},
            {dbpedia_suggestion,
                [
                    {title, {trans, [
                        {nl, <<"Suggestie van DBpedia">>},
                        {en, <<"DBpedia suggestion">>}
                    ]}}
                ],
                [
%                    {contribution, rdf}
                ]
            },
            {hasstep, [
                {title, <<"Stap">>}
            ], [
                {roadmap, roadmap_step}
            ]},
            {resetorder, [
                {title, <<"Reset Volgorde">>}
            ], [
                {roadmap, roadmap_step}
            ]},
            {hasoption, [
                    {title, <<"Stap optie">>}
                ], [
                    {roadmap_step, step_option}
            ]},
            {hasexpertjournalist,
                [
                    {title, {trans, [
                        {nl, <<"Journalist (expert/specialist)">>}
                    ]}}
                ],
                [
                    {acl_collaboration_group, person}
                ]
            },
            {hasexpertscientist,
                [
                    {title, {trans, [
                        {nl, <<"Wetenschapper (expert/specialist)">>}
                    ]}}
                ],
                [
                    {acl_collaboration_group, person}
                ]
            }
        ]
    }.

update_usergroup_hierarchy(Context) ->
    R = fun(N) -> m_rsc:rid(N, Context) end,
    NewTree = [
        {R(acl_user_group_anonymous), [
            {R(acl_user_group_members), [
                {R(acl_user_group_project_manager), []},
                {R(acl_user_group_community_librarian), [
                    {R(acl_user_group_editors), [
                        {R(acl_user_group_managers), []}
                    ]}
                ]}
            ]}
        ]}
    ],
    m_hierarchy:save(acl_user_group, NewTree, Context).

install_acl_rules(Context) ->
    m_acl_rule:replace_managed(kenniscloud_acl:rules(), kenniscloud, Context).

-spec install_main_menu(#context{}) -> ok.
install_main_menu(Context) ->
    mod_menu:set_menu(
        main_menu,
        [
            {page_kennisgroepen, []},
            {page_subjects, []},
            {page_regions, []},
            {page_agenda, []},
            {page_community, []}
        ],
        Context
    ).

-spec install_footer_menu(#context{}) -> ok.
install_footer_menu(Context) ->
    mod_menu:set_menu(
        footer_menu,
        [
            {page_about, []},
            {page_contact, []},
            {signup_privacy, []}
        ],
        Context
    ).

ensure_title(Category, Title, Context) ->
    {ok, _} = m_rsc:update(Category, [{title, Title}], z_acl:sudo(Context)).

%% Autocreation of meetup crowd
person_names() ->
    ["Skye", "Ciara", "Diamond", "Kendall", "Annalise", "Kaitlynn",
     "Dahlia", "Adelaide", "Larissa", "Kristen", "Makena", "Belinda",
     "Rowan", "Kassidy", "Abbey", "Luna", "Kaitlin", "Lesly",
     "Janessa", "Evelyn", "Marlie", "Sophia", "Kennedy", "Marley",
     "Natasha", "Bianca", "Shyla", "Callie", "Michaela", "Naima",
     "Shiloh", "Ellie", "Hanna", "Jakayla", "Kathryn", "Erika",
     "Mariana", "June", "Tania", "Mallory", "Zaria", "Jaliyah",
     "Jaslyn", "Shaylee", "Ainsley", "Sadie", "Akira", "Macie",
     "Denisse", "Livia", "Johan", "Jorge", "Alvaro", "Kadyn",
     "Ezequiel", "Kayden", "Gilberto", "Case", "Zaire", "Talon",
     "Broderick", "Theodore", "Damari", "Angel", "Devin", "Maximo",
     "Moshe", "Rogelio", "Josiah", "Ronald", "Aaden", "Talan",
     "Mason", "Jaidyn", "Lewis", "Davin", "Jeffrey", "Gideon",
     "Ernesto", "Donavan", "Jonathan", "Van", "Reuben", "Derrick",
     "Boston", "Leon", "Jerry", "Cruz", "Jagger", "Niko",
     "Dominique", "Alonso", "Braden", "Rohan", "Jared",
     "Gordon", "Nikolai", "Malcolm", "Julien", "Jadiel"].

tag_names() ->
    ["cocoa", "shells", "fencing", "snakes", "buffaloes", "lions",
     "gold", "horses", "mining", "North Africa", "Salvation Army",
     "vegetables", "goats", "shoes", "birds", "army",
     "Disney movies", "vaccines", "lighters", "cobblers", "dogs",
     "pipe organs", "movie theaters", "weaving", "chess", "socialism",
     "salt", "soccer", "golf", "cactus", "water skiing", "paper boys",
     "frogs", "radio", "sheep", "basketball", "muppets", "bears",
     "Boy Scouts", "railroads", "sports", "barbers", "jewelry",
     "crime", "alligators", "tattoos", "potatoes", "football",
     "windmills", "bakeries"].

make_person(Name) ->
    UniqueName = "person_" ++ Name,
    {list_to_atom(UniqueName),
     person,
     [{title, Name},
      {summary, <<"I'm an autogenerated person!">>},
      {language, [nl]}
     ]}.

make_keyword(Name) ->
    UniqueName = "keyword_" ++ Name,
    {list_to_atom(UniqueName),
     keyword,
     [{title, Name},
      {language, [nl]}
     ]}.

make_event(Name, Summary) ->
    UniqueName = "event_" ++ Name,
    {list_to_atom(UniqueName),
     event,
     [{title, Name},
      {summary, Summary},
      {language, [nl]},
      {address_city, <<"Tilburg">>},
      {address_street_1, <<"Burgemeester Brokxlaan">>},
      {content_group_id, page_nieuwestadmaken}
     ]}.

make_daycrowdevent(UniqueName, Summary) ->
    {list_to_atom(UniqueName),
     daycrowdevent,
     [{title, UniqueName},
      {summary, Summary},
      {language, [nl]},
      {address_city, <<"Tilburg">>},
      {address_street_1, <<"Burgemeester Brokxlaan">>}
     ]}.

example_event() ->
    PersonCount = 20,
    KeywordCount = 10,
    KeywordsPerPerson = 3,

    EventName = "everything",
    Persons = pick_n(person_names(), PersonCount),
    Keywords = pick_n(tag_names(), KeywordCount),

    Objects = [ make_person(Name) || Name <- Persons ]
        ++ [ make_keyword(Name) || Name <- Keywords ]
        ++ [ make_event(EventName, "I'm an autogenerated event!") ],
    Edges =
        [ {event_everything,
           subject,
           list_to_atom("keyword_" ++ Keyword)}
          || Keyword <- Keywords ]
        ++ [ {list_to_atom("person_" ++ Person),
              rsvp,
              event_everything}
          || Person <- Persons ]
        ++ [ {list_to_atom("person_" ++ Person),
              subject,
              list_to_atom("keyword_" ++ Keyword)}
             || Person <- Persons,
                Keyword <- pick_n(Keywords, KeywordsPerPerson)
           ],
    {Objects, Edges}.

example_daycrowdevent() ->
    PersonCount = 0,
    KeywordCount = 10,
    KeywordsPerPerson = 3,

    EventName = "adaycrowd",
    Persons = pick_n(person_names(), PersonCount),
    Keywords = pick_n(tag_names(), KeywordCount),

    Objects = [ make_person(Name) || Name <- Persons ]
        ++ [ make_keyword(Name) || Name <- Keywords ]
        ++ [ make_daycrowdevent(EventName, "I'm an autogenerated daycrowd event!") ],
    Edges =
        [ {person_community_librarian, rsvp, adaycrowd}
        ]
        ++ [ {adaycrowd,
           subject,
           list_to_atom("keyword_" ++ Keyword)}
          || Keyword <- Keywords ]
        ++ [ {list_to_atom("person_" ++ Person),
              rsvp,
              adaycrowd}
          || Person <- Persons ]
        ++ [ {list_to_atom("person_" ++ Person),
              subject,
              list_to_atom("keyword_" ++ Keyword)}
             || Person <- Persons,
                Keyword <- pick_n(Keywords, KeywordsPerPerson)
           ],
    {Objects, Edges}.

pick_n(_, 0) -> [];
pick_n(List, N) ->
   H = choice(List),
   [H] ++ pick_n(lists:delete(H, List), N - 1).

choice(List) ->
    lists:nth(rand:uniform(length(List)), List).

setup_dev_users(Context) ->
    create_identity_if_not_exists(person_community_librarian, "Conny Community Librarian", "123123", [], Context),
    create_identity_if_not_exists(person_member, "Leo Lid", "123123", [], Context).

%% From mod_ginger_base/support/schema.erl: Set username and password if not set before
%% TODO: Move to zotonic_mod_driebit_base?
-spec create_identity_if_not_exists(atom(), string(), string(), list(tuple()), #context{}) -> ok.
create_identity_if_not_exists(Name, Username, Password, Props, Context) ->
    Resource = m_rsc:rid(Name, Context),
    case m_identity:is_user(Resource, Context) of
        false ->
            %% Create new credentials
            case m_identity:lookup_by_username(Username, Context) of
                undefined ->
                    {ok, _IdentityId} = m_identity:insert_unique(Resource, username_pw, Username, Props, z_acl:sudo(Context)),
                    ok = m_identity:set_username_pw(Resource, Username, Password, z_acl:sudo(Context));
                _ ->
                    %% Another user already exists with the username, so do nothing
                    ok
            end;
        true ->
            %% The user already has credentials, so don't change them
            ok
    end.

setup_upload_rights(Context) ->
    % Limit the upload size and allowed file types for registered users:
    m_rsc:update(
        acl_user_group_members,
        #{
            <<"acl_upload_size">> => 10, % MB
            <<"acl_mime_allowed">> => <<".pdf, image/*">>
        },
        Context
    ).
