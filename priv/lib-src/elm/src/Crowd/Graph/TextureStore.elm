module Crowd.Graph.TextureStore exposing
    ( TextureStore
    , TextureUrl
    , empty
    , get
    , load
    , loadNonPowerOfTwo
    , preloadAll
    , preloadAllNonPowerOfTwo
    , toString
    , url
    )

import Dict exposing (Dict)
import Task
import WebGL.Texture as Texture exposing (Texture)


type TextureStore
    = TextureStore (Dict String Texture)


get : TextureUrl -> TextureStore -> Maybe Texture
get textureUrl (TextureStore store) =
    Dict.get (toString textureUrl) store


empty : TextureStore
empty =
    TextureStore Dict.empty


insert : TextureUrl -> Texture -> TextureStore -> TextureStore
insert textureUrl texture (TextureStore store) =
    TextureStore (Dict.insert (toString textureUrl) texture store)


type TextureUrl
    = TextureUrl String


url : String -> TextureUrl
url =
    TextureUrl


toString : TextureUrl -> String
toString (TextureUrl urlString) =
    urlString


preloadAll : TextureStore -> List TextureUrl -> ((TextureStore -> TextureStore) -> msg) -> Cmd msg
preloadAll =
    preloadAllOptions Texture.defaultOptions


preloadAllNonPowerOfTwo : TextureStore -> List TextureUrl -> ((TextureStore -> TextureStore) -> msg) -> Cmd msg
preloadAllNonPowerOfTwo =
    preloadAllOptions Texture.nonPowerOfTwoOptions


preloadAllOptions : Texture.Options -> TextureStore -> List TextureUrl -> ((TextureStore -> TextureStore) -> msg) -> Cmd msg
preloadAllOptions options currentStore textureUrls handle =
    Cmd.batch <| List.map (\textureUrl -> loadOptions options textureUrl currentStore handle) textureUrls


load : TextureUrl -> TextureStore -> ((TextureStore -> TextureStore) -> msg) -> Cmd msg
load =
    loadOptions Texture.defaultOptions


loadNonPowerOfTwo : TextureUrl -> TextureStore -> ((TextureStore -> TextureStore) -> msg) -> Cmd msg
loadNonPowerOfTwo =
    loadOptions Texture.nonPowerOfTwoOptions


loadOptions : Texture.Options -> TextureUrl -> TextureStore -> ((TextureStore -> TextureStore) -> msg) -> Cmd msg
loadOptions options textureUrl (TextureStore currentStore) handle =
    let
        onLoad res =
            case res of
                Ok newTexture ->
                    handle (insert textureUrl newTexture)

                Err _ ->
                    handle identity
    in
    case Dict.get (toString textureUrl) currentStore of
        Nothing ->
            Task.attempt onLoad (Texture.loadWith options (toString textureUrl))

        Just _ ->
            Cmd.none
