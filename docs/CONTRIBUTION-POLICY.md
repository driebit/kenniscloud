# Contribution Policy

## **1. Introduction**

### **Purpose of the Document**

This document outlines the contribution policy for the KennisCloud platform, providing clear guidelines on how individuals and organisations can participate in the platform. It ensures a structured, inclusive, and effective contribution process, fostering collaboration and knowledge-sharing within the community.

### **Overview of the Project**

KennisCloud is a platform that connects individuals, communities, and organisations to share and create knowledge. It facilitates both digital collaboration and in-person discussions through structured knowledge workshops. The platform aims to make reliable information more accessible and to encourage the exchange of expertise across various fields.

### **Encouragement for Contributions**

Contributions are essential to the success of KennisCloud. Whether through submitting ideas, sharing expertise, participating in discussions, or collaborating on projects, every contribution enriches the platform. Contributors are encouraged to actively engage, ask questions, and support the community in building a strong, knowledge-driven network.

## **2. Code of Conduct**

### **Expected Behaviour**

-   **Respectful Communication**: Contributors are expected to engage in conversations that are professional, respectful, and constructive. This includes providing clear, thoughtful feedback and avoiding inflammatory language.
-   **Collaboration**: Open and cooperative work is encouraged. Members should seek to collaborate, help others, and work towards the project's collective success.
-   **Support and Assistance**: Actively helping others, answering questions, and offering assistance where possible fosters a friendly and helpful environment.
-   **Inclusivity**: The environment should be welcoming and inclusive, where everyone feels comfortable contributing regardless of background, skill level, or perspective.

### **Anti-Harassment Policies**

-   **Zero Tolerance for Harassment**: Harassment of any kind, including but not limited to discriminatory comments, threats, personal attacks, and inappropriate jokes, is strictly prohibited. This includes harassment based on gender, race, sexual orientation, disability, religion, or any other identity factor.
-   **Commitment to a Safe Environment**: The open-source community must be a safe space for everyone, ensuring that no one feels marginalised or excluded.

### **Consequences of Violations**

The consequences of violating any community standards and regulations will be enforced by the governing party and should be promoted as well as enforced by the community where possible.

-   **Warnings**: First-time minor violations may result in a warning, explaining the expected behaviour and how the individual can correct their actions.
-   **Temporary Suspension**: In cases of more severe violations or repeated warnings, an individual may be temporarily suspended from the platform or from contributing to the project.
-   **Permanent Ban**: In extreme cases or if violations continue, a permanent ban from the project or its community may be imposed.

## **3. How to Contribute**

![Ontwikkeling - huidig proces-2024-08-30-135927.png](https://prod-files-secure.s3.us-west-2.amazonaws.com/cd2cd2ac-ea4e-45e7-8d6d-b15df0193002/74bb9ee0-95a8-4706-88df-d21ed15d0d9a/Ontwikkeling_-_huidig_proces-2024-08-30-135927.png)

-   **Bug reports and Feature requests**
    -   Bugs, feature requests as well as ideas and suggestions can be reported in the issue tracking functionality of the software repository platform, currently this will be Github Issues (or any projects we can setup there). Service tickets that Driebit needs to pick up directly will still be reported via JIRA in the current way of working.
    -   If Driebit finds issues on GitHub that are similar to any of the JIRA tickets we will create a link to keep track of the ongoing work. However it is also the responsibility of the Kenniscloud core team to also have context on what is being developed and/or reported (since they will have access to both JIRA and GitHub issues).
    -   Arbitrage of issues is done by the maintainers.
-   **Submitting code (Pull requests, Forking, Branching)**
    -   Pull requests:
        -   Contributors are able to open pull requests to the public repository on GitHub. The pull requests will (for the time being) be handled by the maintainers. As long as the contributions meet the requirements of this document and serve the best interests of the KennisCloud platform, they will be merged and deployed through releases.
    -   Forks:
        -   Contributors are also able to fork the repository for creating diverging features or taking it in a different direction (in the example of when the contribution doesn’t fit the platform’s current direction or the maintainers’ requirements). However the use of the code will fall back to the licenses attached to said code.
    -   Branches:
        -   When branches are created from public issues on GitHub they need to be linked for the sake of documentation and accountability. In the case of any issue number or code, the appropriate issue should be referenced in the branch name (i.e. KC-123-new-feature)
    -   Commits:
        -   Commit messages need to be clear and transparent regarding the changes being done. In the case of any issue number or code, the appropriate issue should be referenced in the commit message (i.e. KC-123: Add documentation regarding new feature)
-   **Writing documentation**
    -   All new features or edits should be well documented where appropriate following suit with the current code documentations. While difficult to enforce, the maintainers will add a concise description regarding this in the README.md which can be found in the repository.
    -   A pull request template will be added to the repository with a checklist that includes an item with the text “_I’ve updated the documentation_”. The contributor will have to check it which should increase the awareness to document contributions.

## **4. Contribution Guidelines**

-   Coding standards (Style guides, Naming conventions)
    -   Pull requests based on technologies/languages already used in the project are preferred (html for templating, css/scss, elm, erlang).
    -   Substantial changes in tech stack should be justified by contributors and will be assessed by the maintainers. maintainers will reserve the decision of excluding a contribution if it is decided that it will pose difficulty for evaluation or maintenance.
    -   The maintainers will add linters where possible to alert contributors that their code does not meet the standard and/or format determined by the maintaining team.
-   Testing requirements
    -   In case of contributions that the KennisCloud platform shareholder team has commissioned (or is aware of), shareholder will be responsible for testing the features before they are deployed. The maintainers can also account for some testing, however this should be decided between maintainers and the shareholders.
    -   In the future manual testing can be partly replaced by automated testing.

## **5. Licensing and Copyright**

-   License under which contributions are accepted
    -   Contributors may add their own copyright statement to their modifications and may provide additional or different license terms and conditions for use, reproduction, or distribution of their modifications, or for any such derivative works as a whole, provided Their use, reproduction, and distribution of the work otherwise complies with the conditions stated in this License.
    -   Appropriate licenses can be found as added by authors of the code in the designated files within the repository.

## **6. Communication Channels**

-   The maintainers will have direct contact with the contributors on their contribution branches on GitHub.
