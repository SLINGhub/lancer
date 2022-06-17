# Contributing to `DCVtestkit`

First of all, thanks for considering contributing to `DCVtestkit` 😄! We hope that you have found the tool useful in your work 😀 and we apologise for any mishaps 😣 along the way.

`DCVtestkit` is an open source project, maintained by people who care.

## Acknowledgements 😌

This contributing file is based on a [template](https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c) from Peter Desmet released under CC0. [![License: CC0-1.0](https://licensebuttons.net/l/zero/1.0/80x15.png)](http://creativecommons.org/publicdomain/zero/1.0/)

## Versioning 🔢

Refer to the [NEWS.md file](https://github.com/SLINGhub/DCVtestkit/blob/main/NEWS.md) to see what is being worked on as well as update to changes between back to back versions.

Software version numbers indicate following: `MAJOR.MINOR.PATCH.DEVELOPMENT`. 

Here are key steps to keep in mind:

-   The **major** version number generally do not increase unless the changes made affect a large group. Examples are moving the software to a new repository, changes to API, etc... 

-   When new features are added or (re)moved, we typically increase the **minor** version number.

-   Minimal, non breaking changes or bug fixes only are indicated by increasing the **patch** version number. Examples of minimal changes are are updating of documentations, fixing of typo in the software output and so on

-   Current development versions of our packages (i.e. main branch from GitHub) additionally have a **development** version number. The **development** version number is typically `9000`

## Code of conduct 👩‍🏫

Please note that this project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/0/code_of_conduct/). By participating in this project you agree to abide by its terms.

## Improve the documentation 📚
Noticed a typo on the website? Think a function could use a better example? Good documentation makes all the difference, so your help to improve it is very welcome!

### Function documentation
Functions are described as comments near their code and translated to documentation using `roxygen2`. If you want to improve a function description:

1. Go to `R/` directory in the [code repository](https://github.com/SLINGhub/DCVtestkit/blob/main/R).
2. Look for the file with the name of the function.
3. [Propose a file change](https://docs.github.com/en/repositories/working-with-files/managing-files/editing-files) to update the function documentation in the roxygen comments (starting with `#'`).

## Contribute code 📝

Care to fix bugs 🐛 or implement new functionality for `DCVtestkit`? Great👏! Thank you for volunteering your time to help out. Have a look at the [issue list](https://github.com/SLINGhub/DCVtestkit/issues) and leave a comment on the things you want to work on. See also the development guidelines below.

## Development guidelines 👨‍💻

### README Documentation

For the README documentation, [Rmarkdown](https://rmarkdown.rstudio.com/) is used together with [R](https://www.r-project.org/) and [RStudio IDE](https://www.rstudio.com/products/rstudio/download/).

Here are some useful resources.
  * https://www.rstudio.com/resources/webinars/getting-started-with-r-markdown/ 
  * https://rmarkdown.rstudio.com/github_document_format.html

RStudio was used because of its friendly user interface (more button clicks than command lines) to create markdown and html document and to use git. Based on past experiences, it is easier to guide beginners to create html documents and use git using RStudio than pure command line. In addition, most people in the lab uses R. 

With the efforts made by the [R for Data Science Online Learning Community](https://www.rfordatasci.com/), they have created a learning environment via their [Slack account](http://r4ds.io/join) which make beginners more comfortable to ask question and share about R, RStudio, Rmarkdown, Git and GitHub issues. Give it a try to make your learning experience in R, Statistics, Git and GitHub a more fruitful experience that is worth sharing.

### GitHub Workflow

We try to follow the [GitHub flow](https://guides.github.com/introduction/flow/) for development.

1. Fork [this repo](https://github.com/SLINGhub/DCVtestkit) and clone it to your computer. To learn more about this process, see [this guide](https://guides.github.com/activities/forking/).
2. If you have forked and cloned the project before and it has been a while since you worked on it, [pull changes from the original repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/) to your clone by using `git pull upstream main`.
3. Open the RStudio project file (`.Rproj`).
4. Make your changes:
    * Write your code.
    * Test your code (bonus points for adding unit tests) .
    * Document your code so that others can understand.
    * Check your code with `devtools::check()` and aim for 0 errors and warnings.
5. Commit and push your changes.
6. Submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request).
