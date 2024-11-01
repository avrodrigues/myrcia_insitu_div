
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Biogeographical history of *Myrcia* diversity

<!-- badges: start -->
<!-- badges: end -->

Here you find code and data to reproduce the analysis of the manuscript
entitled “A complex biogeographical history explains the spatial
distribution of *Myrcia* diversity in the Atlantic Forest”

## How to use the repository

Download the repository or clone it using the git command:

`git clone https://github.com/avrodrigues/myrcia_insitu_div.git`

## Setting the environment

We used the R version 4.2.2 and the [{renv}
package](https://rstudio.github.io/renv/index.html) v0.17.3 to create a
reproducible environment for the analysis.

After download the repository, you should run the code
`renv::restore()`. It will install all the packages needed with the same
version we used to produce the code and the analysis. Since it has
several packages to be installed, this should take some time to be
concluded.

## Repository structure

    #> .
    #> ├── data
    #> ├── fig
    #> ├── LICENSE
    #> ├── myrcia_insitu_div.Rproj
    #> ├── output
    #> ├── README.md
    #> ├── README.Rmd
    #> ├── renv
    #> ├── renv.lock
    #> └── script

### Main folders

In `data` you find all the data necessary to run the analysis and
compute the diversity and stability metrics.  
In `script` you find the code for the analysis and figures.  
In `output` you find the output metrics used in the study. s In `fig`
you find the figures and images used to produce the figures in the
manuscript.
