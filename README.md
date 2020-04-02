# rapidPop: Rapid population assessments in R
This package implements Rapid Population Assessments (RPAs) using camera trap data following [Schlichting et al. (2020)](https://wildlife.onlinelibrary.wiley.com/doi/abs/10.1002/wsb.1075) in R Shiny Apps. 

# Dependencies
- If you are using a Windows computer, you might require an installation of [Rtools](https://cran.r-project.org/bin/windows/Rtools/) to install packages from github with the R package devtools. 
- This package uses the R packages unmarked, shiny, and shinyFiles, and these packages will be installed when you install rapidPop following the instructions below. 


# Install the package using:
```
# install devtools if you don't have it
if (!require('devtools')) install.packages('devtools')
# check error messages and ensure that devtools installed properly. 

# install rapidPop from github
devtools::install_github("mikeyEcology/rapidPop") 
# This line might prompt you to update some packages. It would be wise to make these updates. 

# load this package
library(rapidPop)
```

# Run the Shiny App using:
```
rapidPop::runShiny("occMod")
```
- Select your `Input file` as the `.csv` or `txt` file containing your observations 
  - Your input file should contain observations (if an animal was detected) using columns for each day of survey. There should be a row for each camera. 
  - If you want to look at the effect of a parameter on occupancy, there should also be a column containing some information. For example, if you are looking at the effect of eradication on occuapncy, you could have a column called "removal", with values "before" and "after". Then you would type "removal" for the column name of the parameter. 
  
- For more details see [this paper](https://www.biorxiv.org/content/10.1101/2020.03.30.017103v2).
- For more details on RPAs, see [Schlichting et al. (2020)](https://wildlife.onlinelibrary.wiley.com/doi/abs/10.1002/wsb.1075). 

# Reference for this package

Tabak, M. A., Lewis, J. S., Schlichting, P. E., Snow, N. P., VerCauteren, K. C., & Miller, R. S. (2020). [rapidPop: Rapid population assessments of wildlife using camera trap data in R Shiny Applications](https://www.biorxiv.org/content/10.1101/2020.03.30.017103v2). BioRxiv, 2020.03.30.017103. doi:10.1101/2020.03.30.017103


Or using BibTex:
```
@article{tabakRapidPopRapidPopulation2020,
  title = {{{rapidPop}}: {{Rapid}} Population Assessments of Wildlife Using Camera Trap Data in {{R Shiny Applications}}},
  shorttitle = {{{rapidPop}}},
  author = {Tabak, Michael A. and Lewis, Jesse S. and Schlichting, Peter E. and Snow, Nathan P. and VerCauteren, Kurt C. and Miller, Ryan S.},
  year = {2020},
  month = apr,
  pages = {2020.03.30.017103},
  publisher = {{Cold Spring Harbor Laboratory}},
  doi = {10.1101/2020.03.30.017103},
  journal = {bioRxiv},
  language = {en}
}
```
