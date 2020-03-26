# rapidPop

# How to use this package before it is made public
- You will need to have the [unmarked package](https://cran.r-project.org/web/packages/unmarked/unmarked.pdf), available from CRAN, installed and loaded into your R session. 
- Run all of the [code for the occMod function](https://github.com/mikeyEcology/rapidPop/blob/master/R/occMod.R). (Simply copy and paste into your R console.)
- Run all of the [code for the occMod shiny App](https://github.com/mikeyEcology/rapidPop/blob/master/R/occMod_shiny.R). A shiny app will appear to run an occupancy model. 


# Install the package using (once public):
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
