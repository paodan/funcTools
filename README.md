funcTools package contains miscellaneous functions or tools to make my work easier.

# Installation
if(!require("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("paodan/funcTools")

# Function Example:

`funCode` can show me the source R code of other functions including those S3 and S4 methods of a generic function.

`runC` can make it easier to run functions from C code with R.

Put `setwd2thisFile()` in a file and run this line in RStudio can set your working directory to 
where this file is. 

`rQsub2` can help submit Slurm jobs with R scripts.

`createProject` can initialize a structured directory for a project.

`loadAsList` can load `.Rdata` as a list and assign it to a variable, which is handier than `load` function.

`theme_Publication` and `theme_transparent` can change a good ggplot theme ready for publication.

`fileNameFactory` can create functions to conveniently make a file name.


And check `help(package = "funcTools")` for many more.