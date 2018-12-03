
workpath <- "/Users/samlin/Documents/my_R/dev_ATarget"

setwd(workpath)
getwd()
source("utils_udf_pa.R")
source("utils_udf_misc.R")
source("utils_pwr_2p2n_tol.R")
#getTestResultsGivenES(0.02, 0.04, 0.8, 0.95)

library(shiny)
library(dplyr)
library(datasets)
library(shinythemes)
library(ggplot2)

# options(shiny.port = 1234)
# options(shiny.host = "10.50.54.128")

#ip="10.50.54.128"
ip="10.50.83.73"
#ip="192.168.1.16"

runApp(appDir = getwd(), host=ip, port=1234 )
