#*********************************************
#              Loading libraries             #  
#*********************************************


library(flextable)
library(data.table)
library(plyr) # must be loaded BEFORE dplyr/tidyverse
library(gdata) # read.xls
library(zoo)
library(readxl)
library(stringr)
library(readxl)
# library(XLConnect) # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
library(magrittr)    # use %<>%   
library(scales)
# library(xlsx) # writing xlsx files
# library("btools")

# parallel computing
library(foreach)
library(doParallel)
library(microbenchmark)

# Tydiverse and other RStudio packages
library(tidyverse) # loads ggplot2, tibble, tidyr, readr, purrr, dplyr, forcats
library(knitr)

options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20

source(paste0(here::here(),"/Functions.R"))


#*********************************************
#          Colors and figure themes          #  
#*********************************************

# Colors
{
	RIG.blue  <- "#003598"
	RIG.red   <- "#A50021"
	RIG.green <- "#009900"
	RIG.yellow <- "#FFFF66"
	RIG.purple <- "#9966FF"
	RIG.yellow.dark <- "#ffc829"
	RIG.orange <- "#fc9272"
	
	demo.color6 <- c(RIG.red,
									 RIG.orange,
									 RIG.purple,
									 RIG.green ,
									 RIG.blue,
									 RIG.yellow.dark)
	
	
	RIG.theme <- function(){
		theme(panel.grid.major.x = element_line(size = 0.3, color = "gray90"), #element_blank(),
					panel.grid.minor.x = element_blank(),
					panel.grid.minor.y = element_blank(),
					panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
					plot.title=element_text(hjust=0.5),
					plot.subtitle=element_text(hjust=0.5),
					plot.caption=element_text(hjust=0, size = 9))
	}
}


#*******************************************************************************
#                                  Local Tools          #  
#*******************************************************************************


read_excel_tblInfo <- function(fileName, sheetName, Range = "a2:b4"){
  read_excel(fileName, sheet = sheetName, range = Range, col_names = c("var", "value"))
}

# read_excel_range <- function(fileName, sheetName, Range = "a2:b4" ){
#   
#   tblInfo <- read_excel_tblInfo(fileName, sheetName, Range = Range)
#   df <- read_excel(fileName, sheet = sheetName, range = tblInfo[tblInfo$var == "cell_range", "value"][[1]] )
#   
#   list(df      = df,
#        tblInfo = tblInfo)
#   
# }

read_excel_range <- function(fileName, sheetName, Range = "a2:b4" ){
  
  tblInfo <- read_excel_tblInfo(fileName, sheetName, Range = Range)
  df      <- read_excel(fileName, sheet = sheetName, range = tblInfo[tblInfo$var == "cell_range", "value"][[1]] )
  
  tblInfo <-  tblInfo$value %>% split(tblInfo$var)
  
  list(df      = df,
       tblInfo = tblInfo)
}






