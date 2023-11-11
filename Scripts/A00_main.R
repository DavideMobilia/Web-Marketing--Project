#### OPTIONS ####

options(scipen=999)
set.seed(123456)

#### LIBRARIES ####
library(dplyr)
library(magrittr)
library(ggplot2)
library(forcats)
library(lubridate)
library(RQuantLib) 
library(pastecs)
library(kernlab) 
library(caret) 
library(tidyr) 
library(plotly) 
library(randomForest) 
library(pROC) 
library(funModeling) 
library(rpart) 
library(rpart.plot) 
library(ROSE) 
library(arulesViz)
library(rattle)
library(arules)


#### DIRECTORIES ####
working_dir = "/Users/martalvi/Desktop/Bicocca/1°\ anno/DgMarketing/Progetto/Script"
data_dir = "/Users/martalvi/Desktop/Bicocca/1°\ anno/DgMarketing/Progetto/Data"

setwd(working_dir)

#### EXECUTION FULL PIPELINE ####
 PIPELINE_scripts <- c(
   'B01_ingestion.R'
   , 'C01_preparation_df1.R'
   , 'C02_preparation_df2.R'
   , 'C03_preparation_df3.R'
   , 'C04_preparation_df4.R'
   , 'C05_preparation_df5.R'
   , 'C06_preparation_df6.R'
   , 'C07_preparation_df7.R'
   , 'D01_RFM.R'
   , 'D02_churn.R'
   , 'D03_Basket_Analysis.R'
   )
 
 for(i in PIPELINE_scripts){
   source(i, echo = TRUE)
 }