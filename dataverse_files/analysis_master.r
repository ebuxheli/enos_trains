###analysis_master.r
###master train analysis script
###RdE October 2012

rm(list = ls())

###THE PACKAGES BELOW MAY NEED TO BE INSTALLED USING install.packages('x'), WHERE X IS THE PACKAGE NAME
library(ri)
library(RItools)
library(car)
library(xtable)
library(effects)
library(RColorBrewer)
library(readr)
############################

options(scipen = 999)  ##set for non-scientific notation output

##Load data
dat.all = read.csv('dataverse_files/pnas_data.csv')
dat.t1 = read.csv('dataverse_files/t1_data.csv')
dat.all.prime = read.csv('dataverse_files/prime_data.csv')
###data loading for faces graphic
conf.dat = read.csv('dataverse_files/confederate_face_data.csv')
hisp.dat = read.csv('dataverse_files/hispanic_face_data.csv')
white.dat = read.csv('dataverse_files/white_face_data.csv')


##the following scripts will be executed
source('dataverse_files/balance_check.r') ##executes balance tests
source('dataverse_files/main_results.r')  ##produces ATE and car subanalysis
source('dataverse_files/subset_inference.r')  ##produces dose effect and subgroup effects in supporting material
source('dataverse_files/primetest.r') ##produces prime test in supporting material
source('dataverse_files/output_create.r')  ##creates tables
source('dataverse_files/outgraphic_single_pnas.r') ##produces graphics, expect face graphic
source('dataverse_files/face_summary_pnas.r') ##creates face graphic