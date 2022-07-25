#' ---
#' title: "combineTables.R"
#' author: "Adapted for the SK PTM by Griffy J. Vigneron"
#' date: "22 June 2022"
#' output: github_document
#' ---

#' This script reads individual expert estimates from multiple .csv files
#' and compiles them into a single **Estimates_combined.csv** file.
#' It requires that each expert table is saved as a .csv file in a *benefits* subfolder within the *data* folder, 
#' contain the same number of rows and columns, and no other .csv files are in the same folder.

#+ warning = FALSE, message = FALSE
#+ 

library(stringi)
library(dplyr)
library(tidyverse)
library(data.table)
library(here)

setwd('D:/UBC/R_code/SKAnalysis/data/benefits')
#' Read in the individual tables of expert estimates and combine. NOTE to maintain confidentiality, only sample tables are provided
#+ warning = FALSE, message = FALSE
subfolder <- here("data", "benefits")
files <- list.files(path = paste(subfolder, "/", sep=""), # Name of the subfolder in working directory that contains the files
                    pattern = "*.csv", 
                    full.names = T)

#are all these needed? Should these be added in to the reorganizing step?
skiplines <- 16 # number of header rows to skip (first few lines contain worksheet instructions which are not needed)
nstrat <- 17 # number of management strategies (including combinations, but excluding baseline)
numcols <- (nstrat+1+2) #*5 + 1 # total number of columns to read in (5 columns for each strategy and the baseline [Best guess, Lower, Upper, Confidence, and a Quality check column], plus 1 column for group names)
ngroups <- 7 # number of ecological groups (rows)
numrows <- (ngroups+1)*3 + 1 # total number of row to read in (5 rows for each strategy and the example [Best guess, Lower, Upper, Confidence, and a Quality check column], plus 1 row for group names)
experts <- c(1:4) # vector of expert codes, should correspond to the same order as in 'files'

#temp <- read.csv("/content/Benefits_Template_datacheck_combined_strategy3.csv", skip=skiplines, header=TRUE)
temp <- read.csv(files[1], skip = skiplines, nrows = numrows)
temp <- temp %>% 
    select(0:numcols) %>%
    filter(!row_number() %in% c(1))

#ecological groups
ecodata <- pull(temp, Ecological.Groups) #create a group of ecological names
ecodata <- data_frame(as.data.frame(ecodata))

#remove blank rows from ecological groups
ecodata <- ecodata[!apply(ecodata == "", 1, all), ]

#create empty dataframe for output
byexpert <- data.frame() 
Expert <- list()
e = 0

for (i in 2:length(experts)){ # else use length(files) if all estimates are available
  temp <- read.csv(files[1], skip = skiplines, nrows = numrows)
  temp <- temp %>% 
    select(0:numcols) %>%
    filter(!row_number() %in% c(1))#shouldn't read a file in a loop? Do I need to to read each expert estimate?
  e = e+1 #counter for number of experts/sheets
  temp <- rename(temp,c('Col'='X'))
  temp <- temp[!grepl("CHECK", temp$Col),]
  temp <- temp[!grepl("CONFIDENCE", temp$Col),]
  
  
  #create data frame for loop output with row names for Ecological Groups
  df<-ecodata
  
  #reorganize estimates
  for (i in 1:nrow(ecodata)) { #for all ecological groups (including example)
    n = 2 #starts from two, so eco groups are first column
    Expert <- append(Expert, e)
    for (j in 3:nstrat) { #for all strategies (starts from 3rd col)
      for (k in 1:3) { #do this 3 times: best, lower, upper -> this should be changed if including confidence/CHECK
        df[i,n]= temp[(i-1)*3+k,j] #number of rows - 1, 5 times + 3
        n = n+1
      } 
    }
  }
  
  byexpert <-rbind(df, byexpert)
}

#Add expert column
byexpert %>% add_column(newColname = "Expert")
byexpert$Expert <- Expert
byexpert <- byexpert %>% 
  select(Expert, everything()) #place expert column in 1st position

#Add header names: Best, Lowest, Highest 
n = 3
s = 1 #counter for strategy number
for(j in 3:nstrat){ #for each strategy
  for(k in 1:3){ #best, lowest, highest
    names(byexpert)[n] <- paste(toString(temp[k,2]), "_", s)
    n = n+1
  } 
  s = s+1  
}

byexpert <- byexpert %>%
  rename(`Ecological Group` = `ecodata`) 

#' Standardize group labels if needed (this will be project specific)
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "example only, values are random")==1)] <- "EXAMPLE"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "GRASSLAND SPECIES")==1)] <- "Grassland Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "BURROW/DEN AND ASSOCIATED SPECIES")==1)] <- "Burrow and Den Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "SAND DUNE SPECIES")==1)] <- "Sand Dune Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "WETLAND AND SHOREBIRD SPECIES")==1)] <- "Wetland and Shorebird Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "AMPHIBIANS")==1)] <- "Amphibians"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "FISH SPECIES")==1)] <- "Fish Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "HEALTHY PRAIRIE LANDSCAPE")==1)] <- "Healthy Prairire Landscape"


#' Output results
#write_csv(byexpert, "./results/Estimates_combined.csv")
