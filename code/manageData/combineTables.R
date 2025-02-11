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

library(naniar)
library(stringi)
library(dplyr)
library(plyr)
library(tidyverse)
library(data.table)
library(here)


setwd('D:/UBC/R_code/SKAnalysis') #connect working directory
#' Read in the individual tables of expert estimates and combine. NOTE to maintain confidentiality, only sample tables are provided
#+ warning = FALSE, message = FALSE
subfolder <- 'D:/UBC/R_code/SKAnalysis/data/benefits' #C:\Users\Griffy\Dropbox\SWSask_PTM_Shared\10_Results&Figs\data\benefits - here("data", "benefits") #connect benefits folder
files <- list.files(path = paste(subfolder, "/", sep=""), # Name of the subfolder in working directory that contains the files
                    pattern = "*.csv", 
                    full.names = T)

#You may not need all of these
skiplines <- 16 # number of header rows to skip (first few lines contain worksheet instructions which are not needed)
nstrat <- 17 # number of management strategies (including combinations, but excluding baseline)
numcols <- (nstrat+1+2) #*5 + 1 # total number of columns to read in (5 columns for each strategy and the baseline [Best guess, Lower, Upper, Confidence, and a Quality check column], plus 1 column for group names)
ngroups <- 7 # number of ecological groups (rows)
numrows <- (ngroups+1)*3 + 2 + 1 # total number of row to read in (5 rows for each strategy and the example [Best guess, Lower, Upper, Confidence, and a Quality check column], plus 1 row for group names)
experts <- c(1:10) # vector of expert codes, should correspond to the same order as in 'files'

#read the csvs
temp <- read.csv(files[1], skip = skiplines, nrows = numrows, header = TRUE)
temp <- temp %>% 
    select(0:numcols) %>%
    filter(!row_number() %in% c(1)) #remove empty rows
names(temp) <- lapply(temp[1, ], as.character) #Make first row into header
temp <- temp[-1,] 
temp <- temp[-1,] 
temp <- data.frame(temp, fix.empty.names = TRUE) #rename empty colnames

#ecological groups
ecodata <- pull(temp, Ecological.Groups) #create a group of ecological names
ecodata <- data_frame(as.data.frame(ecodata)) #make them a data frame

#remove blank rows or NA from ecological groups
ecodata <- ecodata[!apply(ecodata == "", 1, all), ] #remove blank rows
ecodata <- ecodata %>% drop_na() #remove NA

#create empty dataframe for output
byexpert <- data.frame() 
Expert <- list()
e = 0
df<-ecodata

for (i in 1:length(experts)){ # else use length(files) if all estimates are available
  temp2 <- read_csv(files[i], skip = skiplines)
  temp2 <- temp2 %>% 
    select(0:numcols) %>%
    filter(!row_number() %in% c(1))#shouldn't read a file in a loop? Do I need to to read each expert estimate?
  #names(temp) <- lapply(temp[1, ], as.character) #Make first row into header
  #temp <- temp[-1,] 
  temp2 <- data.frame(temp2, fix.empty.names = TRUE) #rename empty colnames
  colnames(temp2)[2]<- "Col" #gives column 2 a name
  temp2$Col <- stri_replace_all_regex(temp2$Col,
                                     pattern=c('Best', 'Highest', '\\<High\\>', 'Lowest', '\\<Low\\>'),
                                     replacement=c('Best guess', 'Upper', 'Upper', 'Lower', 'Lower'),
                                     vectorize=FALSE)
  temp2 <- temp2[!grepl("CHECK", temp2$Col),]
  temp2 <- temp2[!grepl("CONFIDENCE", temp2$Col),]
  e = e+1 #counter for number of experts/sheets
  
  #create data frame for loop output with row names for Ecological Groups
  #df<-ecodata
  
  #reorganize estimates
  for (h in 1:nrow(ecodata)) { #for all ecological groups (including example)
    n = 2 #starts from two, so eco groups are first column
    Expert <- append(Expert, e) #fills a list of expert numbers
    for (j in 3:ncol(temp2)) { #for all strategies (starts from 3rd col)
      for (k in 1:3) { #do this 3 times: best, lower, upper -> this should be changed if including confidence/CHECK
        df[h,n]= temp2[(h-1)*3+k,j] #number of rows - 1, 5 times + 3
        n = n+1
      } 
    }
  }
  
  byexpert <-rbind(byexpert, df)
}

#Change X's to NA
na_strings <- c("X", "X ", "x", "x ")
byexpert <- byexpert %>% 
  replace_with_na_all(condition=~.x %in% na_strings) 

#Add expert column
Expert <- as.character(Expert)
byexpert %>% add_column(newColname = "Expert")
byexpert$Expert <- Expert
byexpert <- byexpert %>% 
  select(Expert, everything()) #place expert column in 1st position

#Add header names: Best, Lowest, Highest 
#byexpert2 <- byexpert
n = 3
s = 1 #counter for strategy number
for(j in 3) {
  for(k in 1:3){ #best, lowest, highest
    names(byexpert)[n] <- paste(toString(temp2[k,2]))
    n = n+1
  } 
}
clist <- c("Best guess", "Lower", "Upper")
for(j in 1:nstrat){ #for each strategy; 
  for(k in clist){ #best_1, lowest_1, highest_1 etc - rows
    names(byexpert)[n] <- paste(k, "_", s)
    n = n+1
  } 
  s = s+1  
}

byexpert <- byexpert %>%
  dplyr::rename(`Ecological Group` = `ecodata`) 

bestguess_base <- byexpert[,grep("Best guess$", colnames(byexpert))]
lower_base <- byexpert[,grep("Lower$", colnames(byexpert))]
upper_base <- byexpert[,grep("Upper$", colnames(byexpert))]

# Find strategy column indices
bestguess <- grep("Best guess",colnames(byexpert))
lowest <- grep("Lower", colnames(byexpert))
highest <- grep("Upper", colnames(byexpert))

# For each relevant column, replace "b" with baseline values from the same row
# This needs to be fixed later - this was copied
for (i in 1:length(bestguess)) {
  bg_temp <- which(byexpert[,bestguess[i]]=="B" | byexpert[,bestguess[i]]=="b")
  byexpert[bg_temp,bestguess[i]] <- bestguess_base[bg_temp,]
  
  l_temp <- which(byexpert[,lowest[i]]=="B" | byexpert[,lowest[i]]=="b")
  byexpert[l_temp,lowest[i]] <- lower_base[l_temp,]
  
  u_temp <- which(byexpert[,highest[i]]=="B" | byexpert[,highest[i]]=="b")
  byexpert[u_temp,highest[i]] <- upper_base[u_temp,]
}

#' Standardize group labels if needed (this will be project specific)
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "example only, values are random")==1)] <- "EXAMPLE"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "GRASSLAND SPECIES")==1)] <- "Grassland Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "BURROW")==1)] <- "Burrow and Den Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "SAND DUNE SPECIES")==1)] <- "Sand Dune Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "WETLAND")==1)] <- "Wetland and Shorebird Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "AMPHIBIANS")==1)] <- "Amphibians"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "FISH SPECIES")==1)] <- "Fish Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "HEALTHY")==1)] <- "Healthy Prairie Landscape"

byexpert <- byexpert[!grepl('EXAMPLE', byexpert$`Ecological Group`),]

names(byexpert) <- gsub(" _ ", "_", names(byexpert))
names(byexpert) <- gsub(" ", ".", names(byexpert))

#' Output results
write.csv(byexpert, file="./results/Estimates_combined.csv", row.names=FALSE) 




