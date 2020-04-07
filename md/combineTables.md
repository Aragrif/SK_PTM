Compile Expert Estimates
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This script reads individual expert estimates from multiple .csv files and compiles them into a single **Estimates\_combined.csv** file. It requires that each expert table is saved as a .csv file in a *benefits* subfolder within the *data* folder, contain the same number of rows and columns, and no other .csv files are in the same folder.

``` r
library(stringi)
library(tidyverse)
library(naniar)
library(here)
```

Read in the individual tables of expert estimates and combine. NOTE to maintain confidentiality, only sample tables are provided

``` r
subfolder <- here("data", "benefits")
files <- list.files(path = paste(subfolder, "/", sep=""), # Name of the subfolder in working directory that contains the files
           pattern = "*.csv", 
           full.names = T)

skiplines <- 14 # number of header rows to skip (first few lines contain worksheet instructions which are not needed)
nstrat <- 22 # number of management strategies (including combinations, but excluding baseline)
numcols <- (nstrat+1)*5 + 1 # total number of columns to read in (5 columns for each strategy and the baseline [Best guess, Lower, Upper, Confidence, and a Quality check column], plus 1 column for group names)
ngroups <- 9 # number of ecological groups (rows)
experts <- c(1:19) # vector of expert codes, should correspond to the same order as in 'files'

temp <- read_csv(files[1], skip = skiplines) 
temp <- temp[1:ngroups,1:numcols] %>% # Keeps only the relevant rows and columns 
  select(.,-contains("Quality")) %>% # Excludes the columns used to check data quality
  add_column(.,Expert=rep(experts[1],ngroups),.before="Ecological Group") # add a column for expert code
byexpert <- temp

for (i in 2:length(experts)){ # else use length(files) if all estimates are available
  temp <- read_csv(files[i], skip = skiplines)
  temp <- temp[1:ngroups,1:numcols] %>%
    select(.,-contains("Quality")) %>%
    add_column(.,Expert=rep(experts[i],ngroups),.before="Ecological Group")
  byexpert<-rbind(byexpert,temp)
  }
```

Recode "X" to NA

``` r
na_strings <- c("X", "X ", "x", "x ")
byexpert <- byexpert %>% 
  replace_with_na_all(condition=~.x %in% na_strings)
```

Recode "B" to baseline values

``` r
# Get the baseline values
bestguess_base <- byexpert[,grep("Best guess$", colnames(byexpert))]
lower_base <- byexpert[,grep("Lower$", colnames(byexpert))]
upper_base <- byexpert[,grep("Upper$", colnames(byexpert))]
conf_base <- byexpert[,grep("Confidence$", colnames(byexpert))]

# Find strategy column indices
bestguess <- grep("Best guess_",colnames(byexpert))
lowest <- grep("Lower_", colnames(byexpert))
highest <- grep("Upper_", colnames(byexpert))
conf <- grep("Confidence_",colnames(byexpert))

# For each relevant column, replace "b" with baseline values from the same row
for (i in 1:length(bestguess)) {
  bg_temp <- which(byexpert[,bestguess[i]]=="B" | byexpert[,bestguess[i]]=="b")
  byexpert[bg_temp,bestguess[i]] <- bestguess_base[bg_temp,]
  
  l_temp <- which(byexpert[,lowest[i]]=="B" | byexpert[,lowest[i]]=="b")
  byexpert[l_temp,lowest[i]] <- lower_base[l_temp,]
  
  u_temp <- which(byexpert[,highest[i]]=="B" | byexpert[,highest[i]]=="b")
  byexpert[u_temp,highest[i]] <- upper_base[u_temp,]
  
  byexpert[l_temp,conf[i]] <- conf_base[l_temp,] # using the index for lower as some may have been left blank/NA
}
```

Standardize group labels if needed (this will be project specific)

``` r
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest Species")==1)] <- "Mature Forest and Peatland Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest/ Peatland Species")==1)] <- "Mature Forest and Peatland Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Mature Forest/Peatland Species")==1)] <- "Mature Forest and Peatland Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Grassland/Open Habitat species")==1)] <- "Grassland, Open, or Agricult Assoc"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Grassland or Open Habitat Species")==1)] <- "Grassland, Open, or Agricult Assoc"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "Forest Openings and Young Forest")==1)] <- "Forest Openings and Young Forest Species"
```

Output results

``` r
# write_csv(byexpert, "./data/Estimates_combined.csv")
```
