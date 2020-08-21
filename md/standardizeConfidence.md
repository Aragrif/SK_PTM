Standardize Benefit Estimates
================
Adapted for the SJR PTM by Abbey Camaclang
28 June 2019

This script standardizes the benefit estimates and saves results tables as .csv files:
1) **Estimates\_tidy.csv** - Tidy (long) version of the original **Estimates\_combined.csv** file
2) **Estimates\_count\_group.csv** - Number of expert estimates for each ecological group
3) **Estimates\_count\_strategy.csv** - Number of expert estimates there are for each strategy x group
4) **Estimates\_std\_wide.csv** - Standardized estimates in same table format as Estimates\_combined.csv
5) **Estimates\_std\_long.csv** - Tidy version of Standardized estimates - for use in plotting

It requires output from *combineTables.R*, which organizes the estimates into a single table and saves it as **Estimates\_combined.csv** in the data folder.

Load packages

``` r
library(tidyverse)
library(stringr)
library(here)
```

Read in and tidy data
---------------------

``` r
subfolder <- here("results")
estimates <- read.csv(paste0(subfolder,"/Estimates_combined.csv"))
# head(estimates)
```

Use tidyr package to transform data to tidy version, with single columns for Estimate (e.g., best guess, lower, upper) and Value (value of estimate, 0-100)

``` r
rlong <-
  gather(estimates,
         key = Estimate,
         value = Value,
         Best.guess:Confidence_22)
head(rlong)
```

    ##   Expert                   Ecological.Group   Estimate Value
    ## 1      1                     Migratory Fish Best.guess     3
    ## 2      1                   Riparian Species Best.guess    NA
    ## 3      1                    Aquatic Species Best.guess    NA
    ## 4      1                    Wetland Species Best.guess    NA
    ## 5      1 Grassland, Open, or Agricult Assoc Best.guess    NA
    ## 6      1 Mature Forest and Peatland Species Best.guess    NA

``` r
rlong <- na.omit(rlong)
# write_csv(rlong, "./results/Estimates_tidy.csv")

rlong$Value <- as.numeric((rlong$Value))
# str(rlong) # Check data type
```

Summarize the number of expert estimates
----------------------------------------

Tabulate how many expert estimates there are for each ecological group

``` r
table.data <-spread(rlong, Estimate, Value) 
table.subset <- table.data[, c(1, 2)] # Subset table.data to only include the columns "Expert" and "Ecological.Group"
exp.table <- table(table.subset$Ecological.Group)
# write.csv(exp.table, "./results/Estimates_count_group.csv", row.names=FALSE)
exp.table
```

    ## 
    ##                          Aquatic Species 
    ##                                        9 
    ##                                     Bats 
    ##                                       10 
    ## Forest Openings and Young Forest Species 
    ##                                       10 
    ##                             Forest Trees 
    ##                                        8 
    ##       Grassland, Open, or Agricult Assoc 
    ##                                       13 
    ##       Mature Forest and Peatland Species 
    ##                                        8 
    ##                           Migratory Fish 
    ##                                       12 
    ##                         Riparian Species 
    ##                                       12 
    ##                          Wetland Species 
    ##                                       13

Create new columns to specify Estimate Type and Strategy separately

``` r
# Find the "_" character and uses the digits that follow it as the Strategy name. 
rlong$Strategy <- "BLANK"
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rlong$Estimate[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

# Create a new column for type of estimate
rlong$Est.Type <- "BLANK" # creates a new column in the table
rlong$Est.Type[grep("Best.guess", rlong$Estimate)] <- "Best.Guess"
rlong$Est.Type[grep("Lower", rlong$Estimate)] <- "Lower"
rlong$Est.Type[grep("Upper", rlong$Estimate)] <- "Upper"
rlong$Est.Type[grep("Confidence", rlong$Estimate)] <- "Confidence"
```

Tabulate how many estimates there are for each strategy

``` r
table.subset2 <- subset(rlong, Est.Type=="Best.Guess") # Subset to count how many experts provided estimates for each group and strategy
strat.levels <- unique(table.subset2$Strategy)
table.subset2$Strategy <- factor(table.subset2$Strategy,levels = strat.levels)
st.table <- table(table.subset2$Ecological.Group, table.subset2$Strategy)
# write.csv(st.table, "./results/Estimates_count_strategy.csv")
st.table
```

    ##                                           
    ##                                            Baseline S1 S2 S3 S4 S5 S6 S7
    ##   Aquatic Species                                 9  8  9  8  8  9  8  9
    ##   Bats                                           10 10 10  9  9  9  9  9
    ##   Forest Openings and Young Forest Species       10 10 10 10 10 10 10 10
    ##   Forest Trees                                    8  8  8  8  8  8  8  8
    ##   Grassland, Open, or Agricult Assoc             13 12 12 12 12 12 12 12
    ##   Mature Forest and Peatland Species              8  8  8  8  8  8  8  8
    ##   Migratory Fish                                 12 12 12 12 12 12 12 12
    ##   Riparian Species                               12 12 11 12 10 12 11 12
    ##   Wetland Species                                13 13 13 13 13 13 13 13
    ##                                           
    ##                                            S8 S9 S10 S11 S12 S13 S14 S15
    ##   Aquatic Species                           9  9   8   7   7   9   8   9
    ##   Bats                                      9  9   8  10   8   9   9   9
    ##   Forest Openings and Young Forest Species 10 10   9   9   9  10  10   9
    ##   Forest Trees                              8  8   7   7   8   8   8   8
    ##   Grassland, Open, or Agricult Assoc       12 12  11  11  11  12  12  12
    ##   Mature Forest and Peatland Species        8  8   7   7   7   8   8   8
    ##   Migratory Fish                           12 12  12  10   9  12  11  11
    ##   Riparian Species                         11 12  11  10  10  12  11  11
    ##   Wetland Species                          13 13  12  11  11  12  13  12
    ##                                           
    ##                                            S16 S17 S18 S19 S20 S21 S22
    ##   Aquatic Species                            9   7   7   8   7   7   8
    ##   Bats                                       9   9   8   8   7   9   9
    ##   Forest Openings and Young Forest Species  10   9   9   9   8   9   9
    ##   Forest Trees                               8   7   7   7   6   6   6
    ##   Grassland, Open, or Agricult Assoc        13  11  11  11  10  11  12
    ##   Mature Forest and Peatland Species         8   7   7   7   6   7   7
    ##   Migratory Fish                            12  11  11  11  11  10  12
    ##   Riparian Species                          12  11  11  11  10  11  11
    ##   Wetland Species                           12  12  12  11  11  12  12

Standardize to a specified confidence level
-------------------------------------------

Standardize upper and lower estimates to 80% confidence.
1) For each estimate, divide confidence by 100
2) Take the upper and lower estimates, and standardize using
\* Lower standardized interval: B−((B−L)×(S∕C))
\* Upper standardized interval: B+((U−B)×(S∕C))
3) Leave Best Guess estimate as is

``` r
S <- 0.8 # confidence level to standardize estimates to
```

Subset dataframe by estimate type

``` r
bg <- subset(rlong, Est.Type == "Best.Guess")
low <- subset(rlong, Est.Type == "Lower")
up <- subset(rlong, Est.Type == "Upper")
conf <- subset(rlong, Est.Type == "Confidence")
```

Check that order of rows are the same

``` r
# Results should equal the number of rows in tables, if all entries are matching
# test1 <- sum(str_detect(bg$Ecological.Group,low$Ecological.Group)) 
# test2 <- sum(str_detect(bg$Ecological.Group,up$Ecological.Group))
# test3 <- sum(str_detect(bg$Ecological.Group,conf$Ecological.Group))
# test4 <- sum(bg$Expert == low$Expert) # checks that Experts also align
```

Create copies of the datasets and add new columns with the standardized estimates

``` r
low.new <- low
up.new <- up
bg.new <- bg
conf.new <- conf

conf.new$St.Value <- (conf$Value) / 100
low.new$St.Value <-
  bg$Value - (((bg$Value) - (low$Value)) * (S / (conf.new$St.Value)))
up.new$St.Value <-
  bg$Value + (((up$Value) - (bg$Value)) * (S / (conf.new$St.Value)))
bg.new$St.Value <- bg$Value
```

Combine into new tables and saves standardized estimates into .csv files

``` r
rlong.std <- rbind(low.new, up.new, bg.new, conf.new) 

# Constrain standardized values to 0 - 100
rlong.std$St.Value[rlong.std$St.Value < 0] <- 0 
rlong.std$St.Value[rlong.std$St.Value > 100] <- 100

# Create new table in wide format
rlong.sub <- rlong.std[,c(1,2,3,7)] 
rlong.wide <- spread(rlong.sub, Estimate, St.Value)

# Make sure Strategies are in correct order
est.levels <- unique(rlong$Estimate)
rlong.wide <- rlong.wide[, c("Expert", "Ecological.Group", est.levels)] 

# Make sure Ecological groups are in the same order as in original tables
grp.levels <- unique(rlong.std$Ecological.Group)
rlong.wide$Ecological.Group<-factor(rlong.wide$Ecological.Group, levels=grp.levels) 
rlong.wide <- with(rlong.wide, rlong.wide[order(Expert, Ecological.Group),]) 

# Output results
# write_csv(rlong.wide, "./results/Estimates_std_wide.csv") 
# write_csv(rlong.std, "./results/Estimates_std_long.csv")
```
