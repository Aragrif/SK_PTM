Calculate benefit, cost, and feasibility values for new ALL STRATEGIES combinations
================
Abbey Camaclang
25 Sep 2019

Specific to SJR PTM data.
This code calculates new values for benefit, cost, and feasibility of new S22 (All strategies except S6) and S23 (All strategies except S5) combination strategies based on the expert estimates for the ALL Strategies combination (with both S5 + S6 -- this was labelled as S22 in the original tables)

Requires **Estimates\_aggregated\_benefits.csv** and **Estimates\_aggregated\_baseline.csv** from *aggregateEstimates.R*, and a **CostFeas** table of strategy costs and feasibilities.

Load packages

``` r
library(tidyverse)
library(here)
```

Read in input tables (use results from *aggregateEstimates.R*) and save original tables with different filenames

``` r
subfolder <- here("data")
grplist <- read_csv(paste0(subfolder, "/EcolGroupsList.csv")) # Table of species in each ecological group
costfeas <- read_csv(paste0(subfolder, "/CostFeas.csv")) 
resultsfolder <- here("results")
base.mat.agg <- read_csv(paste0(resultsfolder, "/Estimates_aggregated_baseline.csv"))
ben.mat.agg <- read_csv(paste0(resultsfolder, "/Estimates_aggregated_benefits.csv"))
exp.pop <- read_csv(paste0(resultsfolder, "/Estimates_aggregated_performance.csv"))
grpwtd_ben <- read_csv(paste0(resultsfolder, "/Estimates_aggregated_benefits_groupWtd.csv"))
```

Calculate new cost and feasibility
----------------------------------

``` r
S22.cost <- costfeas$Cost[which(costfeas$Strategy == "S22")] - costfeas$Cost[which(costfeas$Strategy == "S6")]
S23.cost <- costfeas$Cost[which(costfeas$Strategy == "S22")] - costfeas$Cost[which(costfeas$Strategy == "S5")]
S22.exc <- c("Baseline", "S6", costfeas$Strategy[18:length(costfeas$Strategy)]) # list of strategies to exclude from calculation of new feasibility
S22.feas <- mean(costfeas$Feasibility[!costfeas$Strategy %in% S22.exc])
S23.exc <- c("Baseline", "S5", costfeas$Strategy[18:length(costfeas$Strategy)]) # list of strategies to exclude from calculation of new feasibility
S23.feas <- mean(costfeas$Feasibility[!costfeas$Strategy %in% S23.exc])

# Add new cost and feasibility values to table and save.
new.costfeas <- costfeas %>%
  add_row(Strategy = "S23", Cost = S23.cost, Feasibility = S23.feas)
new.costfeas$Cost[which(new.costfeas$Strategy == "S22")] <- S22.cost
new.costfeas$Feasibility[which(new.costfeas$Strategy == "S22")] <- S22.feas

# write_csv(new.costfeas, "./results/CostFeas_rev.csv")
```

Calculate new benefit estimates
-------------------------------

``` r
# Get estimates needed for calculation
S5.ben <- select(ben.mat.agg, contains("_5"))
S6.ben <- select(ben.mat.agg, contains("_6"))
SAll.ben <- select(ben.mat.agg, contains("_22")) 

# Weight by feasibility
S5.wtben <- S5.ben * costfeas$Feasibility[which(costfeas$Strategy == "S5")]
S6.wtben <- S6.ben * costfeas$Feasibility[which(costfeas$Strategy == "S6")]
SAll.wtben <- SAll.ben * costfeas$Feasibility[which(costfeas$Strategy == "S22")]

# Adjust benefit estimates
S22.wtben <- SAll.wtben - (S6.wtben - S5.wtben) # new weighted benefit estimate 
S22.ben <- S22.wtben/costfeas$Feasibility[which(costfeas$Strategy == "S22")] # unweight to get values for benefit matrix

# Replace old values with new ones for benefit matrix
new.ben.agg <- cbind(ben.mat.agg, SAll.ben)
names(new.ben.agg)[(ncol(ben.mat.agg)+1):ncol(new.ben.agg)] <- c("Wt.Best.guess_23", "Wt.Lower_23", "Wt.Upper_23")

# If replacing estimates for all ecological groups:
new.ben.agg[,which(names(new.ben.agg)=="Wt.Best.guess_22")] <- S22.ben[,which(names(S22.ben)=="Wt.Best.guess_22")] 
new.ben.agg[,which(names(new.ben.agg)=="Wt.Lower_22")] <- S22.ben[,which(names(S22.ben)=="Wt.Lower_22")]
new.ben.agg[,which(names(new.ben.agg)=="Wt.Upper_22")] <- S22.ben[,which(names(S22.ben)=="Wt.Upper_22")]

# Save new table and use for *getBenefitMatrix.R*
# write_csv(new.ben.agg, "./results/Estimates_aggregated_benefits_rev.csv")
```

Calculate performance with new benefit estimates
------------------------------------------------

Unweighted performance

``` r
agg.perf <- new.ben.agg[,2:ncol(new.ben.agg)] + as.matrix(base.mat.agg[,2:ncol(base.mat.agg)])
agg.perf <- cbind(base.mat.agg, agg.perf)

# write_csv(agg.perf, "./results/Estimates_aggregated_performance_rev.csv")
```

Performance weighted by number of species in group for calculating CE scores

``` r
numspp <- apply(grplist, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]) )
grpwtd_ben <- new.ben.agg[,2:ncol(new.ben.agg)]*numspp
grpwtd_ben <- cbind(new.ben.agg[,1], grpwtd_ben)
names(grpwtd_ben)[1] <- "Ecological.Group"

# write_csv(grpwtd_ben, "./results/Estimates_aggregated_benefits_groupwtd_rev.csv")
```
