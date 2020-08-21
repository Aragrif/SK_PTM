Generate Benefits Matrix for Complementarity Analysis
================
Adapted for the SJR PTM by Abbey Camaclang
3 July 2019

This code weights benefits by feasibility, recalculates expected performance based on weighted benefit estimates, and generates the benefit matrix for use in the complementarity analysis. Based on 1\_Cost-Effectiveness.R code from FRE PTM project, but using a different (shorter) way to implement

Requires **Estimates\_aggregated\_benefits.csv** and **Estimates\_aggregated\_baseline.csv** from *aggregateEstimates.R*, and a **CostFeas** table of strategy costs and feasibilities

Load package

``` r
library(tidyverse)
library(here)
```

Read in data tables

``` r
subfolder <- here("results")
ben.mat.agg <- read_csv(paste0(subfolder, "/Estimates_aggregated_benefits_rev.csv"))
base.mat.agg <- read_csv(paste0(subfolder, "/Estimates_aggregated_baseline.csv"))

costfeas <- read_csv(paste0(subfolder, "/CostFeas_rev.csv"))
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)
```

Calculate the expected benefits (= benefit x feasibility)
---------------------------------------------------------

``` r
# Tidy data
rlong.ben <- gather(ben.mat.agg, key = Estimate, value = Value, 
                    colnames(ben.mat.agg)[2]:colnames(ben.mat.agg)[ncol(ben.mat.agg)]) %>%
  separate(., Estimate, c("Est.Type", "Strategy"), sep = "[_]", remove = FALSE)
rlong.ben$Strategy <- as_factor(paste0("S", rlong.ben$Strategy))

# Join with cost & feasibility table then weight benefits by feasibility
joined.data <- left_join(rlong.ben, costfeas, by = "Strategy") %>%
  mutate(., Wt.Value = Value * Feasibility)
names(joined.data)[1] <- "Ecological.Group"

# Spread table and output results
joined.wide <- joined.data %>%
  select(., c(Ecological.Group, Estimate, Wt.Value)) %>%
  spread(key = Estimate, value = Wt.Value)
est.levels <- unique(joined.data$Estimate)
joined.wide <- joined.wide[, c("Ecological.Group", est.levels)] # rearranges columns so strategies are in the correct order

# write_csv(joined.wide, "./results/Expected_Benefits.csv")
```

Calculate expected performance (= baseline probability of persistence + expected benefits)
------------------------------------------------------------------------------------------

``` r
# Join with baseline estimates to make sure the observations (Ecol. groups) line up correctly
# then split again to add weighted benefits to (averaged) baseline and get the expected performance
joinedbase.wide <- left_join(base.mat.agg, joined.wide, by = "Ecological.Group") 

base.mat <- joinedbase.wide[,2:4]
perf.mat <- joinedbase.wide[,5:ncol(joinedbase.wide)] + as.matrix(base.mat)

perf.mat <- cbind(joinedbase.wide$Ecological.Group,base.mat,perf.mat)
names(perf.mat)[1] <- "Ecological.Group"

# write_csv(perf.mat, "./results/Expected_Performance.csv")

# Create expected performance matrix for complementarity analysis (optimization) and uncertainty analysis
# Best guess (most likely) estimates
wt.ben <- perf.mat %>%
  select(., c(Ecological.Group, contains("Best.guess"))) 
wt.ben.t <- data.frame(t(wt.ben[,-1]))
names(wt.ben.t) <- wt.ben$Ecological.Group 

# Lower (most pessimistic)
wt.ben.low <- perf.mat %>%
  select(., c(Ecological.Group, contains("Lower"))) 
wt.ben.low.t <- data.frame(t(wt.ben.low[,-1]))
names(wt.ben.low.t) <- wt.ben.low$Ecological.Group 

# Upper (most optimistic)
wt.ben.hi <- perf.mat %>%
  select(., c(Ecological.Group, contains("Upper"))) 
wt.ben.hi.t <- data.frame(t(wt.ben.hi[,-1]))
names(wt.ben.hi.t) <- wt.ben.hi$Ecological.Group 

# Create vector of strategy names to add to the tables
strat.names <- vector()
strat.names[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rownames(wt.ben.t)[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
strat.names[which(str_detect(rownames(wt.ben.t), "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

wt.ben.t <- cbind(strat.names,wt.ben.t)
names(wt.ben.t)[1] <- "Strategy"

wt.ben.low.t <- cbind(strat.names, wt.ben.low.t)
names(wt.ben.low.t)[1] <- "Strategy"

wt.ben.hi.t <- cbind(strat.names, wt.ben.hi.t)
names(wt.ben.hi.t)[1] <- "Strategy"

# Output new tables
# write_csv(wt.ben.t, "./results/BestGuess.csv") # use this table for the complementarity analysis
# write_csv(wt.ben.low.t, "./results/Lower.csv") # for uncertainty analysis under most pessimistic scenario
# write_csv(wt.ben.hi.t, "./results/Upper.csv") # for uncertainty analysis under most optimistic scenario
```
