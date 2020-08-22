Uncertainty Analysis (Benefit estimates)
================
Abbey Camaclang
27 Aug 2019

This code is used to run the uncertainty analysis using the confidence bounds from experts' benefit estimates.
Requires **Estimates\_std\_long.csv** from *standardizeConfidence.R*, as well as **CostFeas.csv** (with the new combo strategies), **EcolGroupsList.csv**, and **SpecialCases.csv**.

Based on algorithm of R code written by Danial Stratford (<danial.stratford@csiro.au>), found in PTM Handbook Supporting Info.
For each MC iteration,
1. Sample performance value for each expert - ecological group - strategy using rpert from mc2d package, shape = 4 (default)
2. Calculate benefit Bi = pi - p0
3. Average across experts
4. Sum over all ecological groups to get total benefit value for each strategy
5. Calculate cost-effectiveness CEi = (Bi \* Fi)/Ci

Code not copied directly as data frames for this project are structured differently and wanted to avoid additional rearranging.
Load packages

``` r
library(mc2d)
library(tidyverse)
library(cowplot)
library(ggridges)
library(viridis)
library(here)

MC <-  10000 # number of iterations
a <- 1000000 # scaling for CE scores
```

Prepare data for sampling
-------------------------

``` r
datafolder <- here("data")
resultfolder <- here("results")
costfeas <- read_csv(paste0(resultfolder, "/CostFeas_rev.csv"))
costfeas$Strategy <- as_factor(costfeas$Strategy)
costfeas <- costfeas[-1,] # Remove baseline values

rlong.std <- read_csv(paste0(resultfolder, "/Estimates_std_long.csv")) # use read_csv to make sure factors read in as character

# Specify factor levels
grp.levels <- unique(rlong.std$Ecological.Group)
expcode <- unique(rlong.std$Expert) 

newstrat.levels <- levels(costfeas$Strategy)
rlong.std$Strategy <- factor(rlong.std$Strategy, levels = newstrat.levels)

est.levels <- c("Lower", "Best.Guess", "Upper")
rlong.std <- subset(rlong.std, Est.Type %in% c("Best.Guess", "Lower", "Upper")) #remove entries for 'Confidence'
rlong.std$Est.Type <- factor(rlong.std$Est.Type, levels = est.levels)

# Rearrange table so estimates for each group * strategy are on the same row
rlong.sub2 <- rlong.std[,c(1,2,5,6,7)]
rlong.std.wide <- spread(rlong.sub2, key = Est.Type, value = St.Value)
rlong.std.wide$Expert <- as.factor(rlong.std.wide$Expert)
rlong.std.wide$Ecological.Group <- factor(rlong.std.wide$Ecological.Group, levels = grp.levels)

# Get number of species in each group and number of species scored by each expert for each strategy
grplist <- read_csv(paste0(datafolder, "/EcolGroupsList.csv")) # Table of species in each ecological group
numspp <- apply(grplist, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]) )

grpwts <- data.frame(Ecological.Group = names(numspp), numspp) 
grpwts$Ecological.Group <- factor(grpwts$Ecological.Group, levels = grp.levels)

spcases <- read_csv(paste0(datafolder, "/SpecialCases.csv")) # Table of number of species in each group scored by individual experts (if different from total)
spcases$Strategy <- str_c("S", spcases$Strategy)
spcases$Strategy[which(spcases$Strategy == "SBaseline")] <- "Baseline"
spcases$Strategy <- factor(spcases$Strategy, levels = newstrat.levels)
spcases$Expert <- factor(spcases$Expert, levels = levels(rlong.std.wide$Expert))
spcases$`Ecological Group`<- factor(spcases$`Ecological Group`, levels = grp.levels)
names(spcases)[which(str_detect(names(spcases), "Ecological Group") == 1)] <- "Ecological.Group"  
```

For SJR PTM only
----------------

Derive new S22 estimates Rename S22 as S23 and calculate new S22

``` r
temp.table <- filter(rlong.std.wide, Strategy %in% c("S5", "S6", "S22")) %>%
  gather(`Lower`, `Best.Guess`, `Upper`, key = Type, value = Estimate) %>%
  spread(key = Strategy, value = Estimate) %>%
  rename(S23 = S22) %>%
  mutate(Diff = S6 - S5, S22 = S23 - Diff) 
 
S23.table <- select(temp.table, Expert, Ecological.Group, Type, S23) %>%
  spread(key = Type, value = S23) %>%
  mutate(Strategy = "S23") %>%
  select(Expert, Ecological.Group, Strategy, Lower, Best.Guess, Upper)
S23.table$Strategy <- factor(S23.table$Strategy, levels = newstrat.levels)

new.S22 <- select(temp.table, Expert, Ecological.Group, Type, S22) %>%
  spread(key = Type, value = S22) %>%
  mutate(Strategy = "S22") %>%
  select(Expert, Ecological.Group, Strategy, Lower, Best.Guess, Upper) %>%
  gather(`Lower`, `Best.Guess`, `Upper`, key = Type, value = Estimate)
```

Ensure that new S22 estimates are &gt; or = to the max estimates of component strategies (i.e., S1 - S5 and S7 - S21)

``` r
# Get max estimates of component strategies (i.e., S1-S5 and S7 - S21)
compstrat <- filter(rlong.std.wide, !Strategy %in% c("S6", "S22"))
grouped <- group_by(compstrat, Expert, Ecological.Group) %>%
  filter(Best.Guess == max(Best.Guess)) %>%
  filter(Lower == max(Lower)) %>%
  filter(Upper == max(Upper)) %>%
  slice(which.max(Best.Guess)) %>%
  gather(`Lower`, `Best.Guess`, `Upper`, key = Type, value = Estimate)

# Compare max estimates to new S22 estimates and use the higher value
joined <- full_join(grouped, new.S22, by = c("Expert", "Ecological.Group", "Type")) %>%
  rename(S22 = Estimate.y, Other = Estimate.x) %>%
  select(Expert, Ecological.Group, Type, Other, S22) %>%  # remove unnecessary columns
  gather(Other, S22, key = Strategy, value = Estimate) %>%
  arrange(Expert, Ecological.Group) %>% # re-order rows
  spread(key = Type, value = Estimate) %>%
  filter(Best.Guess == max(Best.Guess)) %>% # get the maximum of the values for each expert * ecological group
  gather(Best.Guess, Lower, Upper, key = Type, value = Estimate) %>%
  spread(key = Strategy, value = Estimate)

joined$S22[which(is.na(joined$S22))] <- joined$Other[which(is.na(joined$S22))]

# Extract updated S22 values
S22.table <- select(joined, -Other) %>%
  spread(key = Type, value = S22) %>%
  mutate(Strategy = "S22") %>%
  select(Expert, Ecological.Group, Strategy, Lower, Best.Guess, Upper) #need to change Strategy into factor
S22.table$Strategy <- factor(S22.table$Strategy, levels = newstrat.levels)

# Constrain new estimates to 0-100 range
S22.table$Lower[which(S22.table$Lower < 0)] <- 0
S22.table$Upper[which(S22.table$Upper > 100)] <- 100
```

Update table with new estimates

``` r
rlong.std.wide.orig <- rlong.std.wide # store original version first, just in case

rlong.std.wide$Strategy<-factor(rlong.std.wide$Strategy, levels = newstrat.levels)
rlong.std.wide <- filter(rlong.std.wide, Strategy != "S22") # remove old S22 estimates from table
rlong.std.wide <- bind_rows(rlong.std.wide, S22.table, S23.table) %>% # add new S22 estimates and S23 estimates
  # factor with S23 as a new level
  arrange(Expert, Ecological.Group, Strategy)  
```

Update spcases table with values for new S22, S23

``` r
# Duplicate S22, rename the duplicate as S23
spcases.S23 <- spcases %>%
  filter(Strategy == "S22") %>%
  mutate(Strategy = "S23")
spcases.all <- rbind(spcases, spcases.S23)
# write_csv(spcases.all, "./results/SpecialCases_new.csv")
```

Uncertainty analysis for benefit uncertainty
--------------------------------------------

``` r
samples <- matrix(nrow = nrow(rlong.std.wide),ncol = MC)
MC.CE_Score <- list()
Benefits_uncrtn <- list()

set.seed(1565)

for(it in 1:MC){
 
  # Sample benefit values
  samples[,it] <- rpert(nrow(rlong.std.wide),
                        min = rlong.std.wide$Lower,
                        mode = rlong.std.wide$Best.Guess,
                        max = rlong.std.wide$Upper, shape=4)
  
  temp <- cbind(rlong.std.wide[,1:3], samples[,it])
  names(temp)[4] <- "MC.Value"
  
  temp.wide <- spread(temp, key = Strategy, value = MC.Value)
  temp.strat <- select(temp.wide, as.character(newstrat.levels[2]):as.character(newstrat.levels[length(newstrat.levels)])) 
  
  temp.ben <- temp.strat - temp.wide$Baseline
  temp.ben[temp.ben<0] <- 0 # replaces negative values with 0 (assume same as baseline) - Constrained samples
  temp.ben <- cbind(temp.wide[,1:2], temp.ben) 
  
  temp.long <- gather(temp.ben, key = Strategy, value = MCValue, -c(1:2))
  temp.long$Strategy <- factor(temp.long$Strategy, levels = newstrat.levels)
  
  # Combine tables to calculate weights for each expert - group - strategy 
  temp.joined <- left_join(temp.long, spcases.all, by=c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  fullwts.idx <- which(is.na(temp.joined$NumSppScored))
  temp.joined$NumSppScored[fullwts.idx] <- temp.joined$numspp[fullwts.idx]
  
  fullwts <- aggregate(temp.joined$NumSppScored, 
                       by = list(Ecological.Group = temp.joined$Ecological.Group, Strategy = temp.joined$Strategy), 
                       FUN = sum, na.rm = TRUE)
  temp.joined <- temp.joined %>%
    left_join(., fullwts, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.MCValue = MCValue*Wts)
  temp.joined$Strategy <- factor(temp.joined$Strategy, levels = newstrat.levels)
  
  # Aggregate across experts
  temp.agg <- aggregate(temp.joined$Wt.MCValue, 
                        by = list(Ecological.Group = temp.joined$Ecological.Group, Strategy = temp.joined$Strategy), 
                        FUN = sum, na.rm = TRUE)

  # Weight benefits by number of species in group
  wt.temp.agg <- left_join(temp.agg, grpwts, by = "Ecological.Group") %>%
    mutate(Wt.Benefit = x*numspp)    
  
  # Calculate total benefit of each strategy across ecological groups
  # First rearrange so strategies are in rows and ecol groups are in columns - basically need to end up with col vector of total benefit
  sum.ben <- aggregate(wt.temp.agg$Wt.Benefit, 
                       by = list(Strategy = wt.temp.agg$Strategy),
                       FUN = sum, na.rm = TRUE)
  names(sum.ben) <- c("Strategy", "Benefit")
  sum.ben$Strategy <- factor(sum.ben$Strategy, levels = newstrat.levels)
  
  # Join with cost/feasibility table and calculate cost-effectiveness
  strat.est <- left_join(sum.ben, costfeas, by="Strategy") %>%
    mutate(., Sc.Cost = Cost/a, # scale costs to get reasonable values
           Exp.Benefit = Benefit * Feasibility, # weight benefits
           CE = (Benefit * Feasibility)/Sc.Cost) # calculate cost-effectiveness scores
  
  # Rank strategies by (weighted)Benefit, Cost, and CE score
  CE_Score <- select(strat.est, c("Strategy", "Benefit", "Cost", "Feasibility", "Exp.Benefit","CE")) %>%
    mutate(., CE_rank = rank(-CE), 
           ExpBenefit_rank = rank(-Exp.Benefit), 
           Cost_rank = rank(Cost))
  
  MC.CE_Score[[it]] <- CE_Score
  
  # Calculate expected performance for use as Benefit matrix in optimization
  # Get unweighted aggregated benefit estimates and weight by feasibility
  temp.joined.data <- left_join(wt.temp.agg, costfeas, by = "Strategy") %>%
    mutate(., Wt.Value = x * Feasibility) # weight by feasibility
  wide.temp.agg <- select(temp.joined.data, Ecological.Group, Strategy, Wt.Value) %>%
    spread(Strategy, Wt.Value)
  
  # Aggregate baseline
  temp.base <- temp.wide[,1:3]
  temp.base$Strategy <- "Baseline"
  temp.base$Strategy <- factor(temp.base$Strategy, levels = newstrat.levels)
  names(temp.base)[3] <- "MCValue"
  temp.base.joined <- left_join(temp.base, spcases, by=c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  fullwts.idx.base <- which(is.na(temp.base.joined$NumSppScored))
  temp.base.joined$NumSppScored[fullwts.idx.base] <- temp.base.joined$numspp[fullwts.idx.base]
  
  fullwts.base <- aggregate(temp.base.joined$NumSppScored, 
                            by = list(Ecological.Group = temp.base.joined$Ecological.Group, Strategy = temp.base.joined$Strategy), 
                            FUN = sum, na.rm = TRUE)
  temp.base.joined <- temp.base.joined %>%
    left_join(., fullwts.base, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.MCValue = MCValue*Wts)
  
  temp.base.agg <- aggregate(temp.base.joined$Wt.MCValue, 
                             by = list(Ecological.Group = temp.base.joined$Ecological.Group, Strategy = temp.base.joined$Strategy), 
                             FUN = sum, na.rm = TRUE)
  names(temp.base.agg)[3] <- "Wt.Best.guess"
  
  # Calculate performance and save in a list
  wt.perf <- wide.temp.agg[,2:ncol(wide.temp.agg)] + as.matrix(temp.base.agg[,3])
  Benefits_uncrtn[[it]] <- wt.perf %>%
    add_column(Ecological.Group = wide.temp.agg$Ecological.Group, .before = "S1")
  
} #end for
```

Get results and save as .csv files

``` r
MC.CE_Table <- lapply(MC.CE_Score, "[", 1:length(strat.est$Strategy), "CE")
MC.Results <- matrix(unlist(MC.CE_Table), ncol = MC, byrow = FALSE)
MC.Results <- data.frame(strat.est$Strategy[1:length(strat.est$Strategy)], MC.Results)
names(MC.Results)[1] <- "Strategy"

MC.CE_Rank <- lapply(MC.CE_Score, "[", 1:length(strat.est$Strategy), "CE_rank")
MC.Ranks <- matrix(unlist(MC.CE_Rank), ncol = MC, byrow = FALSE)
MC.Ranks <- data.frame(strat.est$Strategy[1:length(strat.est$Strategy)], MC.Ranks)
names(MC.Ranks)[1] <- "Strategy"

MC_Samples <- data.frame(rlong.std.wide[,1:3], samples)

# write_csv(MC.Results, "./results/Uncertainty_CEScores_benefits.csv")
# write_csv(MC.Ranks, "./results/Uncertainty_CERanks_benefits.csv")
# write_csv(MC_Samples, "./results/MC_PerfSamples_benefits_constr.csv")
# saveRDS(Benefits_uncrtn, "./results/Benefits_uncrtn_constr.rds") # if doing uncertainty analysis for the complementarity optimization
```

Box plots of CE scores

``` r
MC.CE <- gather(MC.Results, key = MC.Iter, value = CE, 2:ncol(MC.Results))
# MC.CE$Strategy <- factor(MC.CE$Strategy, levels = unique(MC.CE$Strategy))

temp.plot <-
  ggplot(MC.CE, aes(x = Strategy, # for each Ecological group, plot Estimate Type on x-axis 
                    y = CE # and St.Value on y-axis, 
                    ) # and colour the boxplots by estimate type
         ) + 
  geom_boxplot(
    lwd = 0.3 #lwd changes line width
    , fatten = 1 # thickness of median line; default is 2
    , outlier.size = 1 # size of outlier point
    ) + # tell R to display data in boxplot form
  theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
  theme(
    axis.text = element_text(size = 10)
    , axis.line = element_line(size = 1)
    ) + 
  scale_x_discrete(name = "Management strategies"
                   , breaks = newstrat.levels[2:length(newstrat.levels)]
                   , labels = newstrat.levels[2:length(newstrat.levels)]# Give the x-axis variables labels
                   ) + 
  labs(x = "Management strategies"
       , y = "Cost-effectiveness score"
       )

print(temp.plot)
```

![](analyzeBenefitsUncertainty_CEscores_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
# ggsave(filename=paste0("./results/Uncrtn_Benefit_", MC, "R_scores_constr.pdf", sep = ""), temp.plot, width = 180, height = 115, units = "mm")
# ggsave(filename=paste0("./results/Uncrtn_Benefit_", MC, "R_scores_constr.tiff", sep = ""), temp.plot, width = 180, height = 115, units = "mm", dpi = 600)
```

Histogram of CE ranks

``` r
MC.CE_r <- gather(MC.Ranks, key = MC.Iter, value = CE_rank, 2:ncol(MC.Ranks))
# MC.CE_r$Strategy <- as_factor(MC.CE_r$Strategy)

count_ranks <- xyTable(MC.CE_r$Strategy, MC.CE_r$CE_rank)
rank_table <- data.frame(levels(MC.CE_r$Strategy)[count_ranks$x], count_ranks$y, count_ranks$number)
rank_table_sort <- as_tibble(rank_table)
names(rank_table_sort) <- c("Strategy", "CE_rank", "Count")
rank_table_sort <- group_by(rank_table_sort, Strategy) %>%
  filter(Count == max(Count)) %>%
  arrange(desc(CE_rank))

strat.order <- rank_table_sort$Strategy
new.names<-paste0(strat.order)

temp.plot.r <-
  ggplot(MC.CE_r, aes(y = factor(Strategy, levels = new.names)
                      , x = CE_rank
                      , fill = factor(Strategy, levels = new.names)
                      )
         ) +
  geom_density_ridges(stat = "binline", bins = 23, scale = 0.9, draw_baseline = FALSE) +
  theme_ridges(grid = TRUE, center_axis_labels = TRUE) +
  theme(
    legend.position = "none"
    , panel.spacing = unit(0.1, "lines")
    , strip.text = element_text(size = 8)
    , axis.ticks = element_blank()
    , axis.line = element_line(size = 0.3)
    , panel.grid = element_line(size = 0.3)
    ) +
  scale_fill_viridis(option = "plasma", discrete = TRUE) +
  labs(x = "Cost-effectiveness rank"
       , y = "Management strategies"
       ) +
  scale_x_continuous(breaks = c(1:23)
                     , labels = c(1:23) # Give the x-axis variables labels
                     )

print(temp.plot.r)
```

![](analyzeBenefitsUncertainty_CEscores_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
# ggsave(filename=paste0("./results/Uncrtn_Benefit_", MC, "R_Ranks_constr.pdf", sep = ""), temp.plot.r, width = 180, height = 180, units = "mm")
# ggsave(filename=paste0("./results/Uncrtn_Benefit_", MC, "R_Ranks_constr.tiff", sep = ""), temp.plot.r, width = 180, height = 180, units = "mm", dpi = 600)
```
