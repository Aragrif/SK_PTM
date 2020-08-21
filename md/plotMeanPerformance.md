Plot Averaged Performance
================
Abbey Camaclang
10 July 2019

Creates pointrange plots of the standardized mean estimates of probability of persistence (y-axis) for each strategy (x-axis) and for each ecological group (subplots).

It can be used to plot mean estimates that are either unweighted (**Estimates\_aggregated\_performance.csv**) or weighted by feasibility (**Expected\_Performance.csv** from *getBenefitMatrix.r*)

Load packages

``` r
library(tidyverse)
library(cowplot)
library(gridExtra)
library(here)
```

Prepare data for plotting

``` r
# Specify which file to read
# datafolder <- here("data")
resultfolder <- here("results")

weighted <- 1

if (weighted == 0) {
  est.file <- paste0(resultfolder, "/Estimates_aggregated_performance") # unweighted
} else {
  if (weighted == 1) {
    est.file <- paste0(resultfolder, "/Expected_Performance") # feasibility weighted
  } else {
    stop("Must specify if estimates are weighted by feasibility (1) or not (0)")
  }
}

exp.pop <- read_csv(paste0(est.file, ".csv", sep = ""))
grp.levels <- unique(exp.pop$Ecological.Group)
exp.pop$Ecological.Group <- factor(exp.pop$Ecological.Group, levels = grp.levels)

# Organize data into correct format for plotting
exp.pop.long <- exp.pop %>%
  gather(., key = "Estimate", value = "St.Value", -Ecological.Group) %>%
  separate(., Estimate, c("Est.Type", "Strategy"), sep = "[_]", remove = FALSE) %>%
  mutate(Strategy = paste0("S", Strategy)) %>%
  mutate(Strategy = str_replace(Strategy, "SNA", "Baseline")) %>%
  mutate(Strategy = factor(Strategy, levels = unique(Strategy)))

plot.data <- select(exp.pop.long, -Estimate) %>%
  spread(., Est.Type, St.Value)

strat.levels <- levels(plot.data$Strategy)

# write_csv(plot.data, paste0(est.file, "_tidy.csv", sep = ""))

base.data <- plot.data[which(plot.data$Strategy=="Baseline"),]
plot.data.nobase <- plot.data[which(plot.data$Strategy!="Baseline"),]

# Renaming for plot readability
plot.data.nobase$Ecological.Group<-as.character(plot.data.nobase$Ecological.Group)
plot.data.nobase$Ecological.Group[which(str_detect(plot.data.nobase$Ecological.Group, "Forest Openings and Young Forest Species")==1)] <- "Forest Openings or Young Forest Spp"
plot.data.nobase$Ecological.Group<-as_factor(plot.data.nobase$Ecological.Group)

base.data$Ecological.Group<-as.character(base.data$Ecological.Group)
base.data$Ecological.Group[which(str_detect(base.data$Ecological.Group, "Forest Openings and Young Forest Species")==1)] <- "Forest Openings or Young Forest Spp"
base.data$Ecological.Group<-as_factor(base.data$Ecological.Group)
```

Plot mean estimates by strategy for all groups

``` r
temp.plot2 <- 
  ggplot(plot.data.nobase, aes(x = Strategy, y = Wt.Best.guess) ) +
  geom_pointrange(aes(ymin = Wt.Lower, ymax = Wt.Upper)) +
  geom_hline(aes(yintercept = Wt.Best.guess), base.data, colour = "blue") +
  geom_hline(aes(yintercept = Wt.Lower), base.data, colour = "blue", lty = "dashed") +
  geom_hline(aes(yintercept = Wt.Upper), base.data, colour = "blue", lty = "dashed") +
  theme_cowplot() +  # minimalist theme from cowplot package
  theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # top, right, bottom and left margins around the plot area
        panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), # adjust space between y-axis numbers and y-axis label
        axis.text.x = element_text(size = 10, angle = 60, hjust = 0.5, vjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0)
  ) +
  facet_wrap( ~ Ecological.Group, nrow = 3, ncol = 3) +  # create a separate panel for each ecological group
  scale_x_discrete(breaks = strat.levels, labels = c("B", 1:23) ) +
  labs(x = "Strategies",
       y = "Probability of persistence (%)"
       # , title = "Mean estimates, standardized to 80% confidence level" 
       #caption = "Figure 1. Estimated probability of persistence of each ecological group under the Baseline scenario (B) and each of the management strategies (1 - 22). 
       #Values are based on expert best guess and lower and upper estimates (standardized to 80% confidence level), averaged over number of experts who 
       #provided estimates for the strategy and ecological group."
  ) +
  ylim(0, 100) 

print(temp.plot2)
```

![](plotMeanPerformance_files/figure-markdown_github/unnamed-chunk-3-1.png)

Save plot as pdf file

``` r
# ggsave(filename=paste0(est.file, "_plot.pdf", sep = ""), temp.plot2, width = 11, height = 8.5, units = "in")
```
