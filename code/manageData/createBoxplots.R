#' ---
#' title: "Plot Standardized Benefit Estimates"
#' author: "Adapted for the SJR PTM by Griffy J. Vigneron"
#' date: "12 October 2022"
#' output: github_document
#' ---

#' Based on *Boxplot_script.R* from FRE PTM project 
#' This script creates two plots for each Ecological Group:  
#' 1) boxplots of the best guess, lower, and upper estimates for each Strategy from all Experts;  
#' 2) pointrange plots showing the best guess, lower and upper estimates of each Expert for each Strategy.  
#' NOTE: need to create 2 subfolders named 'boxplots' and 'pointrange' where plots will be saved.  
#'
#' It requires output from *standardizeConfidence.R*, which standardizes the individual estimates to 80% confidence level
#' and saves results as **Estimates_std_long.csv**
#' 
#' Load packages
#+ message = FALSE, warning = FALSE
library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(here)

#' Read in data from benefits aggregation
#+ warning = FALSE, message = FALSE
#+ 
setwd('D:/UBC/R_code/SKAnalysis')

rlong.std <- read.csv("./results/Estimates_std_long2.csv")
#subfolder <- here("results")
#rlong.std <- read_csv(paste0(subfolder, "/Estimates_std_long.csv")) # use read_csv to make sure factors read in as character

#Add 'Strategy' Column
#rlong.std$Strategy <- as.numeric(gsub(".*?([0-9]+).*", "\\1", rlong.std$Estimate))
#rlong.std$Strategy <- rlong.std$Strategy %>% replace_na(0)

#' Prepare data for plotting 
strat.levels <- unique(rlong.std$Strategy)
grp.levels <- unique(rlong.std$Ecological.Group)
est.levels <- c("Lower", "Best.Guess", "Upper")
expcode <- unique(rlong.std$Expert) 

# Order the strategies to plot in the desired order
rlong.std$Strategy <- factor(rlong.std$Strategy, levels = strat.levels)

# Subset to remove confidence estimates
rlong.std <- subset(rlong.std, Est.Type %in% c("Best.Guess", "Lower", "Upper"))
rlong.std$Est.Type <- factor(rlong.std$Est.Type, levels = est.levels)

#' ## Boxplots
#' Plot group estimates as boxplots and save as .pdf
for (j in seq_along(expcode)) {
  
  grp.list <- list()
  
  for (i in seq_along(grp.levels)) {
    
    temp.grpdata <- subset(rlong.std, Ecological.Group == grp.levels[i])
    temp.plot <-
      ggplot(temp.grpdata, aes(x = Est.Type, # for each Ecological group, plot Estimate Type on x-axis 
                               y = Value, # and St.Value on y-axis, 
                               fill = Est.Type) # and colour the boxplots by estimate type
             ) + 
      geom_boxplot() + # tell R to display data in boxplot form
      #Add these lines to create plots that anonymously highlight estimates from each individual expert
      #geom_point(data = subset(temp.grpdata, Expert == expcode[j]), # plot indiv. expert's estimates as blue datapoints on top of the boxplots
      #           aes(x = Est.Type, y = Value), 
      #           color = 'blue'
      #           ) +
      theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
      theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # adjust margins around the outside of the plot (top, right, bottom, left)
            panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
            axis.title.y = element_text(margin = margin(t = 0, 
                                                        r = 10,
                                                        b = 0,
                                                        l = 0) # adjust space between y-axis numbers and y-axis label
                                        ),
            plot.caption = element_text(size = 10, hjust = 0)
            ) + 
      facet_wrap( ~ Strategy, nrow = 3) +  # create a separate panel of estimates for each management strategy
      scale_x_discrete(name = "",
                       breaks = c("Lower", "Best.Guess", "Upper"),
                       labels = c("L", "B", "U") # Give the x-axis variables shortened labels
                       ) + 
      scale_fill_manual(values = c("white", "gray80", "white"), # Assign colours to each type of estimate and don't show a legend
                        guide = "none" 
                        ) + 
      labs(x = "", 
           y = "Probability of persistence (%)", 
           title = paste(grp.levels[i]),
           caption = paste0(
             "Figure ", i, ". Boxplots summarizing the distribution of the lower (L), best guess (B), and upper (Upper) expert estimates of the probability of persistence 
             of ", grp.levels[i], " under the Baseline scenario and each of the management strategies (S1 - S17). The thick horizontal lines indicate the median estimate, while the surrounding box shows the 
             interquartile range. Any outliers are shown as points beyond the plot whiskers. Your individual estimates are shown in blue.")
           ) +  
      ylim(0, 100) # set the y-axis limits from 0-100

    grp.list[[i]] <- temp.plot
    
  }
  
  # To save all plots as a single .pdf: 
  plot1 <- marrangeGrob(grp.list, nrow = 1, ncol = 1, top = NULL) # arranges plots for saving to single pdf file, one plot per page
   ggsave(filename = paste0("Exp", expcode[j], "_rev.pdf", sep=''), 
          plot1, 
          path = "./data/plots/boxplots", 
          width = 11, height = 8.5, units = "in")
  
}

print(temp.plot)

#' ## Pointrange plots
#' Plots each expert estimate separately (x-axis = Expert, y-axis point = Best guess, range = lower->upper)

# Rearrange table so estimates for each group * strategy are on the same row
rlong.sub2 <- rlong.std[,c(1,2,4,5,6)] 
rlong.std.wide <- spread(rlong.sub2,key=Est.Type,value=Value)
rlong.std.wide$Expert<-as.factor(rlong.std.wide$Expert)

#rlong.std.wide <- read.csv("./results/Estimates_std_wide.csv")

# Create plots
for (j in seq_along(expcode)) {
  
  grp.list <- list()
  
  for (i in seq_along(grp.levels)) {
  
    temp.expdata <- subset(rlong.std.wide, Ecological.Group == grp.levels[i]) %>% #or rlong.std.wide
      mutate(expi = ifelse(Expert == expcode[j], T, F)) # this column allows for highlighting individual expert estimates
    
    temp.plot2 <-
      ggplot(temp.expdata, aes(x = Expert, # using the data Ecological group, plot Experts on X-axis
                               y = Best.Guess, # and corresponding standardized estimates on y-axis
                               #color = expi, # use this if highlighting individual expert responses
                               #color = Expert # use this if plotting all experts together (not highlighted)
                               )
             ) +  
      geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
      theme_cowplot() +  # use the theme "cowplot" for the plots, which is a nice minimalist theme
      theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # adjust margins around the outside of the plot
            panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot (spacing of 1)
            axis.title.y = element_text(margin = margin(t = 0,
                                                        r = 10,
                                                        b = 0,
                                                        l = 0)), # adjust space between y-axis numbers and y-axis label
            axis.text.x = element_text(size=10),
            legend.justification=c(1,0), legend.position=c(0.98,-0.05), # repositions legend box
            plot.caption = element_text(size = 10, hjust = 0)
            ) +  
      scale_color_manual(values = c("grey", "blue"), guide = "none") + # turn this off if ploting all experts together
      # scale_color_brewer(palette='Paired') + # changes color palette
      facet_wrap( ~ Strategy, nrow = 3) +  # create a separate panel of estimates for each management strategy
      labs(x = "Experts",
           y = "Probability of persistence (%)",
           title = paste(grp.levels[i]),
           caption = paste0(
             "Figure ", i, ". Plots of each expert estimate of the probability of persistence of ", grp.levels[i], " under the Baseline scenario and each of the 
             management strategies (S1 - S17). Each point indicates the best guess of one expert. Your individual estimates are plotted in blue.")
           ) +
      ylim(0, 100) # set the y-axis limits from 0-100

    grp.list[[i]] <- temp.plot2

    # To save plots as a .pdf, one file per ecological group:
    # ggsave(temp.plot2, file=paste0(grp.levels[i],"_byExp.pdf", sep=''), path = "./data/plots/pointrange/", width = 10, height = 8, units = "in")
  
  }
  
  # To save all plots as a single .pdf: 
  plot2 <- marrangeGrob(grp.list, nrow = 1, ncol = 1, top = NULL) # arranges plots for saving to single pdf file, one plot per page
  ggsave(
  # #filename = "IndivEstimates.pdf", # if plotting all estimates without highlighting
     filename = paste0("Indiv_Exp", expcode[j], "_rev.pdf", sep=''), # if highlighting individual expert estimates
     plot2, 
     path = "./data/plots/pointrange", 
     width = 11, height = 8.5, units = "in"
     )
  
}

print(temp.plot2)
