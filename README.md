# SaintJohnRiver_PTM

Archive of data, code, and results of the Saint John River watershed Priority Threat Management analysis, manuscript submitted 29 Mar 2020 to Conservation Science and Practice. Note that data tables that show individual expert estimates are excluded from this archive, according to the terms of the confidentiality agreement.

## data/

### benefits/
* raw data files of expert estimates (/benefits/expXX.csv, where XX are expert ID numbers) were excluded from this archive to maintain expert confidentiality. A blank table (exp00.csv) is provided instead for reference. 

### cost/SJR_CostFeasibility_Final.xlsx
* raw data on estimated costs of each individual action within a strategy. All identifying information has been removed to maintain expert confidentiality.

### EcolGroupsList.csv
* table with ecological groups (columns) with group names as headers, and the common names of species/communities included in the group (rows)

### SJRStrategyCombinations.csv
* table of strategies (Baseline, S1-S23) and the individual strategies that are part of combination strategies (S17-S23) - used in complementarity analysis

### SpecialCases.csv
* info on number of species in each group that individual experts based their estimates on, where this is less than the total number of species in the group

### CostFeas.csv
* summary table of (original) strategy names (S1-S22, where S22 is 'All Strategies'), total (discounted) costs over 25 years, and estimated feasibility

### SJR_StrategyPerformance_Aggregated.xlsx
* standardized and aggregated expert estimates of the probability of persistence of each group under the baseline and each strategy

### SJR_BenefitsWorksheet_blank.xlsx
* sample spreadsheet used to elicit expert judgment on performance of management strategies

## code/

### manageData/combineTables.R 
* reads individual expert estimate tables and combines them into single Estimates_combined.csv file (not included in this archive)

### manageData/standardizeConfidence.R 
* takes Estimates_combined.csv file and standardizes lower and upper estimates to 80% confidence level
* saves outputs into 2 tables: Estimates_std_long.csv (tidy version) and Estimates_std_wide.csv (same table format as Estimates_combined.csv) (tables not included in this archive)
* also counts the number of expert estimates for each ecological group (data/Estimates_count_group.csv) and for each strategy-group combination (data/Estimates_count_strategy.csv)
* saves a tidy version of Estimates_combined.csv (Estimates_tidy.csv) (not included in this archive)

### manageData/createBoxplots.R 
* uses Estimates_std_long.csv to create plots for expert review & feedback (plots not included in this archive)
  + plot1 are boxplots of the best guess, lower, and upper estimates for each strategy, with separate plots for each ecological group
  + plot2 are pointrange plots of each individual expert estimate for each strategy, with separate plots for each ecological group

### manageData/aggregateEstimates.R 
* uses Estimates_std_wide.csv to
  + calculate the average performance (probability of persistence) under the Baseline scenario (data/ Estimates_aggregated_baseline.csv)
  + calculate benefit of each strategy, _Aggregated benefit = Strategy performance - Baseline performance_, and average the benefit across experts (data/Estimates_aggregated_benefits.csv)
  + weights aggregated benefit by number of species in each ecol group (data/Estimates_aggregated_benefits_groupwtd.csv)
  + calculate the average performance (probability of persistence) under each strategy = _Aggregated benefits + Aggregated baseline_ (data/Estimates_aggregated_performance.csv)
  
### getNewCombos.R
* derives benefit estimates for new combination strategies S22 (All except S6) and S23 (All except S5) from original S22 ('All Strategies'), S5, and S6 estimates. Updates data tables from aggregateEstimates.R and saves as new files with _ _rev_ appended to old filename.

### getBenefitMatrix.R
* uses Estimates_aggregated_baseline.csv and Estimates_aggregated_benefits.csv, and a table of strategy Cost and Feasibility to
  + calculate the expected benefit of each strategy for each ecological group = _Benefit * Feasibility_ (results/Expected_Benefits.csv)
  + calculate the expected performance of each strategy for each ecological group = _Expected_Benefits + Estimates_aggregated_baseline_ (results/Expected_Performance.csv)
  + create a Benefit matrix for use in the optimization using 'best guess' estimates from the Expected_Performance table (results/BestGuess.csv)
  + also creates benefit matrix using 'lower' (results/Lower.csv) and 'upper' (results/Upper.csv) estimates for use in uncertainty analysis of complementarity.

### plotMeanPerformance.R
* can use Estimates_aggregated_performance.csv or Expected_Performance.csv to 
  + create pointrange plots of (unweighted or weighted) standardized mean estimates of probability of persistence (y-axis) for each strategy (x-axis) and for each ecological group (subplots) (results/Expected_Performance_plot.pdf)
  
### optimizeManagement.R
* performs complementarity analysis on Best Guess estimates using consOpt package (https://github.com/ConservationDecisionsLab/consOpt)
* outputs are a table (results/ComplementarityBest.csv) and a plot (results/ComplementarityBest.pdf)
* also performs complementarity analysis using Lower.csv and Upper.csv as benefit matrix (results/ComplementarityLower.csv, results/ComplementarityLower.pdf; results/ComplementarityUpper.csv, results/ComplementarityUpper.pdf)

### calculateCEscore.R
* uses Estimates_aggregated_benefits_groupwtd.csv and a table of strategy Cost and Feasibility to calculate a cost-effectiveness (CE) score _CE = (Benefit*Feasibility)/Cost_ and rank strategies by Benefit, Cost, and CE. Results are saved as results/CostEffectiveness_Scores.csv
* conducts uncertainty analysis on CE scores under cost uncertainty (results/Uncertainty_CEScores_cost.csv), and plots the results (results/Uncrtn_Cost_10000R_Scores.pdf, results/Uncrtn_Cost_10000R_Ranks.pdf)
 
### analyzeBenefitsUncertainty_CEscores.R
* conducts uncertainty analysis on CE scores under uncertainty in benefit estimates (results/Uncertainty_CEScores_benefits.csv), and plots the results (results/Uncrtn_Benefit_10000R_scores_constr.pdf, results/Uncrtn_Benefit_10000R_Ranks_constr.pdf)
* includes script for deriving new S22 and S23 estimates for each individual expert, for use in sampling - applicable for SJR PTM project only
