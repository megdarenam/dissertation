# dissertation
Code and results for master's dissertation. In case of doubt, please send an email to "megdarenam@gmail.com".

# Final_codes
The folder containing the codes used to produce the forecasts, boxplots, MSFE and MCS procedure statistics, and tables containing the model rankings for each statistic.

## autometrics.ox
Oxmetrics code to generate forecasts for the AR 13 model with autometrics regularization.

## boxplots_generator.R
R code to generate the boxplots used in the dissertation.

## generator.R
R code used for the Model Confidence Set procedure and for the MSFE.

## tables_generator.R
Code used to generate tables with model rankings for MSFE and MCS statistics.

## tese.R
Code used to estimate the models and produce the forecasts.

## running_code.R
Code to run the following scripts using a window of 60, 90, and 120 observations for estimation:
1) Tese.R
2) generator.R
3) boxplots_generator.R
4) tables_generator.R

## aggregating_window_sizes.R
Code used to aggregate the results from the 3 estimation windows. It requires manual modification. Run it before "ranking_generator_all_window_sizes.R".

## ranking_generator_all_window_sizes.R
Code used to generate the MCS using models estimated for 3 different window sizes. Produces the results analyzed in section 5.3 of the dissertation.

# window_size_60
Folder containing the boxplots used in the dissertation and the tables with rankings for MSFE and MCS statistics using window size of length 60.

## Boxplots_MSFE
Folder containing the boxplots of the MSFE statistics. "window_size_60_MSFE_P.1" is the boxplot for period 1 (see page 24 of the dissertation) using 60 observations as estimation window, "window_size_60_MSFE_P.2" is the boxplot for period 2,...

## ranking_tables_periods
Folder containing the boxplots used in the dissertation and the tables with rankings for MSFE and MCS statistics. There are two types of tables:

1) MCS_STATS_P.X_ranking: Data Frame containing the rankings based on the p-values of MCS procedure for period X (MCS_STATS.P.1_ranking for period 1, MCS_STATS.P.2_ranking for period 2, ..., MCS_STATS.P.7_ranking for period 7) for each forecast horizon (columns). It's ordered from the best model to the worst models (statistic = NA, i.e., it is not contained in the superior set with alpha equal or greater than 0.25). 
2) MCS_STATS_P.X_ranking: Data Frame containing the rankings based on the MSFE for each forecast horizon. Same logic as before.

# window_size_90
Folder containing the boxplots used in the dissertation and the tables with rankings for MSFE and MCS statistics using window size of length 90.

# window_size_60
Folder containing the boxplots used in the dissertation and the tables with rankings for MSFE and MCS statistics using window size of length 120.

# Section_5_3
Folder containing the figures used in section 5.3, as well as tables with rankings using the P-VALUE of the MCS procedure.

## Folders period3, period4, period5, period6, period7
Within the folders period3, period4, period5, period6, and period7, we have the figures used in the analysis 5.3. They contain the MSFE of the best model (lowest MSFE) for each window size and for each time horizon. These figures are identified as "[...]_MSFE_best_models". The figures identified as "[...]_MSFE_second_models" and "[...]_MSFE_third_models" are the same as before, but now using the second-best model (second-lowest MSFE) and the third-best model (third-lowest MSFE) for each window size. 
"Ranking_tables.RData" contains the rankings of the models by P-Value of the MCS, as done in "ranking_tables_periods", but using all models estimated by the 3 different window sizes.

# Data
It contains the data used in "tese.R" for the Brazilian industrial production, in addition to the forecasts generated in "autometrics.ox".

# forecasts
It contains the results of "tese.R".

# Workspaces
The results of "generator.R".

