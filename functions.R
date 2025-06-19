
# all functions for just Pre-MEDS data as it appears in the class of 2023 report ----
source(here::here("R", "only_pre_fxns.R"))

# create ggplot theme for all visualizations ----
source(here::here("R", "meds_theme.R"))

# initial qualtrics data cleaning / parsing ----
source(here::here("R", "clean_PLO_data.R"))

# clean & plot data for Qs 1-2 ----
source(here::here("R", "q1_os.R"))
source(here::here("R", "q2_store_data.R"))

# clean & plot data for Qs 3-31 ----
source(here::here("R", "clean_freq_rank_data.R"))
source(here::here("R", "plot_freq_use_data.R"))
source(here::here("R", "plot_rank_data.R"))
source(here::here("R", "plot_correct_answer_comparison.R"))
source(here::here("R", "q8_workflow_satisfaction.R"))
source(here::here("R", "q16_median_mode.R"))
source(here::here("R", "q17_linear_regression.R"))
source(here::here("R", "q18_prob_dist.R"))
source(here::here("R", "q19_functions.R"))
source(here::here("R", "q20_env_mod.R"))
source(here::here("R", "q21_spatial_data.R"))
source(here::here("R", "q22_remote_sensing.R"))
source(here::here("R", "q23_map_projections.R"))
source(here::here("R", "q24_reflectance.R"))
source(here::here("R", "q25_ML.R"))
source(here::here("R", "q26_TrainValidSplit.R"))
source(here::here("R", "q29_30_data_viz.R"))
source(here::here("R", "q31_lang.R"))