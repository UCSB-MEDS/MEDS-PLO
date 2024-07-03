clean_PLO_data <- function(PLO_data_raw){
  
  PLO_data_raw |> 
    filter(!row_number() %in% c(1, 2)) |> 
    clean_names() |> 
    mutate(#start_date = as_datetime(start_date, origin = "1981-01-01"),
      #start_date = as_datetime(start_date)
      progress = as.numeric(progress),
      duration_in_seconds = as.numeric(duration_in_seconds),
      finished = as.character(finished),
      operating_system = as_factor(operating_system),
      raw_data = as.character(raw_data),
      small_program = as.character(small_program),
      find_help_online = as.character(find_help_online),
      overcoming_problems = as.character(overcoming_problems),
      confident_programmer = as.character(confident_programmer),
      easier_analyses = as.character(easier_analyses),
      increase_efficiency = as.character(increase_efficiency),
      median = as.numeric(median),
      mode = as.numeric(mode),
      linear_regression = as.character(linear_regression),
      microplastics_lr = as.character(microplastics_lr),
      prob_dist = as.character(prob_dist),
      term_function = as.character(term_function),
      writing_functions = as.character(writing_functions),
      fxn_output = as.character(fxn_output),
      spatial_data = as.character(spatial_data),
      remote_sense_comfort = as.character(remote_sense_comfort),
      map_proj_comfort = as.character(map_proj_comfort), 
      reflec_spec = as.character(reflec_spec),
      sup_vs_unsup_learn = as.character(sup_vs_unsup_learn),
      implemented_algo = as.character(implemented_algo),
      ml_div_data = as.character(ml_div_data),
      train_valid_split = as.character(train_valid_split),
      data_justice = as.character(data_justice),
      bias = as.character(bias),
      data_viz_programming = as.character(data_viz_programming),
      sc0 = as.numeric(sc0)
    )
  
}