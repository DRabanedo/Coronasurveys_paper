Simulation_networkneighbours_disjoint_uniform_sw_2022 <- read.csv("~/GitHub/Coronasurveys_paper/Number of neighbours/Scripts/Simulation_networkneighbours_disjoint_uniform_sw_2022.csv")
sm_df = Simulation_networkneighbours_disjoint_uniform_sw_2022


gen_graph_df = function(simulation_data, magn){
  # sm_df: dataframe to study
  # magn: choose between abserror, mse, bias, sd and median
  # New magnitudes can be implemented and uploaded
  
  n_row  = nrow(simulation_data)
  df_ref = dplyr::select(simulation_data, starts_with("Nh_real"))
    
  graph_df = data.frame(data = 1:n_row)
  
  # Duplicate names 
  
  df_buc = dplyr::select(simulation_data, starts_with(c('Nh_MLE_mod_')))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmod'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_modvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmodvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  # Duplicate elimination
  simulation_data = dplyr::select(simulation_data,  -starts_with(c('Nh_MLE_mod')))
 
   # Análisis
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basic_sum'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_sum'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basic_mean'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_mean'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoS_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MoS'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoSvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MoSvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_PIMLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_PIMLEvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_GNSUM_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_GNSUM'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEO_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_TEO'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEOvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_TEOvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zheng_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Zheng'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zhengvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Zhengvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Direct'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Direct'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  return(graph_df)
}

sm_df
gen_graph_df(sm_df, 'sd')
