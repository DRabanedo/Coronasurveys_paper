sm_df = Simulation_visibilityfactor_disjoint_uniform_207

dplyr::select(sm_df, hp)
colnames(sm_df)
a = 'Nh_real_1'
dplyr::select(sm_df, all_of(a))


gen_graph_df = function(simulation_data, magn){
  # sm_df: dataframe to study
  # magn: choose between abserror, mse, bias, sd and median
  
  n_row  = nrow(simulation_data)
  df_ref = dplyr::select(simulation_data, starts_with("Nh_real"))
    
  graph_df = data.frame(data = 1:n_row)
  
  
  
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
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_mean'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basic_mean'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_mean'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_mean'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with(c('Nh_MLE_')))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLEvis'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  return(graph_df)
}
gen_graph_df(sm_df, 'median')

dplyr::select(sm_df, starts_with("Nh_MLE"))
