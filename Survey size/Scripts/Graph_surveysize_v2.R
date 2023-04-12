####################### Survey size graphs scripts #############################

# This script generates the graphs automatically as a png archive #

###### Packages ######

library(dplyr)
library(matrixStats)
library(ggplot2)
library(stringr)


######################
# Data import

network = 'pa'
population = 'pop1'
hid_distrib = 'uniform'
data_path = '~/GitHub/Coronasurveys_paper/Survey size/CSV/'
sim_path = 'Simulation_surveysize'

#simulation_data  = read.csv("~/GitHub/CoronaSurveys_Simulations/R programs/Disjoint and no disjoint ensemble/Simulations and csv analysis/Survey size/CSV/Simulation_surveysize_notdisjoint_207.csv")
#simulation_data_disjoint  =  read.csv("~/GitHub/CoronaSurveys_Simulations/R programs/Disjoint and no disjoint ensemble/Simulations and csv analysis/Survey size/CSV/Simulation_surveysize_disjoint_207.csv")
simulation_data  = read.csv("~/GitHub/Coronasurveys_paper/Survey size/CSV/Simulation_surveysize_notdisjoint_uniform_pa_pop1_2022.csv")
simulation_data_disjoint  =  read.csv("~/GitHub/Coronasurveys_paper/Survey size/CSV/Simulation_surveysize_disjoint_uniform_pa_pop1_2022.csv")

notdisj_path = paste0(data_path,sim_path,'_notdisjoint_',hid_distrib,'_',network,'_',population,'_2022.csv')
disj_path =  paste0(data_path,sim_path,'_disjoint_',hid_distrib,'_',network,'_',population,'_2022.csv')

simulation_data = read.csv(notdisj_path)
simulation_data_disjoint = read.csv(disj_path)


seed_number = "207"

################################################################################
################################################################################

## Not disjoint ##

Nh_real_dataframe = dplyr::select(simulation_data, starts_with("Nh_real"))

Nh_basic_sum_dataframe = dplyr::select(simulation_data, starts_with("Nh_basic_sum"))
#Nh_basicvis_sum_dataframe = dplyr::select(simulation_data, starts_with("Nh_basicvis_sum"))

Nh_basic_mean_dataframe = dplyr::select(simulation_data, starts_with("Nh_basic_mean"))
#Nh_basicvis_mean_dataframe = dplyr::select(simulation_data, starts_with("Nh_basicvis_mean"))

Nh_PIMLE_dataframe    = dplyr::select(simulation_data, starts_with("Nh_PIMLE_"))
#Nh_PIMLEvis_dataframe = dplyr::select(simulation_data, starts_with("Nh_PIMLEvis_"))

Nh_MLE_dataframe     = dplyr::select(simulation_data, starts_with("Nh_MLE_") & !contains("mod"))
#Nh_MLEvis_dataframe  = dplyr::select(simulation_data, starts_with("Nh_MLEvis_"))

Nh_MLE_mod_dataframe     = dplyr::select(simulation_data, starts_with("Nh_MLE_") & contains("mod"))
#Nh_MLEvis_dataframe  = dplyr::select(simulation_data, starts_with("Nh_MLEvis_"))

Nh_MoS_dataframe     = dplyr::select(simulation_data, starts_with("Nh_MoS_"))
#Nh_MoSvis_dataframe  = dplyr::select(simulation_data, starts_with("Nh_MoSvis_"))

Nh_GNSUM_dataframe   = dplyr::select(simulation_data, starts_with("Nh_GNSUM"))



######### Data analysis ##########
# This way of presenting the data allows us to carry out a more detailed analysis
# of each estimator.

Nh_basic_sum_analysis      = data_analysis(Nh_basic_sum_dataframe, Nh_real_dataframe)
#Nh_basicvis_sum_analysis  = data_analysis(Nh_basicvis_sum_dataframe, Nh_real_dataframe)

Nh_basic_mean_analysis     = data_analysis(Nh_basic_mean_dataframe, Nh_real_dataframe)
#Nh_basicvis_mean_analysis = data_analysis(Nh_basicvis_mean_dataframe, Nh_real_dataframe)

Nh_PIMLE_analysis     = data_analysis(Nh_PIMLE_dataframe, Nh_real_dataframe)
#Nh_PIMLEvis_analysis = data_analysis(Nh_PIMLEvis_dataframe, Nh_real_dataframe)

Nh_MLE_analysis     = data_analysis(Nh_MLE_dataframe, Nh_real_dataframe)
#Nh_MLEvis_analysis = data_analysis(Nh_MLEvis_dataframe, Nh_real_dataframe)

Nh_MLE_mod_analysis     = data_analysis(Nh_MLE_mod_dataframe, Nh_real_dataframe)
#Nh_MLEvis_analysis = data_analysis(Nh_MLEvis_dataframe, Nh_real_dataframe)

Nh_MoS_analysis     = data_analysis(Nh_MoS_dataframe, Nh_real_dataframe)
#Nh_MoSvis_analysis = data_analysis(Nh_MoSvis_dataframe, Nh_real_dataframe)

Nh_GNSUM_analysis  = data_analysis(Nh_GNSUM_dataframe, Nh_real_dataframe)



################################################################################
################################################################################

## Disjoint ##

Nh_real_dataframe_disjoint = dplyr::select(simulation_data_disjoint, starts_with("Nh_real"))

Nh_basic_sum_dataframe_disjoint = dplyr::select(simulation_data_disjoint, starts_with("Nh_basic_sum"))
#Nh_basicvis_sum_dataframe_disjoint = dplyr::select(simulation_data_disjoint, starts_with("Nh_basicvis_sum"))

Nh_basic_mean_dataframe_disjoint = dplyr::select(simulation_data_disjoint, starts_with("Nh_basic_mean"))
#Nh_basicvis_mean_dataframe_disjoint = dplyr::select(simulation_data_disjoint, starts_with("Nh_basicvis_mean"))

Nh_PIMLE_dataframe_disjoint    = dplyr::select(simulation_data_disjoint, starts_with("Nh_PIMLE_"))
#Nh_PIMLEvis_dataframe_disjoint = dplyr::select(simulation_data_disjoint, starts_with("Nh_PIMLEvis_"))

Nh_MLE_dataframe_disjoint     = dplyr::select(simulation_data_disjoint, starts_with("Nh_MLE_") & !contains("mod"))
#Nh_MLEvis_dataframe_disjoint  = dplyr::select(simulation_data_disjoint, starts_with("Nh_MLEvis_"))

Nh_MLE_mod_dataframe_disjoint     = dplyr::select(simulation_data_disjoint, starts_with("Nh_MLE_") & contains("mod"))
#Nh_MLEvis_dataframe_disjoint  = dplyr::select(simulation_data_disjoint, starts_with("Nh_MLEvis_"))

Nh_MoS_dataframe_disjoint     = dplyr::select(simulation_data_disjoint, starts_with("Nh_MoS_"))
#Nh_MoSvis_dataframe_disjoint  = dplyr::select(simulation_data_disjoint, starts_with("Nh_MoSvis_"))

Nh_GNSUM_dataframe_disjoint  = dplyr::select(simulation_data_disjoint, starts_with("Nh_GNSUM"))



######### Data analysis ##########
# This way of presenting the data allows us to carry out a more detailed analysis
# of each estimator.

Nh_basic_sum_analysis_disjoint      = data_analysis(Nh_basic_sum_dataframe_disjoint, Nh_real_dataframe)
#Nh_basicvis_sum_analysis_disjoint  = data_analysis(Nh_basicvis_sum_dataframe_disjoint, Nh_real_dataframe)

Nh_basic_mean_analysis_disjoint     = data_analysis(Nh_basic_mean_dataframe_disjoint, Nh_real_dataframe)
#Nh_basicvis_mean_analysis_disjoint = data_analysis(Nh_basicvis_mean_dataframe_disjoint, Nh_real_dataframe)

Nh_PIMLE_analysis_disjoint     = data_analysis(Nh_PIMLE_dataframe_disjoint, Nh_real_dataframe)
#Nh_PIMLEvis_analysis_disjoint = data_analysis(Nh_PIMLEvis_dataframe_disjoint, Nh_real_dataframe)

Nh_MLE_analysis_disjoint     = data_analysis(Nh_MLE_dataframe_disjoint, Nh_real_dataframe)
#Nh_MLEvis_analysis_disjoint = data_analysis(Nh_MLEvis_dataframe_disjoint, Nh_real_dataframe)

Nh_MLE_mod_analysis_disjoint     = data_analysis(Nh_MLE_mod_dataframe_disjoint, Nh_real_dataframe)
#Nh_MLEvis_analysis_disjoint = data_analysis(Nh_MLEvis_dataframe_disjoint, Nh_real_dataframe)

Nh_MoS_analysis_disjoint     = data_analysis(Nh_MoS_dataframe_disjoint, Nh_real_dataframe)
#Nh_MoSvis_analysis_disjoint = data_analysis(Nh_MoSvis_dataframe_disjoint, Nh_real_dataframe)

Nh_GNSUM_analysis_disjoint   = data_analysis(Nh_GNSUM_dataframe_disjoint, Nh_real_dataframe)


plotSurveySize = function(method, method_plot_name){
  graph_data = data.frame(data = simulation_data$data)
  
  graph_data = cbind(graph_data, Nh_basic_sum =  Nh_basic_sum_analysis[[method]])
  #graph_data = cbind(graph_data, Nh_basicvis_sum =  Nh_basicvis_sum_analysis[[method]])
  
  graph_data = cbind(graph_data, Nh_basic_mean =  Nh_basic_mean_analysis[[method]])
  #graph_data = cbind(graph_data, Nh_basicvis_mean =  Nh_basicvis_mean_analysis[[method]])
  
  graph_data = cbind(graph_data, Nh_PIMLE =  Nh_PIMLE_analysis[[method]])
  #graph_data = cbind(graph_data, Nh_PIMLEvis =  Nh_PIMLEvis_analysis[[method]])
  
  graph_data = cbind(graph_data, Nh_MLE =  Nh_MLE_analysis[[method]])
  #graph_data = cbind(graph_data, Nh_MLEvis =  Nh_MLEvis_analysis[[method]])
  
  graph_data = cbind(graph_data, Nh_MLE_mod =  Nh_MLE_mod_analysis[[method]])
  #graph_data = cbind(graph_data, Nh_MLEvis =  Nh_MLEvis_analysis[[method]])
  
  graph_data = cbind(graph_data, Nh_MoS =  Nh_MoS_analysis[[method]])
  #graph_data = cbind(graph_data, Nh_MoSvis =  Nh_MoSvis_analysis[[method]])
  
  graph_data = cbind(graph_data, Nh_GNSUM  =  Nh_GNSUM_analysis[[method]])
  
  
  # Graph creation
  
  plot_name = str_c("Simulation_surveysize_", seed_number, "_notdisjoint_",hid_distrib,'_',network,'_',
  population,'_2022',method,".png")
  sub_title = str_c("Not disjoint populations plot, seed ", seed_number)
  
  # png(filename = plot_name,
  #     width = 1000, height = 600)
  
  
  p = ggplot(graph_data) + 
    geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
    #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
    
    geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
    #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
    
    #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
    geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
    
    geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
    #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) +
    
    geom_line(aes(x = data, y =  Nh_MLE_mod, col = "Nh_MLE_mod")) + 
    #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) +
    
    geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
    #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
    
    geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
    
    
    scale_color_discrete("Legend") + 
    labs(title = "Simulations based on the survey size",
         subtitle = sub_title,
         x = "Survey size",
         y = method_plot_name)
  ggsave(plot_name, plot = p)
  
  # dev.off()
}

y_names = c("Absolute Error", " Mean Absolute Error", "Bias", "Standard Deviation", "Median")
method_code = c("abserror", "mse"   ,   "bias"    , "sd"    ,   "median")

for (i in 1:length(y_names)) {
  plotSurveySize(method_code[i],y_names[i])
}
