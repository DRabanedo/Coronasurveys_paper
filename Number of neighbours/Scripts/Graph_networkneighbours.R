################## Number of neighbors graphs scripts ##########################

# This script generates the graphs automatically as a png archive #


###### Packages ######

library(dplyr)
library(matrixStats)
library(ggplot2)
library(stringr)

######################
# Data import

setwd("~/GitHub/Coronasurveys_paper/Number of neighbours/Graphs")

simulation_data_disjoint = read.csv("~/GitHub/Coronasurveys_paper/Number of neighbours/CSV/Simulation_networkneighbours_disjoint_uniform_sw_2022.csv")
simulation_data = read.csv("~/GitHub/Coronasurveys_paper/Number of neighbours/CSV/Simulation_networkneighbours_notdisjoint_uniform_sw_2022.csv")

seed_number = "2022"

simulation_data$data = simulation_data$data*2
simulation_data_disjoint$data = simulation_data_disjoint$data*2

################################################################################

####### Graph representation #######
# The graphic representation allows us to compare the different results obtained
# by each one of the estimators

##### Absolute error #####

## Not disjoint ##


#Dataframe creation

graph_data_abserror = gen_graph_df(simulation_data, 'abserror')

# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_notdisjoint_abserror.png")
sub_title = str_c("Not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)


ggplot(graph_data_abserror) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) +
  
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Mean Absolute Error")

dev.off()



## Disjoint ##

#Dataframe creation

graph_data_abserror_disjoint = gen_graph_df_disjoint(simulation_data_disjoint, 'abserror')


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_disjoint_abserror.png")
sub_title = str_c("Disjoint populations plot, seed ", seed_number)


png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_abserror_disjoint) + 
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) + 
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
      x = "Number of neighbours",
      y = "Mean Absolute Error")


dev.off()




## Not disjoint & disjoint ##

# Dataframe creation

graph_data_abserror_total = cbind(graph_data_abserror, dplyr::select(graph_data_abserror_disjoint, -data & -Nh_real) )


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_total_abserror.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_abserror_total) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +   

  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
           x = "Number of neighbours",
           y = "Mean Absolute Error")


dev.off()



################################################################################


##### Mean of Squares Error (MSE) #####

## Not disjoint ##

#Dataframe creation

graph_data_mse = gen_graph_df(simulation_data, 'mse')


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_notdisjoint_mse.png")
sub_title = str_c("Not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_mse) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Mean of Squares Error")

dev.off()



## Disjoint ##

#Dataframe creation
graph_data_mse_disjoint = gen_graph_df_disjoint(simulation_data_disjoint, 'mse')


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_disjoint_mse.png")
sub_title = str_c("Disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_mse_disjoint) + 
  
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +  
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Mean of Squares Error")


dev.off()



## Not disjoint & disjoint ##

# Dataframe creation

graph_data_mse_total = cbind(graph_data_mse, dplyr::select(graph_data_mse_disjoint, -data & -Nh_real) )


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_total_mse.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_mse_total) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +   
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Mean of Squares Error")


dev.off()



################################################################################

###### Bias analysis ######


## Not disjoint ##

# Dataframe creation

graph_data_bias = gen_graph_df(simulation_data, 'bias')


plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_notdisjoint_bias.png")
sub_title = str_c("Not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)


ggplot(graph_data_bias) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) +
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Hidden population estimate")

dev.off()



## Disjoint ##

#Dataframe creation

graph_data_bias_disjoint = gen_graph_df_disjoint(simulation_data_disjoint, 'bias')


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_disjoint_bias.png")
sub_title = str_c("Disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_bias_disjoint) + 
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) +
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Hidden population estimate")


dev.off()




## Not disjoint & disjoint ##

# Dataframe creation

graph_data_bias_total = cbind(graph_data_bias, dplyr::select(graph_data_bias_disjoint, -data  & -Nh_real) )


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_total_bias.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_bias_total) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) + 
  
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Hidden population estimate")


dev.off()





################################################################################

#### Standard deviation analysis ####

#Dataframe creation


## Not disjoint ##

#Dataframe creation

graph_data_sd = gen_graph_df(simulation_data, 'sd')


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_notdisjoint_sd.png")
sub_title = str_c("Not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)


ggplot(graph_data_sd) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Standard deviation")

dev.off()



## Disjoint ##

#Dataframe creation

graph_data_sd_disjoint = gen_graph_df_disjoint(simulation_data_disjoint, 'sd')

# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_disjoint_sd.png")
sub_title = str_c("Disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_sd_disjoint) + 
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Standard deviation")


dev.off()




## Not disjoint & disjoint ##

#Dataframe creation

graph_data_sd_total = cbind(graph_data_sd, dplyr::select(graph_data_sd_disjoint, -data &  -Nh_real) )


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_total_sd.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_sd_total) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +  
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Standard deviation")


dev.off()




################################################################################

#### Median analysis ####

#Dataframe creation


## Not disjoint ##

#Dataframe creation

graph_data_median = gen_graph_df(simulation_data, 'median')


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_notdisjoint_mediana.png")
sub_title = str_c("Not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)


ggplot(graph_data_median) + 
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) +
  
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Median")

dev.off()



## Disjoint ##

#Dataframe creation

graph_data_median_disjoint = gen_graph_df_disjoint(simulation_data_disjoint, 'median')

# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_disjoint_mediana.png")
sub_title = str_c("Disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_median_disjoint) + 
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) +
  
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +  
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Median")


dev.off()




## Not disjoint & disjoint ##

# Dataframe creation

graph_data_median_total = cbind(graph_data_median, dplyr::select(graph_data_median_disjoint, -data & -Nh_real) )


# Graph creation

plot_name = str_c("Simulation_neighboursnumber_", seed_number, "_total_mediana.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_median_total) + 
  
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) +
  
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis, col = "Nh_PIMLEvis")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE, col = "Nh_PIMLE")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE, col = "Nh_MLE")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis, col = "Nh_MLEvis")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod, col = "Nh_MLEmod")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis, col = "Nh_MLEvismod")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS, col = "Nh_MoS")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis, col = "Nh_MoSvis")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM, col = "Nh_GNSUM")) + 
  
  geom_line(aes(x = data, y =  Nh_TEO, col = "Nh_TEO")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis, col = "Nh_TEOvis")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng, col = "Nh_Zheng")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis, col = "Nh_Zhengvis")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_sum_disjoint, col = "Nh_basic_sum_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum_disjoint, col = "Nh_basicvis_sum_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean_disjoint, col = "Nh_basic_mean_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean_disjoint, col = "Nh_basicvis_mean_disjoint")) +
  
  #geom_line(aes(x = data, y =  Nh_PIMLEvis_disjoint, col = "Nh_PIMLEvis_disjoint")) + 
  geom_line(aes(x = data, y =  Nh_PIMLE_disjoint, col = "Nh_PIMLE_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLE_disjoint, col = "Nh_MLE_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEvis_disjoint, col = "Nh_MLEvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MLEmod_disjoint, col = "Nh_MLEmod_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MLEmodvis_disjoint, col = "Nh_MLEvismod_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_MoS_disjoint, col = "Nh_MoS_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_MoSvis_disjoint, col = "Nh_MoSvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_GNSUM_disjoint, col = "Nh_GNSUM_disjoint")) +   
  
  geom_line(aes(x = data, y =  Nh_TEO_disjoint, col = "Nh_TEO_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_TEOvis_disjoint, col = "Nh_TEOvis_disjoint")) + 
  
  geom_line(aes(x = data, y =  Nh_Zheng_disjoint, col = "Nh_Zheng_disjoint")) + 
  #geom_line(aes(x = data, y =  Nh_Zhengvis_disjoint, col = "Nh_Zhengvis_disjoint")) +   
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the number of neighbours",
       subtitle = sub_title,
       x = "Number of neighbours",
       y = "Median")


dev.off()

