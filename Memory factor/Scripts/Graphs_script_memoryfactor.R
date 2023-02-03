#################### Memory factor graphs scripts ##############################

# This script generates the graphs automatically as a png archive #

######################
# Data import

setwd("~/GitHub/Coronasurveys_paper/Memory factor/Graphs")
simulation_data = read.csv("~/GitHub/Coronasurveys_paper/Memory factor/CSV/Simulations_memoryfactor_uniform_sw_2022.csv", header=T)

seed_number = "207"

################################################################################
################################################################################

# Absolute error #

# Dataframe creation #

graph_data_abserror = gen_graph_df(simulation_data, 'abserror')


# Graph creation

plot_name = str_c("Simulation_memoryfactor_", seed_number, "_notdisjoint_abserror.jpg")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_abserror) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the memory factor",
       subtitle = sub_title,
       x = "Memory factor",
       y = "Mean Absolute Error")

dev.off()


################################################################################


##### Mean of Squares Error (MSE) #####

# Dataframe creation #

graph_data_mse = gen_graph_df(simulation_data, 'mse')


# Graph creation

plot_name = str_c("Simulation_memoryfactor_", seed_number, "_notdisjoint_mse.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_mse) + 
  
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the subpopulation memory factor",
       subtitle = sub_title,
       x = "Memory factor",
       y = "Mean Squared Error (MSE)")


dev.off()

################################################################################

###### Bias analysis ######

graph_data_bias = gen_graph_df(simulation_data, 'bias')


# Graph creation

plot_name = str_c("Simulation_memoryfactor_", seed_number, "_notdisjoint_bias.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_bias) + 
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) +
  
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the memory factor",
       subtitle = sub_title,
       x = "Memory factor",
       y = "Hidden population estimate")


dev.off()



################################################################################

#### Standard deviation analysis ####

# Dataframe creation #

graph_data_sd = gen_graph_df(simulation_data, 'sd')


# Graph creation

plot_name = str_c("Simulation_memoryfactor_", seed_number, "_notdisjoint_sd.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_sd) + 
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the memory factor",
       subtitle = sub_title,
       x = "Memory factor",
       y = "Standard deviation")


dev.off()



################################################################################

#### Median analysis ####

# Dataframe creation #

graph_data_median = gen_graph_df(simulation_data, 'median')


# Graph creation

plot_name = str_c("Simulation_memoryfactor_", seed_number, "_notdisjoint_median.png")
sub_title = str_c("Disjoint & not disjoint populations plot, seed ", seed_number)

png(filename = plot_name,
    width = 1000, height = 600)

ggplot(graph_data_median) + 
  geom_line(aes(x = data, y =  Nh_real, col = "Nh_real")) +
  
  geom_line(aes(x = data, y =  Nh_basic_sum, col = "Nh_basic_sum")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_sum, col = "Nh_basicvis_sum")) + 
  
  geom_line(aes(x = data, y =  Nh_basic_mean, col = "Nh_basic_mean")) + 
  #geom_line(aes(x = data, y =  Nh_basicvis_mean, col = "Nh_basicvis_mean")) +
  scale_color_discrete("Legend") + 
  labs(title = "Simulations based on the memory factor",
       subtitle = sub_title,
       x = "Memory factor",
       y = "Median")

dev.off()