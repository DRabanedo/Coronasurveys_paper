################################################################################################################
# Simulation based on the value of the memory factor of the reach variable, leaving the rest of parameters fixed
################################################################################################################

t = Sys.time()

# Population size
N = 10000

# Probability of each subpopulation
v_pop_prob = c(rep(0.02,10), rep(0.04, 5), 0.08, 0.08, 0.16)

# Number of subpopulations
n_pop = length(v_pop_prob)   

# Number of individuals we draw in the survey
n_survey = 500                

# Number of individuals we draw in the hidden population survey 
n_survey_hp = 50              

# Proportion of individuals in the hidden population
hp_prob = 0.1 

# Subpopulation memory factor (parameter to change variance of the perturbations' normal)
sub_memory_factor = 0   

# Visibility factor (Binomial's probability)
visibility_factor = 1     

#reach memory factor (parameter to change variance of the perturbations' normal)
memory_factor = 0            

# Seed
# Seed to obtain the fixed parameters #
seed = 921  
# Seed to perform the simulation #
seed_sim = 2022

################################################################################
## Graph  properties ##

# Graph dimension 
dim = 1   
# Number of neighbors per side that each node is connected to (2*nei neighbors) 
nei = 50     
# Probability of randomize a connection between nodes. It is applied to all connections
p   = 0.1   

################################################################################
# Fixed population parameters #
set.seed(seed)

# Network model #
net_model = sample_smallworld(dim, N, nei, p, loops = FALSE, multiple = FALSE)

## Populations models ##
# Not disjoint population #
Graph_population_matrix = gen_Data_SIR(N, v_pop_prob, visibility_factor, memory_factor, sub_memory_factor, net = net_model, seed = seed)

net_sw     = Graph_population_matrix[[1]]   # Population´s graph
Population = Graph_population_matrix[[2]]   # Population
Mhp_vis    = Graph_population_matrix[[3]]   # Population's visibility matrix

# Population number
v_pop_total = getV_pop(n_pop, Population)

################################################################################

## Auxiliar simulation data ##

# Number of simulations
b = 20 

# Study parameters
parameters    = round(seq(from = 1, to = N, length.out = 15))
parameters_hp = round(seq(from = 1, to = sum(Population$hidden_population), length.out = 15))


# Fixed population parameters #
set.seed(seed)

list_survey = list()
for (i in 1:length(parameters)){
  list_survey[[i]] = gen_Survey(parameters[i], Population)
}


# Fixed population parameters #
set.seed(seed)

list_survey_hp = list()
for (i in 1:length(parameters)){
  list_survey_hp[[i]] = gen_Survey(parameters_hp[i], Population[Population$hidden_population == 1,])
}

################################################################################

set.seed(seed_sim)

#Simulation

for (l in 1:b) {
  
  ###########################
  # Not disjoint population #
  
  Nh_real =  rep(NA,length(parameters)) 
  
  Nh_basic_sum    = rep(NA,length(parameters)) 
  #Nh_basicvis_sum = rep(NA,length(parameters))  
  Nh_basic_mean    = rep(NA,length(parameters)) 
  #Nh_basicvis_mean = rep(NA,length(parameters)) 
  
  Nh_PIMLE = rep(NA,length(parameters)) 
  #Nh_PIMLEvis = rep(NA,length(parameters)) 
  
  Nh_MLE = rep(NA,length(parameters)) 
  #Nh_MLEvis = rep(NA,length(parameters)) 
  
  Nh_MLE_mod     = rep(NA,length(parameters))
  #Nh_MLE_modvis = rep(NA,length(parameters))
  
  Nh_MoS = rep(NA,length(parameters)) 
  #Nh_MoSvis = rep(NA,length(parameters)) 
  
  Nh_GNSUM = rep(NA,length(parameters))
  
  Nh_TEO      = rep(NA,length(parameters))
  #Nh_TEOvis    = rep(NA,length(parameters))
  
  Nh_Zheng    = rep(NA,length(parameters))
  #Nh_Zhengvis   = rep(NA,length(parameters))
  
  
  Nh_Direct = rep(NA,length(parameters))
  
  
  ########################
  # Disjoint populations #
  
  ## Variable reset ##
  
  Nh_real_disjoint =  rep(NA,length(parameters))
  
  Nh_basic_sum_disjoint = rep(NA,length(parameters))
  #Nh_basicvis_sum_disjoint = rep(NA,length(parameters))
  Nh_basic_mean_disjoint = rep(NA,length(parameters))
  #Nh_basicvis_mean_disjoint = rep(NA,length(parameters))                                    
  
  Nh_PIMLE_disjoint = rep(NA,length(parameters))
  #Nh_PIMLEvis_disjoint = rep(NA,length(parameters))
  
  Nh_MLE_disjoint = rep(NA,length(parameters))
  #Nh_MLEvis_disjoint = rep(NA,length(parameters))
  
  Nh_MLE_mod_disjoint     = rep(NA,length(parameters))
  #Nh_MLE_modvis_disjoint = rep(NA,length(parameters))
  
  Nh_MoS_disjoint = rep(NA,length(parameters))
  #Nh_MoSvis_disjoint = rep(NA,length(parameters)) 
  
  Nh_GNSUM_disjoint = rep(NA,length(parameters))
  
  Nh_TEO_disjoint      = rep(NA,length(parameters))
  #Nh_TEOvis_disjoint    = rep(NA,length(parameters))
  
  Nh_Zheng_disjoint     = rep(NA,length(parameters))
  #Nh_Zhengvis_disjoint   = rep(NA,length(parameters))
  
  
  Nh_Direct_disjoint = rep(NA,length(parameters))
  
  
  for (i in 1:length(parameters)) {
    
    #Surveys variation
    survey = Population[list_survey[[i]],]
    #Hidden's population survey
    survey_hp = Population[Population$hidden_population == 1,][list_survey_hp[[i]],]
    
    #Surveys variation
    survey_disjoint = Population_disjoint[list_survey[[i]],]
    survey_hp_disjoint = Population_disjoint[Population_disjoint$hidden_population == 1,][list_survey_hp[[i]],]
    
    
    #Visibility factor estimate
    # Population for the VF estimate
    Population_vf = gen_Survey_VF(sum(Population$hidden_population), Population, Mhp_vis, memory_factor)
    survey_hp_vf  = Population_vf[list_survey_hp[[i]],]
    
    vf_estimate = VF_Estimate(survey_hp_vf)
    vf_estimate_disjoint = vf_estimate
    
    #Estimations
    Nh_real[i] = sum(Population$hidden_population) 
    
    Nh_basic_sum[i]    = getNh_basic_sum(survey,N) 
    #Nh_basicvis_sum[i] = getNh_basicvis_sum(survey,N,vf_estimate)
    
    Nh_basic_mean[i]    = getNh_basic_mean(survey,N) 
    #Nh_basicvis_mean[i] = getNh_basicvis_mean(survey,N,vf_estimate)
    
    Nh_PIMLE[i] = getNh_PIMLE(survey, v_pop_total, N)
    #Nh_PIMLEvis[i] = getNh_PIMLEvis(survey, v_pop_total, N, vf_estimate)
    
    Nh_MLE[i] = getNh_MLE(survey, v_pop_total)
    #Nh_MLEvis[i] = getNh_MLEvis(survey, v_pop_total, vf_estimate)
    
    Nh_MLE_mod[i]  = getNh_MLE_mod(survey, v_pop_total, N)
    #Nh_MLE_modvis[i]  = getNh_MLE_modvis(survey, v_pop_total, N, vf_estimate)
    
    Nh_MoS[i] = getNh_MoS(survey, v_pop_total, N)
    #Nh_MoSvis[i] = getNh_MoSvis(survey, v_pop_total, N, vf_estimate)
    
    Nh_GNSUM[i] =  getNh_GNSUM(survey, survey_hp, v_pop_total, N)
    
    Nh_TEO[i]      = getNh_TEO(survey, v_pop_prob, N, iter = 1000)
    #Nh_TEOvis[i]    = getNh_TEOvis(survey, v_pop_prob, N, vf_est = vf_estimate, iter = 1000)
    
    Nh_Zheng[i]    = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)
    #Nh_Zhengvis[i]   = getNh_Zhengvis(survey, v_pop_prob, N, vf_est = vf_estimate, iterations = 5000, burnins = 1000)
    
    
    Nh_Direct[i] = getNh_Direct(survey, N)
    
    
    #Estimations
    Nh_real_disjoint[i] = sum(Population_disjoint$hidden_population) 
    
    Nh_basic_sum_disjoint[i]    = getNh_basic_sum(survey_disjoint,N) 
    #Nh_basicvis_sum_disjoint[i] = getNh_basicvis_sum(survey_disjoint,N,vf_estimate_disjoint)
    
    Nh_basic_mean_disjoint[i]    = getNh_basic_mean(survey_disjoint,N) 
    #Nh_basicvis_mean_disjoint[i] = getNh_basicvis_mean(survey_disjoint,N,vf_estimate_disjoint)
    
    Nh_PIMLE_disjoint[i] = getNh_PIMLE(survey_disjoint, v_pop_total_disjoint, N)
    #Nh_PIMLEvis_disjoint[i] = getNh_PIMLEvis(survey_disjoint, v_pop_total_disjoint, N, vf_estimate_disjoint)
    
    Nh_MLE_disjoint[i] = getNh_MLE(survey_disjoint, v_pop_total_disjoint)
    #Nh_MLEvis_disjoint[i] = getNh_MLEvis(survey_disjoint, v_pop_total_disjoint, vf_estimate_disjoint)
    
    Nh_MLE_mod_disjoint[i]      = getNh_MLE_mod(survey_disjoint, v_pop_total_disjoint, N)
    #Nh_MLE_modvis_disjoint[i]  = getNh_MLE_modvis(survey_disjoint, v_pop_total_disjoint, N, vf_estimate)
    
    Nh_MoS_disjoint[i] = getNh_MoS(survey_disjoint, v_pop_total_disjoint, N)
    #Nh_MoSvis_disjoint[i] = getNh_MoSvis(survey_disjoint, v_pop_total_disjoint, N, vf_estimate_disjoint)
    
    Nh_GNSUM_disjoint[i] =  getNh_GNSUM(survey_disjoint, survey_disjoint_hp, v_pop_total_disjoint, N)
    
    Nh_TEO_disjoint[i]    = getNh_TEO(survey_disjoint, v_pop_prob, N, iter = 1000)
    #Nh_TEOvis_disjoint[i]    = getNh_TEOvis(survey_disjoint, v_pop_prob, N, iter = 1000)
    
    Nh_Zheng_disjoint[i]  = getNh_Zheng(survey_disjoint, v_pop_prob, N, iterations = 5000, burnins =1000)
    #Nh_Zhengvis_disjoint[i]  = getNh_Zhengvis(survey_disjoint, v_pop_prob, N, iterations = 5000, burnins =1000)
    
    Nh_Direct_disjoint[i] = getNh_Direct(survey_disjoint, N)
  }
  
  #Dataframe construction
  
  simulaciones = cbind(simulaciones,Nh_real = Nh_real)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_real_",l)
  
  simulaciones = cbind(simulaciones,Nh_basic_sum = Nh_basic_sum)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_basic_sum_",l)
  
  #simulaciones = cbind(simulaciones,Nh_basicvis_sum = Nh_basicvis_sum)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_basicvis_sum_",l)
  
  simulaciones = cbind(simulaciones,Nh_basic_mean = Nh_basic_mean)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_basic_mean_",l)
  
  #simulaciones = cbind(simulaciones,Nh_basicvis_mean = Nh_basicvis_mean)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_basicvis_mean_",l)
  
  simulaciones = cbind(simulaciones,Nh_PIMLE = Nh_PIMLE)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_PIMLE_",l)
  
  #simulaciones = cbind(simulaciones,Nh_PIMLEvis = Nh_PIMLEvis)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_PIMLEvis_",l)
  
  simulaciones = cbind(simulaciones,Nh_MLE = Nh_MLE)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_MLE_",l)
  
  #simulaciones = cbind(simulaciones,Nh_MLEvis = Nh_MLEvis)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_MLEvis_",l)
  
  simulaciones = cbind(simulaciones,Nh_MLE_mod = Nh_MLE_mod)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_MLE_mod_",l)
  
  #simulaciones = cbind(simulaciones,Nh_MLE_modvis = Nh_MLE_modvis)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_MLE_modvis_",l)
  
  simulaciones = cbind(simulaciones,Nh_MoS = Nh_MoS)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_MoS_",l)
  
  #simulaciones = cbind(simulaciones,Nh_MoSvis = Nh_MoSvis)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_MoSvis_",l)
  
  simulaciones = cbind(simulaciones,Nh_GNSUM = Nh_GNSUM)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_GNSUM_",l)
  
  simulaciones = cbind(simulaciones,Nh_TEO = Nh_TEO)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_TEO_",l)
  
  #simulaciones = cbind(simulaciones,Nh_TEOvis = Nh_TEOvis)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_TEOvis_",l)
  
  simulaciones = cbind(simulaciones,Nh_Zheng = Nh_Zheng)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_Zheng_",l)
  
  #simulaciones = cbind(simulaciones,Nh_Zhengvis = Nh_Zhengvis)
  #names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_Zhengvis_",l)
  
  simulaciones = cbind(simulaciones,Nh_Direct = Nh_Direct)
  names(simulaciones)[dim(simulaciones)[2]] = str_c("Nh_Direct_",l)
  
  
  #Dataframe construction
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_real = Nh_real)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_real_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_basic_sum = Nh_basic_sum_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_basic_sum_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_basicvis_sum = Nh_basicvis_sum_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_basicvis_sum_disjoint_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_basic_mean = Nh_basic_mean_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_basic_mean_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_basicvis_mean = Nh_basicvis_mean_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_basicvis_mean__disjoint",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_PIMLE = Nh_PIMLE_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_PIMLE_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_PIMLEvis = Nh_PIMLEvis_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_PIMLEvis_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_MLE = Nh_MLE_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_MLE_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_MLEvis = Nh_MLEvis_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_MLEvis_disjoint_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_MLE_mod = Nh_MLE_mod_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_MLE_mod_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_MLE_modvis = Nh_MLE_modvis_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_MLE_modvis_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_MoS = Nh_MoS_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_MoS_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_MoSvis = Nh_MoSvis_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_MoSvis_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_GNSUM = Nh_GNSUM_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_GNSUM_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint, Nh_TEO = Nh_TEO_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_TEO_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint, Nh_TEOvis = Nh_TEOvis_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_TEOvis_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_Zheng = Nh_Zheng_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_Zheng_",l)
  
  #simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_Zhengvis = Nh_Zhengvis_disjoint)
  #names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_Zhengvis_",l)
  
  simulaciones_disjoint = cbind(simulaciones_disjoint,Nh_Direct = Nh_Direct_disjoint)
  names(simulaciones_disjoint)[dim(simulaciones_disjoint)[2]] = str_c("Nh_Direct_",l)
  
  
  print(l)
  
}




################################################################################
file_name = str_c("Simulation_surveysize_notdisjoint_sir_sw_pop1_", seed_sim,".csv")
write.csv(simulaciones,                      # Data frame
          file = file_name,                  # CSV name
          row.names = FALSE )                 # row names: TRUE or FALSE 

################################################################################


################################################################################
file_name_disjoint = str_c("Simulation_surveysize_disjoint_sir_sw_pop1_", seed_sim,".csv")
write.csv(simulaciones_disjoint,              # Data frame
          file = file_name_disjoint,          # CSV name
          row.names = FALSE )                  # row names: TRUE or FALSE 

################################################################################

timer = Sys.time() - t
timer

####################### Network analysis #######################################
###### Links to the hidden population distribution & Degree distribution #######
plot_name = str_c("Network_surveysize_sir_sw_", seed, ".png")

png(filename = plot_name,
    width = 1000, height = 1000)
net_analysis(net_sw, Population, p, 2*nei)
dev.off()

#################### COMPUTATION TIME ANALYSIS ###########################
# Computation time (N=10000) (virtual machine)
#timer ->  1.112204 hours
###########################################################################