################################################################################################################
# Simulation based on the value of the memory factor of the subpopulations, leaving the rest of parameters fixed
################################################################################################################

t = Sys.time()


# Population size
N = 10000

# Probability of each subpopulation
v_pop_prob = c(0.150, 0.150, 0.125, 0.100,0.075, 0.050, 0.050)    

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

# Network
net_model = sample_smallworld(dim, N, nei, p, loops = FALSE, multiple = FALSE)

## Populations ##
# Not disjoint population #

Graph_population_matrix = gen_Data_uniform(N, v_pop_prob, hp_prob, visibility_factor, memory_factor, sub_memory_factor, net = net_model, seed = seed)

net_sw = Graph_population_matrix[[1]]       # Population´s graph
Population = Graph_population_matrix[[2]]   # Population
Mhp_model = Graph_population_matrix[[3]]      # Population's visibility matrix

#Vector with the number of people in each subpopulation

v_pop_total = getV_pop(n_pop, Population)

################################################################################

# Disjoint population #

Population_disjoint =  gen_Population_disjoint(N, net_model, v_pop_prob, Population$hidden_population, Mhp_model, sub_memory_factor, Population$reach, Population$reach_memory, Population$hp_total, Population$hp_survey, seed = seed)

v_pop_total_disjoint =  getV_pop(n_pop, Population_disjoint)
################################################################################


################################################################################
# Auxiliary simulation data #

# Study parameters
parameters = seq(from = 0.1, to = 1, length.out = 10)

#Dataframe to save the data
simulaciones          = data.frame(data = parameters)
simulaciones_disjoint = data.frame(data = parameters)

#Number of iterations for the simulation
b = 15

matrix_vfe            = matrix(0, length(parameters), b)
matrix_vfe_disjoint   = matrix(0, length(parameters), b)


lista_simulacion          = list()
lista_simulacion_disjoint = list()

################################################################################
# Fixed population parameters #
set.seed(seed)

## Surveys ##

# The surveys are fixed so the variance and bias can be calculated.

list_surveys = list()
for (h in 1:b) {
  list_surveys[[h]] = gen_Survey(n_survey, Population)
}

list_surveys_hp = list()
for (h in 1:b) {
  list_surveys_hp[[h]] = gen_Survey(n_survey_hp, Population[Population$hidden_population == 1,])
}

# Fixed population parameters #
set.seed(seed)

Mhp_vis_list = list()
for (i in 1:length(parameters)){
  visibility_factor = parameters[i] 
  Mhp_vis_list[[i]] =  matrixHP_visibility(Mhp_model, visibility_factor)
}

################################################################################
# Fixed population parameters #
set.seed(seed_sim)

# Simulation

for (w in 1:length(parameters)) {
  ## Parameter implementation ##
  visibility_factor = parameters[w] 
  Mhp_vis = Mhp_vis_list[[w]]
  
  # Not disjoint #
  population_buc = dplyr::select(Population, 'hidden_population' | starts_with('subpop') | 'reach' | 'hp_total')  
  
  population_buc  = cbind(population_buc, gen_Reach_hp_memory(population_buc, Mhp_vis, memory_factor)) # HP reach recall error variable
  population_buc  = cbind(population_buc, gen_Reach_memory(population_buc, memory_factor)) #Reach recall error variable
  population_buc  = cbind(population_buc, gen_Subpopulation_memoryfactor(population_buc, Mhp_vis, sub_memory_factor, net_sw) )
  population_buc  = cbind(population_buc, gen_Subpopulation_alters_memoryfactor(population_buc, Mhp_vis, sub_memory_factor) )
  
  Population = population_buc
  
  
  # Disjoint #
  population_disjoint_buc = dplyr::select(Population_disjoint, 'hidden_population' | starts_with('subpop') | 'reach' | 'hp_total') 
  
  population_disjoint_buc  = cbind(population_disjoint_buc, reach_memory = Population$reach_memory)
  population_disjoint_buc  = cbind(population_disjoint_buc, hp_survey = Population$hp_survey)
  population_disjoint_buc  = cbind(population_disjoint_buc, gen_Subpopulation_memoryfactor(population_disjoint_buc, Mhp_vis, sub_memory_factor, net_sw) )
  population_disjoint_buc  = cbind(population_disjoint_buc, gen_Subpopulation_alters_memoryfactor(population_disjoint_buc, Mhp_vis, sub_memory_factor) )
  
  Population_disjoint = population_disjoint_buc
  
  
  ##########################################  
  ##   Not disjoint population analysis   ##
  
  lista_sim = list()
  
  # Population for the VF estimate
  Population_vf = gen_Survey_VF(sum(Population$hidden_population), Population, Mhp_vis, memory_factor)
  
  #Iterations
  for (l in 1:b) {
    #We choose the same survey for each l in order to calculate the bias and variance
    #Surveys
    survey = Population[list_surveys[[l]],]
    survey_hp = Population[Population$hidden_population == 1,][list_surveys_hp[[l]],]
    survey_hp_vf = Population_vf[list_surveys_hp[[l]],]
    
    #Visibility factor estimate
    vf_estimate = VF_Estimate(survey_hp_vf)
    
    #Hidden population estimates
    Nh_real = sum(Population$hidden_population) 
    
    #Nh_basic_sum    = getNh_basic_sum(survey,N) 
    Nh_basicvis_sum = getNh_basicvis_sum(survey,N,vf_estimate) 
    #Nh_basic_mean    = getNh_basic_mean(survey,N) 
    Nh_basicvis_mean = getNh_basicvis_mean(survey,N,vf_estimate) 
    
    #Nh_PIMLE    = getNh_PIMLE(survey, v_pop_total, N)
    Nh_PIMLEvis = getNh_PIMLEvis(survey, v_pop_total, N, vf_estimate)
    
    #Nh_MLE     = getNh_MLE(survey, v_pop_total)
    Nh_MLEvis  = getNh_MLEvis(survey, v_pop_total, vf_estimate)
    
    #Nh_MoS     = getNh_MoS(survey, v_pop_total, N)
    Nh_MoSvis  = getNh_MoSvis(survey, v_pop_total, N, vf_estimate)
    
    #Nh_MLE_mod  = getNh_MLE_mod(survey, v_pop_total, N)
    Nh_MLE_modvis  = getNh_MLE_modvis(survey, v_pop_total, N, vf_estimate)
    
    Nh_GNSUM     =  getNh_GNSUM(survey, survey_hp, v_pop_total, N)
    
    #Nh_TEO      = getNh_TEO(survey, v_pop_prob, N, iter = 1000)
    Nh_TEOvis    = getNh_TEOvis(survey, v_pop_prob, N, vf_est = vf_estimate, iter = 1000)
    
    #Nh_Zheng    = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)
    Nh_Zhengvis   = getNh_Zhengvis(survey, v_pop_prob, N, vf_est = vf_estimate, iterations = 5000, burnins = 1000)
    
    #Dataframe for saving the estimates
    sim = data.frame(Nh_real = Nh_real)
    names(sim)[dim(sim)[2]] = str_c("Nh_real_",l)
    
    #sim = cbind(sim,Nh_basic_sum = Nh_basic_sum)
    #names(sim)[dim(sim)[2]] = str_c("Nh_basic_sum_",l)
    
    sim = cbind(sim,Nh_basicvis_sum = Nh_basicvis_sum)
    names(sim)[dim(sim)[2]] = str_c("Nh_basicvis_sum_",l)
    
    #sim = cbind(sim,Nh_basic_mean = Nh_basic_mean)
    #names(sim)[dim(sim)[2]] = str_c("Nh_basic_mean_",l)
    
    sim = cbind(sim,Nh_basicvis_mean = Nh_basicvis_mean)
    names(sim)[dim(sim)[2]] = str_c("Nh_basicvis_mean_",l)
    
    #sim = cbind(sim,Nh_PIMLE = Nh_PIMLE)
    #names(sim)[dim(sim)[2]] = str_c("Nh_PIMLE_",l)
    
    sim = cbind(sim,Nh_PIMLEvis = Nh_PIMLEvis)
    names(sim)[dim(sim)[2]] = str_c("Nh_PIMLEvis_",l)
    
    #sim = cbind(sim,Nh_MLE = Nh_MLE)
    #names(sim)[dim(sim)[2]] = str_c("Nh_MLE_",l)
    
    sim = cbind(sim,Nh_MLEvis = Nh_MLEvis)
    names(sim)[dim(sim)[2]] = str_c("Nh_MLEvis_",l)
    
    #sim = cbind(sim,Nh_MoS = Nh_MoS)
    #names(sim)[dim(sim)[2]] = str_c("Nh_MoS_",l)
    
    sim = cbind(sim,Nh_MoSvis = Nh_MoSvis)
    names(sim)[dim(sim)[2]] = str_c("Nh_MoSvis_",l)
    
    sim = cbind(sim,Nh_GNSUM = Nh_GNSUM)
    names(sim)[dim(sim)[2]] = str_c("Nh_GNSUM_",l)
    
    #sim = cbind(sim,Nh_MLE_mod = Nh_MLE_mod)
    #names(sim)[dim(sim)[2]] = str_c("Nh_MLE_mod_",l)
    
    sim = cbind(sim,Nh_MLE_modvis = Nh_MLE_modvis)
    names(sim)[dim(sim)[2]] = str_c("Nh_MLE_modvis_",l)
    
    #sim = cbind(sim, Nh_TEO = Nh_TEO)
    #names(sim)[dim(sim)[2]] = str_c("Nh_TEO_",l)
    
    sim = cbind(sim, Nh_TEOvis = Nh_TEOvis)
    names(sim)[dim(sim)[2]] = str_c("Nh_TEOvis_",l)
    
    #sim = cbind(sim, Nh_Zheng = Nh_Zheng)
    #names(sim)[dim(sim)[2]] = str_c("Nh_Zheng_",l)
    
    sim = cbind(sim, Nh_Zhengvis = Nh_Zhengvis)
    names(sim)[dim(sim)[2]] = str_c("Nh_Zhengvis_",l)
    
    matrix_vfe[w,l] = vf_estimate
    lista_sim[[l]] = sim
  }
  simulacion = bind_cols(lista_sim)
  lista_simulacion[[w]] = simulacion
  
  
  
  ######################################
  ## Disjoint subpopulations analysis ##
  
  lista_sim_disjoint = list()
  
  # Population for the visibility factor (vf) estimate
  Population_disjoint_vf = cbind(Population_disjoint[Population_disjoint$hidden_population == 1,], reach_hp = Population_vf$reach_hp)
  Population_disjoint_vf = cbind(Population_disjoint_vf, reach_hp_memory = Population_vf$reach_hp_memory)
  
  #Iterations
  for (l in 1:b) {
    
    #We choose the same survey for each l in order to calculate the bias and variance
    #Surveys
    survey = Population_disjoint[list_surveys[[l]],]
    survey_hp = Population_disjoint[Population_disjoint$hidden_population == 1,][list_surveys_hp[[l]],]
    survey_hp_vf = Population_vf[list_surveys_hp[[l]],]
    
    #Visibility factor estimate
    vf_estimate = VF_Estimate(survey_hp_vf)
    
    #Hidden population estimates
    Nh_real_disjoint = sum(Population_disjoint$hidden_population) 
    
    #Nh_basic_sum_disjoint    = getNh_basic_sum(survey,N) 
    Nh_basicvis_sum_disjoint = getNh_basicvis_sum(survey,N,vf_estimate) 
    #Nh_basic_mean_disjoint    = getNh_basic_mean(survey,N) 
    Nh_basicvis_mean_disjoint = getNh_basicvis_mean(survey,N,vf_estimate) 
    
    #Nh_PIMLE_disjoint    = getNh_PIMLE(survey, v_pop_total_disjoint, N)
    Nh_PIMLEvis_disjoint = getNh_PIMLEvis(survey, v_pop_total_disjoint, N, vf_estimate)
    
    #Nh_MLE_disjoint     = getNh_MLE(survey, v_pop_total_disjoint)
    Nh_MLEvis_disjoint  = getNh_MLEvis(survey, v_pop_total_disjoint, vf_estimate)
    
    #Nh_MoS_disjoint     = getNh_MoS(survey, v_pop_total_disjoint, N)
    Nh_MoSvis_disjoint  = getNh_MoSvis(survey, v_pop_total_disjoint, N, vf_estimate)
    
    #Nh_MLE_mod_disjoint      = getNh_MLE_mod(survey, v_pop_total_disjoint, N)
    Nh_MLE_modvis_disjoint  = getNh_MLE_modvis(survey, v_pop_total_disjoint, N, vf_estimate)
    
    Nh_GNSUM_disjoint   =  getNh_GNSUM(survey, survey_hp, v_pop_total_disjoint, N)    
    
    #Nh_TEO_disjoint    = getNh_TEO(survey, v_pop_prob, N, iter = 1000)
    Nh_TEOvis_disjoint    = getNh_TEOvis(survey, v_pop_prob, N, vf_est = vf_estimate, iter = 1000)
    
    #Nh_Zheng_disjoint  = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)
    Nh_Zhengvis_disjoint  = getNh_Zhengvis(survey, v_pop_prob, N, vf_est = vf_estimate, iterations = 5000, burnins =1000)
    
    
    #Dataframe for saving the estimates
    sim_disjoint = data.frame(Nh_real = Nh_real_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_real_",l)
    
    #sim_disjoint = cbind(sim_disjoint,Nh_basic_sum = Nh_basic_sum_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_basic_sum_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_basicvis_sum = Nh_basicvis_sum_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_basicvis_sum_",l)
    
    #sim_disjoint = cbind(sim_disjoint,Nh_basic_mean = Nh_basic_mean_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_basic_mean_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_basicvis_mean = Nh_basicvis_mean_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_basicvis_mean_",l)
    
    #sim_disjoint = cbind(sim_disjoint,Nh_PIMLE = Nh_PIMLE_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_PIMLE_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_PIMLEvis = Nh_PIMLEvis_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_PIMLEvis_",l)
    
    #sim_disjoint = cbind(sim_disjoint,Nh_MLE = Nh_MLE_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MLE_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_MLEvis = Nh_MLEvis_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MLEvis_",l)
    
    #sim_disjoint = cbind(sim_disjoint,Nh_MoS = Nh_MoS_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MoS_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_MoSvis = Nh_MoSvis_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MoSvis_",l)
    
    sim_disjoint = cbind(sim_disjoint, Nh_GNSUM = Nh_GNSUM_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_GNSUM_",l)
    
    #sim_disjoint = cbind(sim_disjoint,Nh_MLE_mod = Nh_MLE_mod_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MLE_mod_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_MLE_modvis = Nh_MLE_modvis_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MLE_modvis_",l)
    
    #sim_disjoint = cbind(sim_disjoint, Nh_TEO = Nh_TEO_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_TEO_",l)
    
    sim_disjoint = cbind(sim_disjoint, Nh_TEOvis = Nh_TEOvis_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_TEOvis_",l)
    
    #sim_disjoint = cbind(sim_disjoint, Nh_Zheng = Nh_Zheng_disjoint)
    #names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_Zheng_",l)
    
    sim_disjoint = cbind(sim_disjoint, Nh_Zhengvis = Nh_Zhengvis_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_Zhengvis_",l)
    
    matrix_vfe_disjoint[w,l] = vf_estimate
    lista_sim_disjoint[[l]] = sim_disjoint
  }
  simulacion_disjoint = bind_cols(lista_sim_disjoint)
  lista_simulacion_disjoint[[w]] = simulacion_disjoint
  
  print(w)
  
}

simulaciones = bind_rows(lista_simulacion)
simulaciones_disjoint = bind_rows(lista_simulacion_disjoint)

simulaciones["data"] = parameters
simulaciones_disjoint["data"] = parameters



################################################################################
file_name = str_c("Simulation_visibilityfactor_notdisjoint_uniform_sw_pop1_", seed_sim, ".csv")
write.csv(simulaciones,                         # Data frame
          file = file_name,                     # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################


################################################################################
file_name_disjoint = str_c("Simulation_visibilityfactor_disjoint_uniform_sw_pop1_", seed_sim,".csv")
write.csv(simulaciones_disjoint,                # Data frame
          file = file_name_disjoint,            # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################

################################################################################
file_name_disjoint = str_c("Simulation_visibilityfactor_disjoint_vfe_matrix_uniform_sw_pop1_", seed_sim,".csv")
write.csv(matrix_vfe_disjoint,                  # Data frame
          file = file_name_disjoint,            # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################

################################################################################
file_name_disjoint = str_c("Simulation_visibilityfactor_notdisjoint_vfe_matrix_uniform_sw_pop1_", seed_sim,".csv")
write.csv(matrix_vfe,                  # Data frame
          file = file_name_disjoint,            # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################

timer = Sys.time() - t
timer

####################### Network analysis #######################################
###### Links to the hidden population distribution & Degree distribution #######
plot_name = str_c("Network_visibilityfactor_sw_", seed, ".png")

png(filename = plot_name,
    width = 1000, height = 1000)
net_analysis(net_sw, Population, p, 2*nei)
dev.off()


#################### COMPUTATION TIME ANALYSIS ###########################
# Computation time (N=10000) (virtual machine) 
# timer -> 
###########################################################################