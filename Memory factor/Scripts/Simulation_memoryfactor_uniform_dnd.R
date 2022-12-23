################################################################################################################
# Simulation based on the value of the memory factor of the reach variable, leaving the rest of parameters fixed
################################################################################################################

t = Sys.time()

################################
## Simulation data parameters ##
################################

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
seed = 2022

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
Graph_population_matrix = gen_Data_uniform(N, v_pop_prob, hp_prob, visibility_factor, memory_factor, sub_memory_factor, net = net_model, seed = seed)

net_sw     = Graph_population_matrix[[1]]   # Population´s graph
Population = Graph_population_matrix[[2]]   # Population
Mhp_vis    = Graph_population_matrix[[3]]   # Population's visibility matrix

# Population number
v_pop_total = getV_pop(n_pop, Population)

################################################################################

## Auxiliary data for the simulation ##

# Variables reach vector
vect_reach    =  Population$reach
vect_hp       =  Population$hp_total
vect_hp_vis   =  rep(NA, nrow(Population))
vect_reach_re =  rep(NA, nrow(Population))

#Number of iterations for the simulation
b = 25

lista_simulacion = list()

################################################################################
## Fixed loop parameters ##

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

################################################################################

# Simulation

# First, the seed of the simulation is chosen
set.seed(seed_sim)

# Loop
for (w in 1:length(parameters)) {
  ## Parameter implementation ##
  memory_factor = parameters[w]  
  
  vect_reach_re = gen_Reach_memory(Population, memory_factor)$reach_memory
  vect_hp_vis   = gen_Reach_hp_memory(Population, Mhp_vis, memory_factor)$hp_survey
  
  Population$reach_memory = vect_reach_re
  Population$hp_survey    = vect_hp_vis
  
  ##########################################  
  ##   Not disjoint population analysis   ##
  
  lista_sim = list()
  
  # Population for the VF estimate
  Population_vf = gen_Survey_VF(sum(Population$hidden_population), Population, Mhp_vis, memory_factor)
  
  #Iterations
  for (l in 1:b) {
    
    #We choose the same survey for each l in order to calculate the bias and variance
    #Surveys
    survey       = Population[list_surveys[[l]],]
    survey_hp    = Population[Population$hidden_population == 1,][list_surveys_hp[[l]],]
    survey_hp_vf = Population_vf[list_surveys_hp[[l]],]
    
    # Visibility factor estimate
    vf_estimate = VF_Estimate(survey_hp_vf)
    
    # Hidden population estimates
    Nh_real = sum(Population$hidden_population) 
    
    Nh_basic_sum      = getNh_basic_sum(survey,N) 
    #Nh_basicvis_sum  = getNh_basicvis_sum(survey,N,vf_estimate) 
    Nh_basic_mean     = getNh_basic_mean(survey,N) 
    #Nh_basicvis_mean = getNh_basicvis_mean(survey,N,vf_estimate) 
    
    #Nh_PIMLE    = getNh_PIMLE(survey, v_pop_total, N)
    #Nh_PIMLEvis = getNh_PIMLEvis(survey, v_pop_total, N, vf_estimate)
    
    #Nh_MLE     = getNh_MLE(survey, v_pop_total)
    #Nh_MLEvis  = getNh_MLEvis(survey, v_pop_total, vf_estimate)
    
    #Nh_MLE_mod     = getNh_MLE_mod(survey, v_pop_total, N)
    #Nh_MLE_modvis  = getNh_MLE_modvis(survey, v_pop_total, N, vf_estimate)
    
    #Nh_MoS     = getNh_MoS(survey, v_pop_total, N)
    #Nh_MoSvis  = getNh_MoSvis(survey, v_pop_total, N, vf_estimate)
    
    #Nh_GNSUM   =  getNh_GNSUM(survey, survey_hp, v_pop_total, N)
    
    #Nh_TEO      = getNh_TEO(survey, v_pop_prob, N, iter = 1000)
    #Nh_TEOvis    = getNh_TEOvis(survey, v_pop_prob, N, vf_est = vf_estimate, iter = 1000)
    
    #Nh_Zheng    = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)
    #Nh_Zhengvis   = getNh_Zhengvis(survey, v_pop_prob, N, vf_est = vf_estimate, iterations = 5000, burnins = 1000)
    
    
    
    #Dataframe for saving the estimates
    sim = data.frame(Nh_real = Nh_real)
    names(sim)[dim(sim)[2]]  = str_c("Nh_real_",l)
    
    sim = cbind(sim,Nh_basic_sum = Nh_basic_sum)
    names(sim)[dim(sim)[2]] = str_c("Nh_basic_sum_",l)
    
    #sim = cbind(sim,Nh_basicvis_sum = Nh_basicvis_sum)
    #names(sim)[dim(sim)[2]] = str_c("Nh_basicvis_sum_",l)
    
    sim = cbind(sim,Nh_basic_mean = Nh_basic_mean)
    names(sim)[dim(sim)[2]] = str_c("Nh_basic_mean_",l)
    
    #sim = cbind(sim,Nh_basicvis_mean = Nh_basicvis_mean)
    #names(sim)[dim(sim)[2]] = str_c("Nh_basicvis_mean_",l)
    
    #sim = cbind(sim,Nh_PIMLE = Nh_PIMLE)
    #names(sim)[dim(sim)[2]]  = str_c("Nh_PIMLE_",l)
    
    #sim = cbind(sim,Nh_PIMLEvis = Nh_PIMLEvis)
    #names(sim)[dim(sim)[2]] = str_c("Nh_PIMLEvis_",l)
    
    #sim = cbind(sim,Nh_MLE  = Nh_MLE)
    #names(sim)[dim(sim)[2]] = str_c("Nh_MLE_",l)
    
    #sim = cbind(sim,Nh_MLEvis = Nh_MLEvis)
    #names(sim)[dim(sim)[2]]   = str_c("Nh_MLEvis_",l)
    
    #sim = cbind(sim,Nh_MLE_mod = Nh_MLE_mod)
    #names(sim)[dim(sim)[2]] = str_c("Nh_MLE_mod_",l)
    
    #sim = cbind(sim,Nh_MLE_modvis = Nh_MLE_modvis)
    #names(sim)[dim(sim)[2]] = str_c("Nh_MLE_modvis_",l)
    
    #sim = cbind(sim,Nh_MoS  = Nh_MoS)
    #names(sim)[dim(sim)[2]] = str_c("Nh_MoS_",l)
    
    #sim = cbind(sim,Nh_MoSvis = Nh_MoSvis)
    #names(sim)[dim(sim)[2]]   = str_c("Nh_MoSvis_",l)
    
    #sim = cbind(sim,Nh_GNSUM = Nh_GNSUM)
    #names(sim)[dim(sim)[2]]  = str_c("Nh_GNSUM_",l)
    
    #sim = cbind(sim, Nh_TEO = Nh_TEO)
    #names(sim)[dim(sim)[2]] = str_c("Nh_TEO_",l)
    
    #sim = cbind(sim, Nh_TEOvis = Nh_TEOvis)
    #names(sim)[dim(sim)[2]] = str_c("Nh_TEOvis_",l)
    
    #sim = cbind(sim, Nh_Zheng = Nh_Zheng)
    #names(sim)[dim(sim)[2]] = str_c("Nh_Zheng_",l)
    
    #sim = cbind(sim, Nh_Zhengvis = Nh_Zhengvis)
    #names(sim)[dim(sim)[2]] = str_c("Nh_Zhengvis_",l)
    
    
    lista_sim[[l]] = sim
  }
  simulacion = bind_cols(lista_sim)
  lista_simulacion[[w]] = simulacion
  
  print(w)
  
}

simulaciones = bind_rows(lista_simulacion)
simulaciones["data"] = parameters

################################################################################
file_name = str_c("Simulations_memoryfactor_uniform_", seed_sim,".csv")
write.csv(simulaciones,               # Data frame 
          file = file_name,           # Csv name
          row.names = TRUE )          # Row names: TRUE or FALSE 
################################################################################

timer = Sys.time() - t
timer


####################### Network analysis #######################################
###### Links to the hidden population distribution & Degree distribution #######
plot_name = str_c("Network_memoryfactor_uniform_", seed_sim, ".png")

png(filename = plot_name,
    width = 1000, height = 1000)
net_analysis(net_sw, Population, p, 2*nei)
dev.off()



#################### COMPUTATION TIME ANALYSIS #################################
# Computation time (N=10000) (office PC) (length(parameters) = 50)
#timer ->  
################################################################################