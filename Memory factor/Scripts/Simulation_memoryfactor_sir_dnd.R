################################################################################################################
# Simulation based on the value of the memory factor of the reach variable, leaving the rest of parameters fixed
################################################################################################################

t = Sys.time()

#####################
## Simulation data ##
#####################

N = 10000                     # Population size
v_pop_prob = rep(1/10, 5)     # Probability of each subpopulation
n_pop = length(v_pop_prob)    # Number of subpopulations
n_survey = 500                # Number of individuals we draw in the survey
n_survey_hp = 50              # Number of individuals we draw in the hidden population survey 

sub_memory_factor = 0         # Subpopulation memory factor (parameter to change variance of the perturbations' normal)
visibility_factor = 1         # Visibility factor (Binomial's probability)
memory_factor = 0             #reach memory factor (parameter to change variance of the perturbations' normal)

seed = 207                    # Seed
set.seed(seed)

#Graph
dim = 1      # Graph dimension 
nei = 50     # Number of neighbors that each node is connected to. They are neighbors on each side of the node, so they are 2*nei connections
             # before applying the randomization.
p   = 0.1    # Probability of randomize a connection. It is applied to all connections


###############################################################################################################################################################
# Network model #
net_model = sample_smallworld(dim, N, nei, p, loops = FALSE, multiple = FALSE)

## Populations models ##
# Not disjoint population #
Graph_population_matrix = gen_Data_uniform(N, v_pop_prob, visibility_factor, memory_factor, sub_memory_factor, net = net_model)

net_sw     = Graph_population_matrix[[1]]   # Population´s graph
Population = Graph_population_matrix[[2]]   # Population
Mhp_vis    = Graph_population_matrix[[3]]   # Population's visibility matrix

# Population number
v_pop_total = getV_pop(n_pop, Population)

################################################################################

## Auxiliary data for the simulation ##

# Study parameters
parameters = seq(from = 0, to = 1, length.out = 50)

#Dataframe to save the data
simulaciones = data.frame(data = parameters)

# Variables reach vector
vect_reach    =  Population$reach
vect_hp       =  Population$hp_total
vect_hp_vis   =  rep(NA, nrow(Population))
vect_reach_re =  rep(NA, nrow(Population))

#Number of iterations for the simulation
b = 100

lista_simulacion = list()

################################################################################

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

for (w in 1:length(parameters)) {
  ## Parameter implementation ##
  memory_factor = parameters[w]  
  
  vect_reach_re = gen_Reach_memory(Population, memory_factor)$reach_memory
  vect_hp_vis   = gen_Reach_hp_memory(Population, Mhp_vis, memory_factor)$hp_survey
  
  Population$reach_memory = vect_reach_re
  Population$hp_survey    = vect_hp_vis
  
  ##########################################  
  ##   Not disjoint population analysis   ##
  
  ## Variable reset ##
  
  Nh_real = rep(NA,b) 
  
  Nh_basic_sum      = rep(NA,b) 
  #Nh_basicvis_sum  = rep(NA,b) 
  Nh_basic_mean     = rep(NA,b) 
  #Nh_basicvis_mean = rep(NA,b)                                      
  
  #Nh_PIMLE    = rep(NA,b) 
  #Nh_PIMLEvis = rep(NA,b) 
  
  #Nh_MLE    = rep(NA,b) 
  #Nh_MLEvis = rep(NA,b) 
  
  #Nh_MoS    = rep(NA,b) 
  #Nh_MoSvis = rep(NA,b) 
  
  #Nh_GNSUM = rep(NA,b) 
  
  #Nh_MLE_mod     = rep(NA,b) 
  #Nh_MLE_modvis = rep(NA,b)
  
  #Nh_Teo = getNh_Teo(survey,knowpopulation_data,NITERATION)
  #Nh_overdispersed = getNh_overdispersed(survey, v_pop_total,N, warmup,iterations,chains=1)
  
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
    
    #Nh_MLE_mod  = getNh_MLE_mod(survey, v_pop_total, N)
    #Nh_Mod_modvis  = getNh_MoSvis(survey, v_pop_total, N, vf_estimate)
    
    #Nh_MoS     = getNh_MoS(survey, v_pop_total, N)
    #Nh_MoSvis  = getNh_MoSvis(survey, v_pop_total, N, vf_estimate)
    
    #Nh_GNSUM   =  getNh_GNSUM(survey, survey_hp, v_pop_total, N)
    
    #Nh_Teo           = getNh_Teo(survey,v_pop_total)
    #Nh_overdispersed = getNh_overdispersed(survey, v_pop_total,N)
    
    
    
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
    
    #sim = cbind(sim,Nh_Teo = Nh_Teo)
    #names(sim)[dim(sim)[2]] = str_c("Nh_Teo_",l)
    
    #sim = cbind(sim,Nh_Overdispersed = Nh_Overdispersed)
    #names(sim)[dim(sim)[2]] = str_c("Nh_Overdispersed_",l)
    
    lista_sim[[l]] = sim
  }
  simulacion = bind_cols(lista_sim)
  lista_simulacion[[w]] = simulacion
  
  print(w)
  
}

simulaciones = bind_rows(lista_simulacion)
simulaciones["data"] = parameters

################################################################################
file_name = str_c("Simulations_memoryfactor_", seed,".csv")
write.csv(simulaciones,               # Data frame 
          file = file_name,           # Csv name
          row.names = TRUE )          # Row names: TRUE or FALSE 
################################################################################

timer = Sys.time() - t
timer


####################### Network analysis #######################################
###### Links to the hidden population distribution & Degree distribution #######
plot_name = str_c("Network_memoryfactor_", seed, ".png")

png(filename = plot_name,
    width = 1000, height = 1000)
net_analysis(net_sw, Population, p, 2*nei)
dev.off()



#################### COMPUTATION TIME ANALYSIS #################################
# Computation time (N=10000) (office PC) (length(parameters) = 50)
#timer ->  
################################################################################