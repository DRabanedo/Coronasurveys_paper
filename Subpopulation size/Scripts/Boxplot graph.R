##########################
library(ggplot2)
library(dplyr)
library(data.table)
##########################

################################################################################
# Functions #

tns = function(df){
  #transpose data frame
  df_t <- transpose(df)
  
  #redefine row and column names
  rownames(df_t) <- colnames(df)
  colnames(df_t) <- rownames(df)
  
  return(df_t)
}

################################################################################
data = read.csv("~/GitHub/Coronasurveys_paper/Subpopulation size/CSV/Simulations_subpopulationsize_notdisjoint_bay_207.csv")


df_MLE        = tns(dplyr::select(data, starts_with("Nh_MLE") ) )
df_MLE        = cbind('estimator' = rep('MLE',nrow(df_MLE)), df_MLE )
  
df_PIMLE      = tns(dplyr::select(data, starts_with("Nh_PIMLE") ) )
df_PIMLE      = cbind('estimator' = rep('PIMLE',nrow(df_PIMLE)), df_PIMLE )
  
df_MoS        = tns(dplyr::select(data, starts_with("Nh_MoS") ) )
df_MoS        = cbind('estimator' = rep('MoS',nrow(df_MoS)), df_MoS )
  
df_GNSUM      = tns(dplyr::select(data, starts_with("Nh_GNSUM") ) )
df_GNSUM      = cbind('estimator' = rep('GNSUM',nrow(df_GNSUM)), df_GNSUM )
  
#df_basic_sum  = tns(dplyr::select(data, starts_with("Nh_basic_sum") ) )
#df_basic_sum  = cbind('estimator' = rep('basic_sum',nrow(df_basic_sum)), df_basic_sum )
  
#df_basic_mean = tns(dplyr::select(data, starts_with("Nh_basic_mean") ) )
#df_basic_mean = cbind('estimator' = rep('basic_mean',nrow(df_basic_mean)), df_basic_mean )
  
df_Mod        = tns(dplyr::select(data, starts_with("Nh_Mod") ) ) 
df_Mod        = cbind('estimator' = rep('Mod',nrow(df_Mod)), df_Mod )
  
df_Zheng      = tns(dplyr::select(data, starts_with("Nh_Zheng") ) )
df_Zheng      = cbind('estimator' = rep('Zheng',nrow(df_Zheng)), df_Zheng )
  
df_TEO        = tns(dplyr::select(data, starts_with("Nh_TEO") ) )
df_TEO        = cbind('estimator' = rep('TEO',nrow(df_TEO)), df_TEO )


# Graph dataframe 
graph_data = rbind(df_MLE, df_PIMLE, df_MoS, df_GNSUM, df_Mod, df_Zheng, df_TEO)

colnames(graph_data) = c('estimator', 'A', 'B', 'C', 'D','E', 'F','G', 'H','I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q')
graph_data = melt(as.data.table(graph_data) )


# Graph plot
ggplot(graph_data, aes(x = variable, y = value, color = estimator)) + 
    geom_boxplot()



