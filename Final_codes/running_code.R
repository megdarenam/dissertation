
# Generating forecasts and residuals

rm(list = ls())
for (janela_T in c(60, 90 , 120)){
  
  setwd('\\Users\\megda\\OneDrive\\Dissertação')

  window_size <- janela_T

  source("\\Final_codes\\tese.R")

  save.image(
    paste0(
      "\\Users\\megda\\OneDrive\\Dissertação\\restante\\window_size_", window_size , ".RData"))

  rm(list = ls())

}

# Generating MCS tables

rm(list = ls())
for (popo in c('window_size_60.RData' , 'window_size_90.RData' , 'window_size_120.RData')){
  
  setwd('\\Users\\megda\\OneDrive\\Dissertação')

  load(paste0('\\forecasts\\' , popo))

  source("\\Final_codes\\generator.R")

  save.image(
    paste0("\\Users\\megda\\OneDrive\\Dissertação\\Workspaces\\", 'mcs_alpha25_correct_', popo))

  rm(list = ls())

}


# Creating boxplots MCS
rm(list = ls())
for (ws in c('window_size_60' ,'window_size_90' , 'window_size_120')){
  
  setwd('\\Users\\megda\\OneDrive\\Dissertação')

  load(paste0('\\Workspaces\\mcs_alpha25_correct_' , ws, ".RData"))

  source("\\Final_codes\\boxplots_generator.R")

  rm(list = ls())

}


# Creating tables periods
rm(list = ls())
for (rank in c('window_size_60' , 'window_size_90' , 'window_size_120')){
  
  setwd('\\Users\\megda\\OneDrive\\Dissertação')

  load(paste0('\\Workspaces\\mcs_alpha25_correct_' , rank, ".RData"))

  source("\\Final_codes\\tables_generator.R")

  rm(list = ls())

}







