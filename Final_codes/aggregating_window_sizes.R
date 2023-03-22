library(dplyr)
library(stringr)

### This code needs manual changes
### 

MSFE_period.3 <- MSFE_P.3_ranking[1:3 , ]
MSFE_period.4 <- MSFE_P.4_ranking[1:3 , ]
MSFE_period.5 <- MSFE_P.5_ranking[1:3 , ]
MSFE_period.6 <- MSFE_P.6_ranking[1:3 , ]
MSFE_period.7 <- MSFE_P.7_ranking[1:3 , ]

colnames(MSFE_P.3_ranking) <- paste0(colnames(MSFE_P.3_ranking) , "_WS120")
colnames(MSFE_P.4_ranking) <- paste0(colnames(MSFE_P.4_ranking) , "_WS120")
colnames(MSFE_P.5_ranking) <- paste0(colnames(MSFE_P.5_ranking) , "_WS120")
colnames(MSFE_P.6_ranking) <- paste0(colnames(MSFE_P.6_ranking) , "_WS120")
colnames(MSFE_P.7_ranking) <- paste0(colnames(MSFE_P.7_ranking) , "_WS120")

MSFE_period.3 <- bind_cols(MSFE_period.3 , MSFE_P.3_ranking[1:3 ,])
MSFE_period.4 <- bind_cols(MSFE_period.4 , MSFE_P.4_ranking[1:3 ,])
MSFE_period.5 <- bind_cols(MSFE_period.5 , MSFE_P.5_ranking[1:3 ,])
MSFE_period.6 <- bind_cols(MSFE_period.6 , MSFE_P.6_ranking[1:3 ,])
MSFE_period.7 <- bind_cols(MSFE_period.7 , MSFE_P.7_ranking[1:3 ,])

MSFE_3 <- as.data.frame(matrix( , nrow = 12 , ncol = 9))
MSFE_3[ , 1] <- t(MSFE_period.3[1 , 1:12])
MSFE_3[ , 2] <- t(MSFE_period.3[2 , 1:12])
MSFE_3[ , 3] <- t(MSFE_period.3[3 , 1:12])
MSFE_3[ , 4] <- t(MSFE_period.3[1 , 13:24])
MSFE_3[ , 5] <- t(MSFE_period.3[2 , 13:24])
MSFE_3[ , 6] <- t(MSFE_period.3[3 , 13:24])
MSFE_3[ , 7] <- t(MSFE_period.3[1 , 25:36])
MSFE_3[ , 8] <- t(MSFE_period.3[2 , 25:36])
MSFE_3[ , 9] <- t(MSFE_period.3[3 , 25:36])

MSFE_4 <- as.data.frame(matrix( , nrow = 12 , ncol = 9))
MSFE_4[ , 1] <- t(MSFE_period.4[1 , 1:12])
MSFE_4[ , 2] <- t(MSFE_period.4[2 , 1:12])
MSFE_4[ , 3] <- t(MSFE_period.4[3 , 1:12])
MSFE_4[ , 4] <- t(MSFE_period.4[1 , 13:24])
MSFE_4[ , 5] <- t(MSFE_period.4[2 , 13:24])
MSFE_4[ , 6] <- t(MSFE_period.4[3 , 13:24])
MSFE_4[ , 7] <- t(MSFE_period.4[1 , 25:36])
MSFE_4[ , 8] <- t(MSFE_period.4[2 , 25:36])
MSFE_4[ , 9] <- t(MSFE_period.4[3 , 25:36])

MSFE_5 <- as.data.frame(matrix( , nrow = 12 , ncol = 9))
MSFE_5[ , 1] <- t(MSFE_period.5[1 , 1:12])
MSFE_5[ , 2] <- t(MSFE_period.5[2 , 1:12])
MSFE_5[ , 3] <- t(MSFE_period.5[3 , 1:12])
MSFE_5[ , 4] <- t(MSFE_period.5[1 , 13:24])
MSFE_5[ , 5] <- t(MSFE_period.5[2 , 13:24])
MSFE_5[ , 6] <- t(MSFE_period.5[3 , 13:24])
MSFE_5[ , 7] <- t(MSFE_period.5[1 , 25:36])
MSFE_5[ , 8] <- t(MSFE_period.5[2 , 25:36])
MSFE_5[ , 9] <- t(MSFE_period.5[3 , 25:36])

MSFE_6 <- as.data.frame(matrix( , nrow = 12 , ncol = 9))
MSFE_6[ , 1] <- t(MSFE_period.6[1 , 1:12])
MSFE_6[ , 2] <- t(MSFE_period.6[2 , 1:12])
MSFE_6[ , 3] <- t(MSFE_period.6[3 , 1:12])
MSFE_6[ , 4] <- t(MSFE_period.6[1 , 13:24])
MSFE_6[ , 5] <- t(MSFE_period.6[2 , 13:24])
MSFE_6[ , 6] <- t(MSFE_period.6[3 , 13:24])
MSFE_6[ , 7] <- t(MSFE_period.6[1 , 25:36])
MSFE_6[ , 8] <- t(MSFE_period.6[2 , 25:36])
MSFE_6[ , 9] <- t(MSFE_period.6[3 , 25:36])

MSFE_7 <- as.data.frame(matrix( , nrow = 12 , ncol = 9))
MSFE_7[ , 1] <- t(MSFE_period.7[1 , 1:12])
MSFE_7[ , 2] <- t(MSFE_period.7[2 , 1:12])
MSFE_7[ , 3] <- t(MSFE_period.7[3 , 1:12])
MSFE_7[ , 4] <- t(MSFE_period.7[1 , 13:24])
MSFE_7[ , 5] <- t(MSFE_period.7[2 , 13:24])
MSFE_7[ , 6] <- t(MSFE_period.7[3 , 13:24])
MSFE_7[ , 7] <- t(MSFE_period.7[1 , 25:36])
MSFE_7[ , 8] <- t(MSFE_period.7[2 , 25:36])
MSFE_7[ , 9] <- t(MSFE_period.7[3 , 25:36])

colnames(MSFE_3) <- c("best_model_60" , "second_model_60" , "third_model_60",
                      "best_model_90" , "second_model_90" , "third_model_90",
                      "best_model_120" , "second_model_120" , "third_model_120")

colnames(MSFE_4) <- c("best_model_60" , "second_model_60" , "third_model_60",
                      "best_model_90" , "second_model_90" , "third_model_90",
                      "best_model_120" , "second_model_120" , "third_model_120")

colnames(MSFE_5) <- c("best_model_60" , "second_model_60" , "third_model_60",
                      "best_model_90" , "second_model_90" , "third_model_90",
                      "best_model_120" , "second_model_120" , "third_model_120")

colnames(MSFE_6) <- c("best_model_60" , "second_model_60" , "third_model_60",
                      "best_model_90" , "second_model_90" , "third_model_90",
                      "best_model_120" , "second_model_120" , "third_model_120")

colnames(MSFE_7) <- c("best_model_60" , "second_model_60" , "third_model_60",
                      "best_model_90" , "second_model_90" , "third_model_90",
                      "best_model_120" , "second_model_120" , "third_model_120")

write.csv(MSFE_3 , file = "\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\MSFE_PERIOD3.csv")
write.csv(MSFE_4 , file = "\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\MSFE_PERIOD4.csv")
write.csv(MSFE_5 , file = "\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\MSFE_PERIOD5.csv")
write.csv(MSFE_6 , file = "\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\MSFE_PERIOD6.csv")
write.csv(MSFE_7 , file = "\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\MSFE_PERIOD7.csv")


save.image("MSFE_AGREGATED.RData")

########################################################

popo <- 'window_size_60.RData'

load(paste0('\\Users\\megda\\OneDrive\\Dissertação\\restante\\' , popo))

for (i in 1:(horizonte)){
  
  df <- paste("df" , i , sep = ".")
  
  
  interative_aux_AR13 <-
    t(interative_level_sample_AR13[window_size + i  ,
                                   1:ncol(interative_level_sample_AR13)])
  
  interative_aux_AR1 <-
    t(interative_level_sample_AR1[window_size + i  ,
                                  1:ncol(interative_level_sample_AR1)])
  
  interative_aux_DDD <-
    t(interative_level_sample_DDD[window_size+i  ,
                                  1:ncol(interative_level_sample_DDD)])
  
  interative_aux_DDD_AR <-
    t(interative_level_sample_DDD_AR[window_size+i  ,
                                     1:ncol(interative_level_sample_DDD_AR)])
  
  #interative_aux_DDD_AR_LASSO <-
  # t(interative_level_sample_DDD_AR_LASSO[window_size+i  ,
  # 1:ncol(interative_level_sample_DDD_AR_LASSO)])
  
  #interative_aux_DDD_AR_ADALASSO <-
  #t(interative_level_sample_DDD_AR_ADALASSO[window_size+i  ,
  #1:ncol(interative_level_sample_DDD_AR_ADALASSO)])
  
  interative_aux_SDD <-
    t(interative_level_sample_SDD[window_size+i ,
                                  1:ncol(interative_level_sample_SDD)])
  
  interative_aux_AR13_IC <-
    t(interative_level_sample_AR13_IC[window_size + i  ,
                                      1:ncol(interative_level_sample_AR13_IC)])
  
  interative_aux_cluster <-
    t(interative_level_sample_cluster[window_size + i  ,
                                      1:ncol(interative_level_sample_cluster)])
  
  interative_aux_cluster_combination <-
    t(interative_level_sample_cluster_combination[window_size + i  ,
                                                  1:ncol(interative_level_sample_cluster_combination)])
  
  interative_aux_cluster_rls <-
    t(interative_level_sample_cluster_rls[window_size + i  ,
                                          1:ncol(interative_level_sample_cluster_rls)])
  
  interative_aux_cluster_rls_combination <-
    t(interative_level_sample_cluster_rls_combination[window_size + i  ,
                                                      1:ncol(interative_level_sample_cluster_rls_combination)])
  
  interative_aux_combination <-
    t(interative_level_sample_combination[window_size + i  ,
                                          1:ncol(interative_level_sample_combination)])
  
  interative_aux_combination_non_robust <-
    t(interative_level_sample_combination_non_robust[window_size + i  ,
                                                     1:ncol(interative_level_sample_combination_non_robust)])
  
  interative_aux_combination_robust <-
    t(interative_level_sample_combination_robust[window_size + i  ,
                                                 1:ncol(interative_level_sample_combination_robust)])
  
  #interative_aux_tvAR <-
  #t(level_sample_tvVAR[window_size + i  ,
  #1:ncol(level_sample_tvVAR)])
  aux_AR13 <-
    t(level_sample_AR13[window_size + i  , 1:ncol(level_sample_AR13)])
  
  aux_AR1 <-
    t(level_sample_AR1[window_size + i  , 1:ncol(level_sample_AR1)])
  
  aux_RW <-
    t(level_sample_RW[window_size+i , 1:ncol(level_sample_RW)])
  
  aux_DDD <-
    t(level_sample_DDD[window_size+i  , 1:ncol(level_sample_DDD)])
  
  aux_SDD <-
    t(level_sample_SDD[window_size+i  , 1:ncol(level_sample_DDD)])
  
  
  aux_AR13_IC <-
    t(level_sample_AR13_IC[window_size + i  , 1:ncol(level_sample_AR13_IC)])
  
  aux_cluster <-
    t(level_sample_cluster[window_size + i  , 1:ncol(level_sample_cluster)])
  
  aux_cluster_combination <-
    t(level_sample_cluster_combination[window_size + i  , 1:ncol(level_sample_cluster_combination)])
  
  aux_combination <-
    t(level_sample_combination[window_size + i  ,
                               1:ncol(level_sample_combination)])
  
  aux_combination_non_robust <-
    t(level_sample_combination_non_robust[window_size + i  ,
                                          1:ncol(level_sample_combination_non_robust)])
  
  aux_combination_robust <-
    t(level_sample_combination_robust[window_size + i  ,
                                      1:ncol(level_sample_combination_robust)])
  
  aux_lasso_AR13 <-
    t(level_sample_lasso_AR13[window_size + i  ,
                              1:ncol(level_sample_lasso_AR13)])
  
  aux_adalasso_AR13 <-
    t(level_sample_adalasso_AR13[window_size + i  ,
                                 1:ncol(level_sample_adalasso_AR13)])
  
  aux_cluster_rls <-
    t(level_sample_cluster_rls[window_size + i  ,
                               1:ncol(level_sample_cluster_rls)])
  
  aux_cluster_rls_combination <-
    t(level_sample_cluster_rls_combination[window_size + i  ,
                                           1:ncol(level_sample_cluster_rls_combination)])
  
  aux_oxmetrix <- t(dados_oxmetrics[window_size + i  ,
                                    1:ncol(dados_oxmetrics)])
  
  
  assign(df , data.frame(aux_AR13 , aux_RW , aux_DDD,
                         aux_AR13_IC , aux_cluster , aux_SDD , aux_combination ,
                         aux_cluster_rls,
                         interative_aux_AR13  ,
                         interative_aux_DDD ,interative_aux_SDD ,
                         interative_aux_AR13_IC ,
                         interative_aux_cluster ,
                         interative_aux_combination ,
                         #interative_aux_tvAR ,
                         aux_lasso_AR13 , aux_oxmetrix,
                         interative_aux_DDD_AR , interative_aux_AR1,
                         aux_adalasso_AR13, aux_AR1 ,
                         interative_aux_combination_non_robust,
                         interative_aux_combination_robust, aux_combination_robust,
                         aux_combination_non_robust,
                         interative_aux_cluster_combination , aux_cluster_combination,
                         interative_aux_cluster_rls_combination , aux_cluster_rls_combination,
                         interative_aux_cluster_rls
  ))
  
  print(i)
  
}

models <- c("AR 13" , "RW" , "DDD AR 1"  , "AR + IC"
            , "Cluster Pinto-Castle" , "SDD AR 1" , "Combination SDD + DDD" ,
            "Cluster RLS",
            "AR 13 (I)"  ,
            "DDD AR 1 (I)" , "SDD AR 1 (I)" ,
            "AR + IC (I)", "Cluster Pinto-Castle (I)" ,
            "Combination SDD + DDD (I)" ,
            #"tvp_AR" ,
            "Lasso AR 13 (I)" , "Autometrics AR 13 (I)" , "DDD AR 13 (I)"
            , "AR 1 (I)",
            "Adalasso AR 13 (I)" , "AR 1" , "Combination Non-Robust (I)" ,
            "Combination Robust (I)" , "Combination Robust" ,
            "Combination Non-Robust" , "Cluster Pinto-Castle Combination (I)" ,
            "Cluster Pinto-Castle Combination" , "Cluster RLS Combination (I)", "Cluster RLS Combination",
            "Cluster RLS (I)")

colnames(df.1) <- models

colnames(df.2) <- models

colnames(df.3) <- models

colnames(df.4) <- models

colnames(df.5) <- models

colnames(df.6) <- models

colnames(df.7) <- models

colnames(df.8) <- models

colnames(df.9) <- models

colnames(df.10) <- models

colnames(df.11) <- models

colnames(df.12) <- models

for(i in 1:horizonte){
  
  aux_obs <- paste("horiz" , i , sep = ".")
  
  assign(aux_obs ,
         replicate(data$prod_ind[(window_size+i):(window_size+i+repetitions)] ,
                   n = length(df.1)) |>
           as.data.frame())
  
}

residuals.1_60 <- (df.1 - horiz.1)^2
residuals.2_60 <- (df.2 - horiz.2)^2
residuals.3_60 <- (df.3 - horiz.3)^2
residuals.4_60 <- (df.4 - horiz.4)^2
residuals.5_60 <- (df.5 - horiz.5)^2
residuals.6_60 <- (df.6 - horiz.6)^2
residuals.7_60 <- (df.7 - horiz.7)^2
residuals.8_60 <- (df.8 - horiz.8)^2
residuals.9_60 <- (df.9 - horiz.9)^2
residuals.10_60 <- (df.10 - horiz.10)^2
residuals.11_60 <- (df.11 - horiz.11)^2
residuals.12_60 <- (df.12 - horiz.12)^2

colnames(residuals.1_60) <- models

residuals.1_60.mod <- residuals.1_60[!duplicated(as.list(residuals.1_60))]

colnames(residuals.2_60) <- models

colnames(residuals.3_60) <- models

colnames(residuals.4_60) <- models

colnames(residuals.5_60) <- models

colnames(residuals.6_60) <- models

colnames(residuals.7_60) <- models

colnames(residuals.8_60) <- models

colnames(residuals.9_60) <- models

colnames(residuals.10_60) <- models

colnames(residuals.11_60) <- models

colnames(residuals.12_60) <- models

save.image("\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\residuals.RData")


## deixando apenas o modelo RW para 120 observações


residuals.1_60.mod <- residuals.1_60.mod[ , -2]
residuals.1_90.mod <- residuals.1_90.mod[ , -2]
residuals.1_60 <- residuals.1_60[ , -2]
residuals.1_90 <- residuals.1_90[ , -2]
residuals.2_60 <- residuals.2_60[ , -2]
residuals.2_90 <- residuals.2_90[ , -2]
residuals.3_60 <- residuals.3_60[ , -2]
residuals.3_90 <- residuals.3_90[ , -2]
residuals.4_60 <- residuals.4_60[ , -2]
residuals.4_90 <- residuals.4_90[ , -2]
residuals.5_60 <- residuals.5_60[ , -2]
residuals.5_90 <- residuals.5_90[ , -2]
residuals.6_60 <- residuals.6_60[ , -2]
residuals.6_90 <- residuals.6_90[ , -2]
residuals.7_60 <- residuals.7_60[ , -2]
residuals.7_90 <- residuals.7_90[ , -2]
residuals.8_60 <- residuals.8_60[ , -2]
residuals.8_90 <- residuals.8_90[ , -2]
residuals.9_60 <- residuals.9_60[ , -2]
residuals.9_90 <- residuals.9_90[ , -2]
residuals.10_60 <- residuals.10_60[ , -2]
residuals.10_90 <- residuals.10_90[ , -2]
residuals.11_60 <- residuals.11_60[ , -2]
residuals.11_90 <- residuals.11_90[ , -2]
residuals.12_60 <- residuals.12_60[ , -2]
residuals.12_90 <- residuals.12_90[ , -2]


colnames(residuals.1_60) <- paste0(colnames(residuals.1_60) , "_WS60")
colnames(residuals.1_60.mod) <- paste0(colnames(residuals.1_60.mod) , "_WS60")
colnames(residuals.2_60) <- paste0(colnames(residuals.2_60) , "_WS60")
colnames(residuals.3_60) <- paste0(colnames(residuals.3_60) , "_WS60")
colnames(residuals.4_60) <- paste0(colnames(residuals.4_60) , "_WS60")
colnames(residuals.5_60) <- paste0(colnames(residuals.5_60) , "_WS60")
colnames(residuals.6_60) <- paste0(colnames(residuals.6_60) , "_WS60")
colnames(residuals.7_60) <- paste0(colnames(residuals.7_60) , "_WS60")
colnames(residuals.8_60) <- paste0(colnames(residuals.8_60) , "_WS60")
colnames(residuals.9_60) <- paste0(colnames(residuals.9_60) , "_WS60")
colnames(residuals.10_60) <- paste0(colnames(residuals.10_60) , "_WS60")
colnames(residuals.11_60) <- paste0(colnames(residuals.11_60) , "_WS60")
colnames(residuals.12_60) <- paste0(colnames(residuals.12_60) , "_WS60")

colnames(residuals.1_90) <- paste0(colnames(residuals.1_90) , "_WS90")
colnames(residuals.1_90.mod) <- paste0(colnames(residuals.1_90.mod) , "_WS90")
colnames(residuals.2_90) <- paste0(colnames(residuals.2_90) , "_WS90")
colnames(residuals.3_90) <- paste0(colnames(residuals.3_90) , "_WS90")
colnames(residuals.4_90) <- paste0(colnames(residuals.4_90) , "_WS90")
colnames(residuals.5_90) <- paste0(colnames(residuals.5_90) , "_WS90")
colnames(residuals.6_90) <- paste0(colnames(residuals.6_90) , "_WS90")
colnames(residuals.7_90) <- paste0(colnames(residuals.7_90) , "_WS90")
colnames(residuals.8_90) <- paste0(colnames(residuals.8_90) , "_WS90")
colnames(residuals.9_90) <- paste0(colnames(residuals.9_90) , "_WS90")
colnames(residuals.10_90) <- paste0(colnames(residuals.10_90) , "_WS90")
colnames(residuals.11_90) <- paste0(colnames(residuals.11_90) , "_WS90")
colnames(residuals.12_90) <- paste0(colnames(residuals.12_90) , "_WS90")

colnames(residuals.1_120) <- paste0(colnames(residuals.1_120) , "_WS120")
colnames(residuals.1_120.mod) <- paste0(colnames(residuals.1_120.mod) , "_WS120")
colnames(residuals.2_120) <- paste0(colnames(residuals.2_120) , "_WS120")
colnames(residuals.3_120) <- paste0(colnames(residuals.3_120) , "_WS120")
colnames(residuals.4_120) <- paste0(colnames(residuals.4_120) , "_WS120")
colnames(residuals.5_120) <- paste0(colnames(residuals.5_120) , "_WS120")
colnames(residuals.6_120) <- paste0(colnames(residuals.6_120) , "_WS120")
colnames(residuals.7_120) <- paste0(colnames(residuals.7_120) , "_WS120")
colnames(residuals.8_120) <- paste0(colnames(residuals.8_120) , "_WS120")
colnames(residuals.9_120) <- paste0(colnames(residuals.9_120) , "_WS120")
colnames(residuals.10_120) <- paste0(colnames(residuals.10_120) , "_WS120")
colnames(residuals.11_120) <- paste0(colnames(residuals.11_120) , "_WS120")
colnames(residuals.12_120) <- paste0(colnames(residuals.12_120) , "_WS120")

residuals.1_60 <- residuals.1_60[-1:-60 , ]
residuals.1_60.mod <- residuals.1_60.mod[-1:-60 , ]
residuals.2_60 <- residuals.2_60[-1:-60 , ]
residuals.3_60 <- residuals.3_60[-1:-60 , ]
residuals.4_60 <- residuals.4_60[-1:-60 , ]
residuals.5_60 <- residuals.5_60[-1:-60 , ]
residuals.6_60 <- residuals.6_60[-1:-60 , ]
residuals.7_60 <- residuals.7_60[-1:-60 , ]
residuals.8_60 <- residuals.8_60[-1:-60 , ]
residuals.9_60 <- residuals.9_60[-1:-60 , ]
residuals.10_60 <- residuals.10_60[-1:-60 , ]
residuals.11_60 <- residuals.11_60[-1:-60 , ]
residuals.12_60 <- residuals.12_60[-1:-60 , ]

residuals.1_90 <- residuals.1_90[-1:-30 , ]
residuals.1_90.mod <- residuals.1_90.mod[-1:-30 , ]
residuals.2_90 <- residuals.2_90[-1:-30 , ]
residuals.3_90 <- residuals.3_90[-1:-30 , ]
residuals.4_90 <- residuals.4_90[-1:-30 , ]
residuals.5_90 <- residuals.5_90[-1:-30 , ]
residuals.6_90 <- residuals.6_90[-1:-30 , ]
residuals.7_90 <- residuals.7_90[-1:-30 , ]
residuals.8_90 <- residuals.8_90[-1:-30 , ]
residuals.9_90 <- residuals.9_90[-1:-30 , ]
residuals.10_90 <- residuals.10_90[-1:-30 , ]
residuals.11_90 <- residuals.11_90[-1:-30 , ]
residuals.12_90 <- residuals.12_90[-1:-30 , ]

residuals.1 <- data.frame(residuals.1_60 , residuals.1_90 , residuals.1_120)
residuals.1.mod <- data.frame(residuals.1_60.mod , residuals.1_90.mod , residuals.1_120.mod)
residuals.2 <- data.frame(residuals.2_60, residuals.2_90 , residuals.2_120)
residuals.3 <- data.frame(residuals.3_60, residuals.3_90 , residuals.3_120)
residuals.4 <- data.frame(residuals.4_60, residuals.4_90 , residuals.4_120)
residuals.5 <- data.frame(residuals.5_60, residuals.5_90 , residuals.5_120)
residuals.6 <- data.frame(residuals.6_60, residuals.6_90 , residuals.6_120)
residuals.7 <- data.frame(residuals.7_60, residuals.7_90 , residuals.7_120)
residuals.8 <- data.frame(residuals.8_60, residuals.8_90 , residuals.8_120)
residuals.9 <- data.frame(residuals.9_60, residuals.9_90 , residuals.9_120)
residuals.10 <- data.frame(residuals.10_60, residuals.10_90 , residuals.10_120)
residuals.11 <- data.frame(residuals.11_60, residuals.11_90 , residuals.11_120)
residuals.12 <- data.frame(residuals.12_60, residuals.12_90 , residuals.12_120)

rm(list=ls()[! ls() %in% c('residuals.1' ,'residuals.1.mod' , 'residuals.2' , 'residuals.3'
                           , 'residuals.4' , 'residuals.5' , 'residuals.6'
                           , 'residuals.7' , 'residuals.8' , 'residuals.9'
                           , 'residuals.10' , 'residuals.11' , 'residuals.12'
)])

save.image("\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\residuals_final.RData")