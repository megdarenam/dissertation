# Comparando Modelos

## Gerando resíduos ao quadrado para cada horizonte

library(modelconf)
library(MCS) #mcs test
library(forecast) #diebold and mariano test

bootstrap <- 25000
block_length <- 2
indice_max <- 3
ALPHA <- 0.25

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

residuals.1 <- (df.1 - horiz.1)^2
residuals.2 <- (df.2 - horiz.2)^2
residuals.3 <- (df.3 - horiz.3)^2
residuals.4 <- (df.4 - horiz.4)^2
residuals.5 <- (df.5 - horiz.5)^2
residuals.6 <- (df.6 - horiz.6)^2
residuals.7 <- (df.7 - horiz.7)^2
residuals.8 <- (df.8 - horiz.8)^2
residuals.9 <- (df.9 - horiz.9)^2
residuals.10 <- (df.10 - horiz.10)^2
residuals.11 <- (df.11 - horiz.11)^2
residuals.12 <- (df.12 - horiz.12)^2

colnames(residuals.1) <- models

residuals.1.mod <- residuals.1[!duplicated(as.list(residuals.1))]

colnames(residuals.2) <- models

colnames(residuals.3) <- models

colnames(residuals.4) <- models

colnames(residuals.5) <- models

colnames(residuals.6) <- models

colnames(residuals.7) <- models

colnames(residuals.8) <- models

colnames(residuals.9) <- models

colnames(residuals.10) <- models

colnames(residuals.11) <- models

colnames(residuals.12) <- models



## MSFE


### MSFE horizonte = 1


MSFE_1_1 <- as.data.frame((colMeans(residuals.1))^(1/2))


## Dataframe para salvar as estatísticas por recorte de tempo

MSFE_P.1 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MSFE_P.1) <- colnames(residuals.1)

## Dataframe para salvar as estatísticas por horizonte (1 , 3, 6, 9, 12)

MSFE_H.1 <- data.frame(matrix(nrow = 7 , ncol = ncol(residuals.1)))

colnames(MSFE_H.1) <- colnames(residuals.1)

for (i in 1:nrow(MSFE_1_1)){

  MSFE_P.1[1 ,colnames(MSFE_P.1) == rownames(MSFE_1_1)[i]] <-
    MSFE_1_1$`(colMeans(residuals.1))^(1/2)`[i]

  MSFE_H.1[1 ,colnames(MSFE_H.1) == rownames(MSFE_1_1)[i]] <-
    MSFE_1_1$`(colMeans(residuals.1))^(1/2)`[i]

}





### MSFE horizonte = 2

MSFE_2_1 <- as.data.frame((colMeans(residuals.2))^(1/2))


for (i in 1:nrow(MSFE_2_1)){

  MSFE_P.1[2 ,colnames(MSFE_P.1) == rownames(MSFE_2_1)[i]] <-
    MSFE_2_1$`(colMeans(residuals.2))^(1/2)`[i]

}



### MSFE horizonte = 3

MSFE_3_1 <- as.data.frame((colMeans(residuals.3))^(1/2))


## Dataframe para salvar as estatísticas por horizonte (1 , 3, 6, 9, 12)

MSFE_H.3 <- data.frame(matrix(nrow = 7 , ncol = ncol(residuals.1)))

colnames(MSFE_H.3) <- colnames(residuals.1)

for (i in 1:nrow(MSFE_3_1)){

  MSFE_P.1[3 ,colnames(MSFE_P.1) == rownames(MSFE_3_1)[i]] <-
    MSFE_3_1$`(colMeans(residuals.3))^(1/2)`[i]

  MSFE_H.3[1 ,colnames(MSFE_H.3) == rownames(MSFE_3_1)[i]] <-
    MSFE_3_1$`(colMeans(residuals.3))^(1/2)`[i]

}



### MSFE horizonte = 4

MSFE_4_1 <- as.data.frame((colMeans(residuals.4))^(1/2))


for (i in 1:nrow(MSFE_4_1)){

  MSFE_P.1[4 ,colnames(MSFE_P.1) == rownames(MSFE_4_1)[i]] <-
    MSFE_4_1$`(colMeans(residuals.4))^(1/2)`[i]

}



### MSFE horizonte = 5


MSFE_5_1 <- as.data.frame((colMeans(residuals.5))^(1/2))


for (i in 1:nrow(MSFE_5_1)){

  MSFE_P.1[5 ,colnames(MSFE_P.1) == rownames(MSFE_5_1)[i]] <-
    MSFE_5_1$`(colMeans(residuals.5))^(1/2)`[i]

}



### MSFE horizonte = 6

MSFE_6_1 <- as.data.frame((colMeans(residuals.6))^(1/2))


## Dataframe para salvar as estatísticas por horizonte (1 , 3, 6, 9, 12)

MSFE_H.6 <- data.frame(matrix(nrow = 7 , ncol = ncol(residuals.1)))

colnames(MSFE_H.6) <- colnames(residuals.1)

for (i in 1:nrow(MSFE_6_1)){

  MSFE_P.1[6 ,colnames(MSFE_P.1) == rownames(MSFE_6_1)[i]] <-
    MSFE_6_1$`(colMeans(residuals.6))^(1/2)`[i]

  MSFE_H.6[1 ,colnames(MSFE_H.6) == rownames(MSFE_6_1)[i]] <-
    MSFE_6_1$`(colMeans(residuals.6))^(1/2)`[i]

}



### MSFE horizonte = 7

MSFE_7_1 <- as.data.frame((colMeans(residuals.7))^(1/2))


for (i in 1:nrow(MSFE_7_1)){

  MSFE_P.1[7 ,colnames(MSFE_P.1) == rownames(MSFE_7_1)[i]] <-
    MSFE_7_1$`(colMeans(residuals.7))^(1/2)`[i]

}



### MSFE horizonte = 8

MSFE_8_1 <- as.data.frame((colMeans(residuals.8))^(1/2))


for (i in 1:nrow(MSFE_8_1)){

  MSFE_P.1[8 ,colnames(MSFE_P.1) == rownames(MSFE_8_1)[i]] <-
    MSFE_8_1$`(colMeans(residuals.8))^(1/2)`[i]

}



### MSFE horizonte = 9

MSFE_9_1 <- as.data.frame((colMeans(residuals.9))^(1/2))


## Dataframe para salvar as estatísticas por horizonte (1 , 3, 6, 9, 12)

MSFE_H.9 <- data.frame(matrix(nrow = 7 , ncol = ncol(residuals.1)))

colnames(MSFE_H.9) <- colnames(residuals.1)

for (i in 1:nrow(MSFE_9_1)){

  MSFE_P.1[9 ,colnames(MSFE_P.1) == rownames(MSFE_9_1)[i]] <-
    MSFE_9_1$`(colMeans(residuals.9))^(1/2)`[i]

  MSFE_H.9[1 ,colnames(MSFE_H.9) == rownames(MSFE_9_1)[i]] <-
    MSFE_9_1$`(colMeans(residuals.9))^(1/2)`[i]

}



### MSFE horizonte = 10

MSFE_10_1 <- as.data.frame((colMeans(residuals.10))^(1/2))


for (i in 1:nrow(MSFE_10_1)){

  MSFE_P.1[10 ,colnames(MSFE_P.1) == rownames(MSFE_10_1)[i]] <-
    MSFE_10_1$`(colMeans(residuals.10))^(1/2)`[i]

}



### MSFE horizonte = 11

MSFE_11_1 <- as.data.frame((colMeans(residuals.11))^(1/2))


for (i in 1:nrow(MSFE_11_1)){

  MSFE_P.1[11 ,colnames(MSFE_P.1) == rownames(MSFE_11_1)[i]] <-
    MSFE_11_1$`(colMeans(residuals.11))^(1/2)`[i]

}



### MSFE horizonte = 12

MSFE_12_1 <- as.data.frame((colMeans(residuals.12))^(1/2))


## Dataframe para salvar as estatísticas por horizonte (1 , 3, 6, 9, 12)

MSFE_H.12 <- data.frame(matrix(nrow = 7 , ncol = ncol(residuals.1)))

colnames(MSFE_H.12) <- colnames(residuals.1)

for (i in 1:nrow(MSFE_12_1)){

  MSFE_P.1[12 ,colnames(MSFE_P.1) == rownames(MSFE_12_1)[i]] <-
    MSFE_12_1$`(colMeans(residuals.12))^(1/2)`[i]

  MSFE_H.12[1 ,colnames(MSFE_H.12) == rownames(MSFE_12_1)[i]] <-
    MSFE_12_1$`(colMeans(residuals.12))^(1/2)`[i]

}



## MCS
### MCS Horizonte = 1


MCS_1_1 <- data.frame(estMCS(residuals.1.mod , B = bootstrap , l = block_length))

ordernacao <- order(MCS_1_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_1_1 <- MCS_1_1[MCS_1_1$MCS.p.val >= alpha , ]

df_MCS_1_1 <- MCS_1_1


## Dataframe para salvar as estatísticas por recorte de tempo

MCS_STATS_P.1 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MCS_STATS_P.1) <- colnames(residuals.1)


for (i in 1:nrow(df_MCS_1_1)){

  MCS_STATS_P.1[1 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_1_1[i, ])] <-
    df_MCS_1_1$MCS.p.val[i]

}



### MCS Horizonte = 2


MCS_2_1 <- data.frame(estMCS(residuals.2 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_2_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_2_1 <- MCS_2_1[MCS_2_1$MCS.p.val >= alpha , ]

df_MCS_2_1 <- MCS_2_1



for (i in 1:nrow(df_MCS_2_1)){

  MCS_STATS_P.1[2 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_2_1[i, ])] <-
    df_MCS_2_1$MCS.p.val[i]

}


### MCS Horizonte = 3

MCS_3_1 <- data.frame(estMCS(residuals.3 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_3_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_3_1 <- MCS_3_1[MCS_3_1$MCS.p.val >= alpha , ]

df_MCS_3_1 <- MCS_3_1


for (i in 1:nrow(df_MCS_3_1)){

  MCS_STATS_P.1[3 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_3_1[i, ])] <-
    df_MCS_3_1$MCS.p.val[i]

}



## MCS 4



MCS_4_1 <- data.frame(estMCS(residuals.4 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_4_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_4_1 <- MCS_4_1[MCS_4_1$MCS.p.val >= alpha , ]

df_MCS_4_1 <- MCS_4_1



for (i in 1:nrow(df_MCS_4_1)){

  MCS_STATS_P.1[4 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_4_1[i, ])] <-
    df_MCS_4_1$MCS.p.val[i]

}



## MCS 5

MCS_5_1 <- data.frame(estMCS(residuals.5 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_5_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_5_1 <- MCS_5_1[MCS_5_1$MCS.p.val >= alpha , ]

df_MCS_5_1 <- MCS_5_1


for (i in 1:nrow(df_MCS_5_1)){

  MCS_STATS_P.1[5 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_5_1[i, ])] <-
    df_MCS_5_1$MCS.p.val[i]

}



### MCS Horizonte = 6


MCS_6_1 <- data.frame(estMCS(residuals.6 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_6_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_6_1 <- MCS_6_1[MCS_6_1$MCS.p.val >= alpha , ]

df_MCS_6_1 <- MCS_6_1


for (i in 1:nrow(df_MCS_6_1)){

  MCS_STATS_P.1[6 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_6_1[i, ])] <-
    df_MCS_6_1$MCS.p.val[i]

}



## MCS 7


MCS_7_1 <- data.frame(estMCS(residuals.7 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_7_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_7_1 <- MCS_7_1[MCS_7_1$MCS.p.val >= alpha , ]

df_MCS_7_1 <- MCS_7_1



for (i in 1:nrow(df_MCS_7_1)){

  MCS_STATS_P.1[7 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_7_1[i, ])] <-
    df_MCS_7_1$MCS.p.val[i]

}



## MCS 8


MCS_8_1 <- data.frame(estMCS(residuals.8 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_8_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_8_1 <- MCS_8_1[MCS_8_1$MCS.p.val >= alpha , ]

df_MCS_8_1 <- MCS_8_1


for (i in 1:nrow(df_MCS_8_1)){

  MCS_STATS_P.1[8 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_8_1[i, ])] <-
    df_MCS_8_1$MCS.p.val[i]

}



### MCS Horizonte = 9


MCS_9_1 <- data.frame(estMCS(residuals.9 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_9_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_9_1 <- MCS_9_1[MCS_9_1$MCS.p.val >= alpha , ]

df_MCS_9_1 <- MCS_9_1


for (i in 1:nrow(df_MCS_9_1)){

  MCS_STATS_P.1[9 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_9_1[i, ])] <-
    df_MCS_9_1$MCS.p.val[i]


}




## MCS 10


MCS_10_1 <- data.frame(estMCS(residuals.10 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_10_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_10_1 <- MCS_10_1[MCS_10_1$MCS.p.val >= alpha , ]

df_MCS_10_1 <- MCS_10_1


for (i in 1:nrow(df_MCS_10_1)){

  MCS_STATS_P.1[10 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_10_1[i, ])] <-
    df_MCS_10_1$MCS.p.val[i]

}



## MCS 11



MCS_11_1 <- data.frame(estMCS(residuals.11 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_11_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_11_1 <- MCS_11_1[MCS_11_1$MCS.p.val >= alpha , ]

df_MCS_11_1 <- MCS_11_1


for (i in 1:nrow(df_MCS_11_1)){

  MCS_STATS_P.1[11 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_11_1[i, ])] <-
    df_MCS_11_1$MCS.p.val[i]

}



### MCS Horizonte = 12


MCS_12_1 <- data.frame(estMCS(residuals.12 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_12_1$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_12_1 <- MCS_12_1[MCS_12_1$MCS.p.val >= alpha , ]

df_MCS_12_1 <- MCS_12_1


for (i in 1:nrow(df_MCS_12_1)){

  MCS_STATS_P.1[12 ,colnames(MCS_STATS_P.1) == rownames(df_MCS_12_1[i, ])] <-
    df_MCS_12_1$MCS.p.val[i]


}


# Comparando Modelos 2012-01-01 até 2014-01-01
## Gerando resíduos ao quadrado para cada horizonte


library(MCS) #mcs test
library(forecast) #diebold and mariano test

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


  assign(df , data.frame(aux_AR13 , aux_RW , aux_DDD
                         ,aux_AR13_IC , aux_cluster , aux_SDD , aux_combination ,
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

residuals.1 <- (df.1 - horiz.1)^2
residuals.2 <- (df.2 - horiz.2)^2
residuals.3 <- (df.3 - horiz.3)^2
residuals.4 <- (df.4 - horiz.4)^2
residuals.5 <- (df.5 - horiz.5)^2
residuals.6 <- (df.6 - horiz.6)^2
residuals.7 <- (df.7 - horiz.7)^2
residuals.8 <- (df.8 - horiz.8)^2
residuals.9 <- (df.9 - horiz.9)^2
residuals.10 <- (df.10 - horiz.10)^2
residuals.11 <- (df.11 - horiz.11)^2
residuals.12 <- (df.12 - horiz.12)^2

colnames(residuals.1) <- models

residuals.1.mod <- residuals.1[!duplicated(as.list(residuals.1))]

colnames(residuals.2) <- models

colnames(residuals.3) <- models

colnames(residuals.4) <- models

colnames(residuals.5) <- models

colnames(residuals.6) <- models

colnames(residuals.7) <- models

colnames(residuals.8) <- models

colnames(residuals.9) <- models

colnames(residuals.10) <- models

colnames(residuals.11) <- models

colnames(residuals.12) <- models


# 2012-01-01 até 2014-01-01

# pARA A JANELA DE 120 DIAS, 2013-05 ATÉ 2014-09

if (window_size == 120){

  first <- which(
    DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2013-05-01')

  last <- which(
    DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2014-09-01')

} else {

  first <- which(
    DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2012-01-01')

  last <- which(
    DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2014-01-01')

}


residuals.1 <- residuals.1[first:last , ]
residuals.1.mod <- residuals.1.mod[first:last , ]
residuals.2 <- residuals.2[first:last , ]
residuals.3 <- residuals.3[first:last , ]
residuals.4 <- residuals.4[first:last , ]
residuals.5 <- residuals.5[first:last , ]
residuals.6 <- residuals.6[first:last , ]
residuals.7 <- residuals.7[first:last , ]
residuals.8 <- residuals.8[first:last , ]
residuals.9 <- residuals.9[first:last , ]
residuals.10 <- residuals.10[first:last , ]
residuals.11 <- residuals.11[first:last , ]
residuals.12 <- residuals.12[first:last , ]





## MSFE


### MSFE horizonte = 1


MSFE_1_2 <- as.data.frame((colMeans(residuals.1))^(1/2))


## Dataframe para salvar as estatísticas por recorte de tempo

MSFE_P.2 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MSFE_P.2) <- colnames(residuals.1)


for (i in 1:nrow(MSFE_1_2)){

  MSFE_P.2[1 ,colnames(MSFE_P.2) == rownames(MSFE_1_2)[i]] <-
    MSFE_1_2$`(colMeans(residuals.1))^(1/2)`[i]

  MSFE_H.1[2 ,colnames(MSFE_H.1) == rownames(MSFE_1_2)[i]] <-
    MSFE_1_2$`(colMeans(residuals.1))^(1/2)`[i]

}





### MSFE horizonte = 2

MSFE_2_2 <- as.data.frame((colMeans(residuals.2))^(1/2))


for (i in 1:nrow(MSFE_2_2)){

  MSFE_P.2[2 ,colnames(MSFE_P.2) == rownames(MSFE_2_2)[i]] <-
    MSFE_2_2$`(colMeans(residuals.2))^(1/2)`[i]

}



### MSFE horizonte = 3

MSFE_3_2 <- as.data.frame((colMeans(residuals.3))^(1/2))


for (i in 1:nrow(MSFE_3_2)){

  MSFE_P.2[3 ,colnames(MSFE_P.2) == rownames(MSFE_3_2)[i]] <-
    MSFE_3_2$`(colMeans(residuals.3))^(1/2)`[i]

  MSFE_H.3[2 ,colnames(MSFE_H.3) == rownames(MSFE_3_2)[i]] <-
    MSFE_3_2$`(colMeans(residuals.3))^(1/2)`[i]

}





### MSFE horizonte = 4

MSFE_4_2 <- as.data.frame((colMeans(residuals.4))^(1/2))


for (i in 1:nrow(MSFE_4_2)){

  MSFE_P.2[4 ,colnames(MSFE_P.2) == rownames(MSFE_4_2)[i]] <-
    MSFE_4_2$`(colMeans(residuals.4))^(1/2)`[i]

}



### MSFE horizonte = 5


MSFE_5_2 <- as.data.frame((colMeans(residuals.5))^(1/2))


for (i in 1:nrow(MSFE_5_2)){

  MSFE_P.2[5 ,colnames(MSFE_P.2) == rownames(MSFE_5_2)[i]] <-
    MSFE_5_2$`(colMeans(residuals.5))^(1/2)`[i]

}



### MSFE horizonte = 6

MSFE_6_2 <- as.data.frame((colMeans(residuals.6))^(1/2))


for (i in 1:nrow(MSFE_6_2)){

  MSFE_P.2[6 ,colnames(MSFE_P.2) == rownames(MSFE_6_2)[i]] <-
    MSFE_6_2$`(colMeans(residuals.6))^(1/2)`[i]

  MSFE_H.6[2 ,colnames(MSFE_H.6) == rownames(MSFE_6_2)[i]] <-
    MSFE_6_2$`(colMeans(residuals.6))^(1/2)`[i]

}



### MSFE horizonte = 7

MSFE_7_2 <- as.data.frame((colMeans(residuals.7))^(1/2))


for (i in 1:nrow(MSFE_7_2)){

  MSFE_P.2[7 ,colnames(MSFE_P.2) == rownames(MSFE_7_2)[i]] <-
    MSFE_7_2$`(colMeans(residuals.7))^(1/2)`[i]

}



### MSFE horizonte = 8

MSFE_8_2 <- as.data.frame((colMeans(residuals.8))^(1/2))


for (i in 1:nrow(MSFE_8_2)){

  MSFE_P.2[8 ,colnames(MSFE_P.2) == rownames(MSFE_8_2)[i]] <-
    MSFE_8_2$`(colMeans(residuals.8))^(1/2)`[i]

}



### MSFE horizonte = 9

MSFE_9_2 <- as.data.frame((colMeans(residuals.9))^(1/2))


for (i in 1:nrow(MSFE_9_2)){

  MSFE_P.2[9 ,colnames(MSFE_P.2) == rownames(MSFE_9_2)[i]] <-
    MSFE_9_2$`(colMeans(residuals.9))^(1/2)`[i]

  MSFE_H.9[2 ,colnames(MSFE_H.9) == rownames(MSFE_9_2)[i]] <-
    MSFE_9_2$`(colMeans(residuals.9))^(1/2)`[i]

}



### MSFE horizonte = 10

MSFE_10_2 <- as.data.frame((colMeans(residuals.10))^(1/2))


for (i in 1:nrow(MSFE_10_2)){

  MSFE_P.2[10 ,colnames(MSFE_P.2) == rownames(MSFE_10_2)[i]] <-
    MSFE_10_2$`(colMeans(residuals.10))^(1/2)`[i]

}



### MSFE horizonte = 11

MSFE_11_2 <- as.data.frame((colMeans(residuals.11))^(1/2))


for (i in 1:nrow(MSFE_11_2)){

  MSFE_P.2[11 ,colnames(MSFE_P.2) == rownames(MSFE_11_2)[i]] <-
    MSFE_11_2$`(colMeans(residuals.11))^(1/2)`[i]

}



### MSFE horizonte = 12

MSFE_12_2 <- as.data.frame((colMeans(residuals.12))^(1/2))


for (i in 1:nrow(MSFE_12_2)){

  MSFE_P.2[12 ,colnames(MSFE_P.2) == rownames(MSFE_12_2)[i]] <-
    MSFE_12_1$`(colMeans(residuals.12))^(1/2)`[i]

  MSFE_H.12[2 ,colnames(MSFE_H.12) == rownames(MSFE_12_2)[i]] <-
    MSFE_12_2$`(colMeans(residuals.12))^(1/2)`[i]

}



## MCS
### MCS Horizonte = 1


MCS_1_2 <- data.frame(estMCS(residuals.1.mod , B = bootstrap , l = block_length))

ordernacao <- order(MCS_1_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_1_2 <- MCS_1_2[MCS_1_2$MCS.p.val >= alpha , ]

df_MCS_1_2 <- MCS_1_2

## Dataframe para salvar as estatísticas por recorte de tempo

MCS_STATS_P.2 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MCS_STATS_P.2) <- colnames(residuals.1)

for (i in 1:nrow(df_MCS_1_2)){

  MCS_STATS_P.2[1 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_1_2[i, ])] <-
    df_MCS_1_2$MCS.p.val[i]

}



### MCS Horizonte = 2


MCS_2_2 <- data.frame(estMCS(residuals.2, B = bootstrap , l = block_length))

ordernacao <- order(MCS_2_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_2_2 <- MCS_2_2[MCS_2_2$MCS.p.val >= alpha , ]

df_MCS_2_2 <- MCS_2_2



for (i in 1:nrow(df_MCS_2_2)){

  MCS_STATS_P.2[2 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_2_2[i, ])] <-
    df_MCS_2_2$MCS.p.val[i]

}



### MCS Horizonte = 3

MCS_3_2 <- data.frame(estMCS(residuals.3 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_3_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_3_2 <- MCS_3_2[MCS_3_2$MCS.p.val >= alpha , ]

df_MCS_3_2 <- MCS_3_2



for (i in 1:nrow(df_MCS_3_2)){

  MCS_STATS_P.2[3 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_3_2[i, ])] <-
    df_MCS_3_2$MCS.p.val[i]


}



## MCS 4


MCS_4_2 <- data.frame(estMCS(residuals.4, B = bootstrap , l = block_length))

ordernacao <- order(MCS_4_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_4_2 <- MCS_4_2[MCS_4_2$MCS.p.val >= alpha , ]

df_MCS_4_2 <- MCS_4_2



for (i in 1:nrow(df_MCS_4_2)){

  MCS_STATS_P.2[4 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_4_2[i, ])] <-
    df_MCS_4_2$MCS.p.val[i]

}



## MCS 5


MCS_5_2 <- data.frame(estMCS(residuals.5 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_5_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_5_2 <- MCS_5_2[MCS_5_2$MCS.p.val >= alpha , ]

df_MCS_5_2 <- MCS_5_2



for (i in 1:nrow(df_MCS_5_2)){

  MCS_STATS_P.2[5 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_5_2[i, ])] <-
    df_MCS_5_2$MCS.p.val[i]

}



### MCS Horizonte = 6


MCS_6_2 <- data.frame(estMCS(residuals.6 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_6_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_6_2 <- MCS_6_2[MCS_6_2$MCS.p.val >= alpha , ]

df_MCS_6_2 <- MCS_6_2



for (i in 1:nrow(df_MCS_6_2)){

  MCS_STATS_P.2[6 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_6_2[i, ])] <-
    df_MCS_6_2$MCS.p.val[i]


}



## MCS 7


MCS_7_2 <- data.frame(estMCS(residuals.7 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_7_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_7_2 <- MCS_7_2[MCS_7_2$MCS.p.val >= alpha , ]

df_MCS_7_2 <- MCS_7_2



for (i in 1:nrow(df_MCS_7_2)){

  MCS_STATS_P.2[7 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_7_2[i, ])] <-
    df_MCS_7_2$MCS.p.val[i]

}



## MCS 8


MCS_8_2 <- data.frame(estMCS(residuals.8 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_8_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_8_2 <- MCS_8_2[MCS_8_2$MCS.p.val >= alpha , ]

df_MCS_8_2 <- MCS_8_2


for (i in 1:nrow(df_MCS_8_2)){

  MCS_STATS_P.2[8 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_8_2[i, ])] <-
    df_MCS_8_2$MCS.p.val[i]

}



### MCS Horizonte = 9


MCS_9_2 <- data.frame(estMCS(residuals.9 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_9_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_9_2 <- MCS_9_2[MCS_9_2$MCS.p.val >= alpha , ]

df_MCS_9_2 <- MCS_9_2



for (i in 1:nrow(df_MCS_9_2)){

  MCS_STATS_P.2[9 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_9_2[i, ])] <-
    df_MCS_9_2$MCS.p.val[i]


}




## MCS 10


MCS_10_2 <- data.frame(estMCS(residuals.10 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_10_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_10_2 <- MCS_10_2[MCS_10_2$MCS.p.val >= alpha , ]

df_MCS_10_2 <- MCS_10_2


for (i in 1:nrow(df_MCS_10_2)){

  MCS_STATS_P.2[10 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_10_2[i, ])] <-
    df_MCS_10_2$MCS.p.val[i]

}



## MCS 11


MCS_11_2 <- data.frame(estMCS(residuals.11 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_11_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_11_2 <- MCS_11_2[MCS_11_2$MCS.p.val >= alpha , ]

df_MCS_11_2 <- MCS_11_2


for (i in 1:nrow(df_MCS_11_2)){

  MCS_STATS_P.2[11 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_11_2[i, ])] <-
    df_MCS_11_2$MCS.p.val[i]

}



### MCS Horizonte = 12

MCS_12_2 <- data.frame(estMCS(residuals.12 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_12_2$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_12_2 <- MCS_12_2[MCS_12_2$MCS.p.val >= alpha , ]

df_MCS_12_2 <- MCS_12_2



for (i in 1:nrow(df_MCS_12_2)){

  MCS_STATS_P.2[12 ,colnames(MCS_STATS_P.2) == rownames(df_MCS_12_2[i, ])] <-
    df_MCS_12_2$MCS.p.val[i]



}




# Comparando Modelos 2014-12 até 2015-12
## Gerando resíduos ao quadrado para cada horizonte



library(MCS) #mcs test
library(forecast) #diebold and mariano test

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


  assign(df , data.frame(aux_AR13 , aux_RW , aux_DDD
                         ,aux_AR13_IC , aux_cluster , aux_SDD , aux_combination ,
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

residuals.1 <- (df.1 - horiz.1)^2
residuals.2 <- (df.2 - horiz.2)^2
residuals.3 <- (df.3 - horiz.3)^2
residuals.4 <- (df.4 - horiz.4)^2
residuals.5 <- (df.5 - horiz.5)^2
residuals.6 <- (df.6 - horiz.6)^2
residuals.7 <- (df.7 - horiz.7)^2
residuals.8 <- (df.8 - horiz.8)^2
residuals.9 <- (df.9 - horiz.9)^2
residuals.10 <- (df.10 - horiz.10)^2
residuals.11 <- (df.11 - horiz.11)^2
residuals.12 <- (df.12 - horiz.12)^2

colnames(residuals.1) <- models

residuals.1.mod <- residuals.1[!duplicated(as.list(residuals.1))]

colnames(residuals.2) <- models

colnames(residuals.3) <- models

colnames(residuals.4) <- models

colnames(residuals.5) <- models

colnames(residuals.6) <- models

colnames(residuals.7) <- models

colnames(residuals.8) <- models

colnames(residuals.9) <- models

colnames(residuals.10) <- models

colnames(residuals.11) <- models

colnames(residuals.12) <- models

# 2014-12 até 2016-12


first <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2014-12-01')

last <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2015-12-01')



residuals.1 <- residuals.1[first:last , ]
residuals.1.mod <- residuals.1.mod[first:last , ]
residuals.2 <- residuals.2[first:last , ]
residuals.3 <- residuals.3[first:last , ]
residuals.4 <- residuals.4[first:last , ]
residuals.5 <- residuals.5[first:last , ]
residuals.6 <- residuals.6[first:last , ]
residuals.7 <- residuals.7[first:last , ]
residuals.8 <- residuals.8[first:last , ]
residuals.9 <- residuals.9[first:last , ]
residuals.10 <- residuals.10[first:last , ]
residuals.11 <- residuals.11[first:last , ]
residuals.12 <- residuals.12[first:last , ]





## MSFE


### MSFE horizonte = 1


MSFE_1_3 <- as.data.frame((colMeans(residuals.1))^(1/2))


## Dataframe para salvar as estatísticas por recorte de tempo

MSFE_P.3 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MSFE_P.3) <- colnames(residuals.1)


for (i in 1:nrow(MSFE_1_3)){

  MSFE_P.3[1 ,colnames(MSFE_P.3) == rownames(MSFE_1_3)[i]] <-
    MSFE_1_3$`(colMeans(residuals.1))^(1/2)`[i]

  MSFE_H.1[3 ,colnames(MSFE_H.1) == rownames(MSFE_1_3)[i]] <-
    MSFE_1_3$`(colMeans(residuals.1))^(1/2)`[i]

}





### MSFE horizonte = 2

MSFE_2_3 <- as.data.frame((colMeans(residuals.2))^(1/2))


for (i in 1:nrow(MSFE_2_3)){

  MSFE_P.3[2 ,colnames(MSFE_P.3) == rownames(MSFE_2_3)[i]] <-
    MSFE_2_3$`(colMeans(residuals.2))^(1/2)`[i]

}



### MSFE horizonte = 3

MSFE_3_3 <- as.data.frame((colMeans(residuals.3))^(1/2))



for (i in 1:nrow(MSFE_3_3)){

  MSFE_P.3[3 ,colnames(MSFE_P.3) == rownames(MSFE_3_3)[i]] <-
    MSFE_3_3$`(colMeans(residuals.3))^(1/2)`[i]

  MSFE_H.3[3 ,colnames(MSFE_H.3) == rownames(MSFE_3_3)[i]] <-
    MSFE_3_3$`(colMeans(residuals.3))^(1/2)`[i]

}





### MSFE horizonte = 4

MSFE_4_3 <- as.data.frame((colMeans(residuals.4))^(1/2))


for (i in 1:nrow(MSFE_4_3)){

  MSFE_P.3[4 ,colnames(MSFE_P.3) == rownames(MSFE_4_3)[i]] <-
    MSFE_4_3$`(colMeans(residuals.4))^(1/2)`[i]

}



### MSFE horizonte = 5


MSFE_5_3 <- as.data.frame((colMeans(residuals.5))^(1/2))


for (i in 1:nrow(MSFE_5_3)){

  MSFE_P.3[5 ,colnames(MSFE_P.3) == rownames(MSFE_5_3)[i]] <-
    MSFE_5_3$`(colMeans(residuals.5))^(1/2)`[i]

}



### MSFE horizonte = 6

MSFE_6_3 <- as.data.frame((colMeans(residuals.6))^(1/2))


for (i in 1:nrow(MSFE_6_3)){

  MSFE_P.3[6 ,colnames(MSFE_P.3) == rownames(MSFE_6_3)[i]] <-
    MSFE_6_3$`(colMeans(residuals.6))^(1/2)`[i]

  MSFE_H.6[3 ,colnames(MSFE_H.6) == rownames(MSFE_6_3)[i]] <-
    MSFE_6_3$`(colMeans(residuals.6))^(1/2)`[i]

}



### MSFE horizonte = 7

MSFE_7_3 <- as.data.frame((colMeans(residuals.7))^(1/2))


for (i in 1:nrow(MSFE_7_3)){

  MSFE_P.3[7 ,colnames(MSFE_P.3) == rownames(MSFE_7_3)[i]] <-
    MSFE_7_3$`(colMeans(residuals.7))^(1/2)`[i]

}



### MSFE horizonte = 8

MSFE_8_3 <- as.data.frame((colMeans(residuals.8))^(1/2))


for (i in 1:nrow(MSFE_8_3)){

  MSFE_P.3[8 ,colnames(MSFE_P.3) == rownames(MSFE_8_3)[i]] <-
    MSFE_8_3$`(colMeans(residuals.8))^(1/2)`[i]

}



### MSFE horizonte = 9

MSFE_9_3 <- as.data.frame((colMeans(residuals.9))^(1/2))



for (i in 1:nrow(MSFE_9_3)){

  MSFE_P.3[9 ,colnames(MSFE_P.3) == rownames(MSFE_9_3)[i]] <-
    MSFE_9_3$`(colMeans(residuals.9))^(1/2)`[i]

  MSFE_H.9[3 ,colnames(MSFE_H.9) == rownames(MSFE_9_3)[i]] <-
    MSFE_9_3$`(colMeans(residuals.9))^(1/2)`[i]

}



### MSFE horizonte = 10

MSFE_10_3 <- as.data.frame((colMeans(residuals.10))^(1/2))


for (i in 1:nrow(MSFE_10_3)){

  MSFE_P.3[10 ,colnames(MSFE_P.3) == rownames(MSFE_10_3)[i]] <-
    MSFE_10_3$`(colMeans(residuals.10))^(1/2)`[i]

}



### MSFE horizonte = 11

MSFE_11_3 <- as.data.frame((colMeans(residuals.11))^(1/2))


for (i in 1:nrow(MSFE_11_3)){

  MSFE_P.3[11 ,colnames(MSFE_P.3) == rownames(MSFE_11_3)[i]] <-
    MSFE_11_3$`(colMeans(residuals.11))^(1/2)`[i]

}



### MSFE horizonte = 12

MSFE_12_3 <- as.data.frame((colMeans(residuals.12))^(1/2))



for (i in 1:nrow(MSFE_12_3)){

  MSFE_P.3[12 ,colnames(MSFE_P.3) == rownames(MSFE_12_3)[i]] <-
    MSFE_12_3$`(colMeans(residuals.12))^(1/2)`[i]

  MSFE_H.12[3 ,colnames(MSFE_H.12) == rownames(MSFE_12_3)[i]] <-
    MSFE_12_3$`(colMeans(residuals.12))^(1/2)`[i]

}



## MCS
### MCS Horizonte = 1


MCS_1_3 <- data.frame(estMCS(residuals.1.mod , B = bootstrap , l = block_length))

ordernacao <- order(MCS_1_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_1_3 <- MCS_1_3[MCS_1_3$MCS.p.val >= alpha , ]

df_MCS_1_3 <- MCS_1_3


## Dataframe para salvar as estatísticas por recorte de tempo

MCS_STATS_P.3 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MCS_STATS_P.3) <- colnames(residuals.1)


for (i in 1:nrow(df_MCS_1_3)){

  MCS_STATS_P.3[1 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_1_3[i, ])] <-
    df_MCS_1_3$MCS.p.val[i]


}



### MCS Horizonte = 2


MCS_2_3 <- data.frame(estMCS(residuals.2 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_2_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_2_3 <- MCS_2_3[MCS_2_3$MCS.p.val >= alpha , ]

df_MCS_2_3 <- MCS_2_3



for (i in 1:nrow(df_MCS_2_3)){

  MCS_STATS_P.3[2 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_2_3[i, ])] <-
    df_MCS_2_3$MCS.p.val[i]

}



### MCS Horizonte = 3

MCS_3_3 <- data.frame(estMCS(residuals.3 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_3_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_3_3 <- MCS_3_3[MCS_3_3$MCS.p.val >= alpha , ]

df_MCS_3_3 <- MCS_3_3



for (i in 1:nrow(df_MCS_3_3)){

  MCS_STATS_P.3[3 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_3_3[i, ])] <-
    df_MCS_3_3$MCS.p.val[i]


}



## MCS 4


MCS_4_3 <- data.frame(estMCS(residuals.4 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_4_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_4_3 <- MCS_4_3[MCS_4_3$MCS.p.val >= alpha , ]

df_MCS_4_3 <- MCS_4_3



for (i in 1:nrow(df_MCS_4_3)){

  MCS_STATS_P.3[4 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_4_3[i, ])] <-
    df_MCS_4_3$MCS.p.val[i]

}



## MCS 5



MCS_5_3 <- data.frame(estMCS(residuals.5 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_5_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_5_3 <- MCS_5_3[MCS_5_3$MCS.p.val >= alpha , ]

df_MCS_5_3 <- MCS_5_3



for (i in 1:nrow(df_MCS_5_3)){

  MCS_STATS_P.3[5 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_5_3[i, ])] <-
    df_MCS_5_3$MCS.p.val[i]

}



### MCS Horizonte = 6


MCS_6_3 <- data.frame(estMCS(residuals.6 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_6_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_6_3 <- MCS_6_3[MCS_6_3$MCS.p.val >= alpha , ]

df_MCS_6_3 <- MCS_6_3


for (i in 1:nrow(df_MCS_6_3)){

  MCS_STATS_P.3[6 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_6_3[i, ])] <-
    df_MCS_6_3$MCS.p.val[i]


}



## MCS 7


MCS_7_3 <- data.frame(estMCS(residuals.7 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_7_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_7_3 <- MCS_7_3[MCS_7_3$MCS.p.val >= alpha , ]

df_MCS_7_3 <- MCS_7_3



for (i in 1:nrow(df_MCS_7_3)){

  MCS_STATS_P.3[7 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_7_3[i, ])] <-
    df_MCS_7_3$MCS.p.val[i]

}



## MCS 8


MCS_8_3 <- data.frame(estMCS(residuals.8 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_8_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_8_3 <- MCS_8_3[MCS_8_3$MCS.p.val >= alpha , ]

df_MCS_8_3 <- MCS_8_3


for (i in 1:nrow(df_MCS_8_3)){

  MCS_STATS_P.3[8 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_8_3[i, ])] <-
    df_MCS_8_3$MCS.p.val[i]

}



### MCS Horizonte = 9


MCS_9_3 <- data.frame(estMCS(residuals.9 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_9_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_9_3 <- MCS_9_3[MCS_9_3$MCS.p.val >= alpha , ]

df_MCS_9_3 <- MCS_9_3

for (i in 1:nrow(df_MCS_9_3)){

  MCS_STATS_P.3[9 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_9_3[i, ])] <-
    df_MCS_9_3$MCS.p.val[i]


}




## MCS 10


MCS_10_3 <- data.frame(estMCS(residuals.10 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_10_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_10_3 <- MCS_10_3[MCS_10_3$MCS.p.val >= alpha , ]

df_MCS_10_3 <- MCS_10_3


for (i in 1:nrow(df_MCS_10_3)){

  MCS_STATS_P.3[10 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_10_3[i, ])] <-
    df_MCS_10_3$MCS.p.val[i]

}



## MCS 11

MCS_11_3 <- data.frame(estMCS(residuals.11 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_11_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_11_3 <- MCS_11_3[MCS_11_3$MCS.p.val >= alpha , ]

df_MCS_11_3 <- MCS_11_3


for (i in 1:nrow(df_MCS_11_3)){

  MCS_STATS_P.3[11 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_11_3[i, ])] <-
    df_MCS_11_3$MCS.p.val[i]

}



### MCS Horizonte = 12

MCS_12_3 <- data.frame(estMCS(residuals.12 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_12_3$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_12_3 <- MCS_12_3[MCS_12_3$MCS.p.val >= alpha , ]

df_MCS_12_3 <- MCS_12_3


for (i in 1:nrow(df_MCS_12_3)){

  MCS_STATS_P.3[12 ,colnames(MCS_STATS_P.3) == rownames(df_MCS_12_3[i, ])] <-
    df_MCS_12_3$MCS.p.val[i]



}



# Comparando Modelos 2014-12 até 2016-12
## Gerando resíduos ao quadrado para cada horizonte



library(MCS) #mcs test
library(forecast) #diebold and mariano test

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


  assign(df , data.frame(aux_AR13 , aux_RW , aux_DDD
                         ,aux_AR13_IC , aux_cluster , aux_SDD , aux_combination ,
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

residuals.1 <- (df.1 - horiz.1)^2
residuals.2 <- (df.2 - horiz.2)^2
residuals.3 <- (df.3 - horiz.3)^2
residuals.4 <- (df.4 - horiz.4)^2
residuals.5 <- (df.5 - horiz.5)^2
residuals.6 <- (df.6 - horiz.6)^2
residuals.7 <- (df.7 - horiz.7)^2
residuals.8 <- (df.8 - horiz.8)^2
residuals.9 <- (df.9 - horiz.9)^2
residuals.10 <- (df.10 - horiz.10)^2
residuals.11 <- (df.11 - horiz.11)^2
residuals.12 <- (df.12 - horiz.12)^2

colnames(residuals.1) <- models

residuals.1.mod <- residuals.1[!duplicated(as.list(residuals.1))]

colnames(residuals.2) <- models

colnames(residuals.3) <- models

colnames(residuals.4) <- models

colnames(residuals.5) <- models

colnames(residuals.6) <- models

colnames(residuals.7) <- models

colnames(residuals.8) <- models

colnames(residuals.9) <- models

colnames(residuals.10) <- models

colnames(residuals.11) <- models

colnames(residuals.12) <- models

# 2014-12 até 2016-12


first <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2014-12-01')

last <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2016-12-01')



residuals.1 <- residuals.1[first:last , ]
residuals.1.mod <- residuals.1.mod[first:last , ]
residuals.2 <- residuals.2[first:last , ]
residuals.3 <- residuals.3[first:last , ]
residuals.4 <- residuals.4[first:last , ]
residuals.5 <- residuals.5[first:last , ]
residuals.6 <- residuals.6[first:last , ]
residuals.7 <- residuals.7[first:last , ]
residuals.8 <- residuals.8[first:last , ]
residuals.9 <- residuals.9[first:last , ]
residuals.10 <- residuals.10[first:last , ]
residuals.11 <- residuals.11[first:last , ]
residuals.12 <- residuals.12[first:last , ]





## MSFE


### MSFE horizonte = 1


MSFE_1_4 <- as.data.frame((colMeans(residuals.1))^(1/2))


## Dataframe para salvar as estatísticas por recorte de tempo

MSFE_P.4 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MSFE_P.4) <- colnames(residuals.1)


for (i in 1:nrow(MSFE_1_4)){

  MSFE_P.4[1 ,colnames(MSFE_P.4) == rownames(MSFE_1_4)[i]] <-
    MSFE_1_4$`(colMeans(residuals.1))^(1/2)`[i]

  MSFE_H.1[4 ,colnames(MSFE_H.1) == rownames(MSFE_1_4)[i]] <-
    MSFE_1_4$`(colMeans(residuals.1))^(1/2)`[i]

}





### MSFE horizonte = 2

MSFE_2_4 <- as.data.frame((colMeans(residuals.2))^(1/2))


for (i in 1:nrow(MSFE_2_4)){

  MSFE_P.4[2 ,colnames(MSFE_P.4) == rownames(MSFE_2_4)[i]] <-
    MSFE_2_4$`(colMeans(residuals.2))^(1/2)`[i]

}



### MSFE horizonte = 3

MSFE_3_4 <- as.data.frame((colMeans(residuals.3))^(1/2))


for (i in 1:nrow(MSFE_3_4)){

  MSFE_P.4[3 ,colnames(MSFE_P.4) == rownames(MSFE_3_4)[i]] <-
    MSFE_3_4$`(colMeans(residuals.3))^(1/2)`[i]

  MSFE_H.3[4 ,colnames(MSFE_H.3) == rownames(MSFE_3_4)[i]] <-
    MSFE_3_4$`(colMeans(residuals.3))^(1/2)`[i]

}





### MSFE horizonte = 4

MSFE_4_4 <- as.data.frame((colMeans(residuals.4))^(1/2))


for (i in 1:nrow(MSFE_4_4)){

  MSFE_P.4[4 ,colnames(MSFE_P.4) == rownames(MSFE_4_4)[i]] <-
    MSFE_4_4$`(colMeans(residuals.4))^(1/2)`[i]

}



### MSFE horizonte = 5


MSFE_5_4 <- as.data.frame((colMeans(residuals.5))^(1/2))


for (i in 1:nrow(MSFE_5_4)){

  MSFE_P.4[5 ,colnames(MSFE_P.4) == rownames(MSFE_5_4)[i]] <-
    MSFE_5_4$`(colMeans(residuals.5))^(1/2)`[i]

}



### MSFE horizonte = 6

MSFE_6_4 <- as.data.frame((colMeans(residuals.6))^(1/2))


for (i in 1:nrow(MSFE_6_4)){

  MSFE_P.4[6 ,colnames(MSFE_P.4) == rownames(MSFE_6_4)[i]] <-
    MSFE_6_4$`(colMeans(residuals.6))^(1/2)`[i]

  MSFE_H.6[4 ,colnames(MSFE_H.6) == rownames(MSFE_6_4)[i]] <-
    MSFE_6_4$`(colMeans(residuals.6))^(1/2)`[i]

}



### MSFE horizonte = 7

MSFE_7_4 <- as.data.frame((colMeans(residuals.7))^(1/2))


for (i in 1:nrow(MSFE_7_4)){

  MSFE_P.4[7 ,colnames(MSFE_P.4) == rownames(MSFE_7_4)[i]] <-
    MSFE_7_4$`(colMeans(residuals.7))^(1/2)`[i]

}



### MSFE horizonte = 8

MSFE_8_4 <- as.data.frame((colMeans(residuals.8))^(1/2))


for (i in 1:nrow(MSFE_8_4)){

  MSFE_P.4[8 ,colnames(MSFE_P.4) == rownames(MSFE_8_4)[i]] <-
    MSFE_8_4$`(colMeans(residuals.8))^(1/2)`[i]

}



### MSFE horizonte = 9

MSFE_9_4 <- as.data.frame((colMeans(residuals.9))^(1/2))


for (i in 1:nrow(MSFE_9_4)){

  MSFE_P.4[9 ,colnames(MSFE_P.4) == rownames(MSFE_9_4)[i]] <-
    MSFE_9_4$`(colMeans(residuals.9))^(1/2)`[i]

  MSFE_H.9[4 ,colnames(MSFE_H.9) == rownames(MSFE_9_4)[i]] <-
    MSFE_9_4$`(colMeans(residuals.9))^(1/2)`[i]

}



### MSFE horizonte = 10

MSFE_10_4 <- as.data.frame((colMeans(residuals.10))^(1/2))


for (i in 1:nrow(MSFE_10_4)){

  MSFE_P.4[10 ,colnames(MSFE_P.4) == rownames(MSFE_10_4)[i]] <-
    MSFE_10_4$`(colMeans(residuals.10))^(1/2)`[i]

}



### MSFE horizonte = 11

MSFE_11_4 <- as.data.frame((colMeans(residuals.11))^(1/2))


for (i in 1:nrow(MSFE_11_4)){

  MSFE_P.4[11 ,colnames(MSFE_P.4) == rownames(MSFE_11_4)[i]] <-
    MSFE_11_4$`(colMeans(residuals.11))^(1/2)`[i]

}



### MSFE horizonte = 12

MSFE_12_4 <- as.data.frame((colMeans(residuals.12))^(1/2))


for (i in 1:nrow(MSFE_12_4)){

  MSFE_P.4[12 ,colnames(MSFE_P.4) == rownames(MSFE_12_4)[i]] <-
    MSFE_12_4$`(colMeans(residuals.12))^(1/2)`[i]

  MSFE_H.12[4 ,colnames(MSFE_H.12) == rownames(MSFE_12_4)[i]] <-
    MSFE_12_4$`(colMeans(residuals.12))^(1/2)`[i]

}



## MCS
### MCS Horizonte = 1


MCS_1_4 <- data.frame(estMCS(residuals.1.mod , B = bootstrap , l = block_length))

ordernacao <- order(MCS_1_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_1_4 <- MCS_1_4[MCS_1_4$MCS.p.val >= alpha , ]

df_MCS_1_4 <- MCS_1_4

## Dataframe para salvar as estatísticas por recorte de tempo

MCS_STATS_P.4 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MCS_STATS_P.4) <- colnames(residuals.1)

for (i in 1:nrow(df_MCS_1_4)){

  MCS_STATS_P.4[1 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_1_4[i, ])] <-
    df_MCS_1_4$MCS.p.val[i]


}



### MCS Horizonte = 2


MCS_2_4 <- data.frame(estMCS(residuals.2 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_2_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_2_4 <- MCS_2_4[MCS_2_4$MCS.p.val >= alpha , ]

df_MCS_2_4 <- MCS_2_4



for (i in 1:nrow(df_MCS_2_4)){

  MCS_STATS_P.4[2 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_2_4[i, ])] <-
    df_MCS_2_4$MCS.p.val[i]

}



### MCS Horizonte = 3
MCS_3_4 <- data.frame(estMCS(residuals.3 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_3_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_3_4 <- MCS_3_4[MCS_3_4$MCS.p.val >= alpha , ]

df_MCS_3_4 <- MCS_3_4



for (i in 1:nrow(df_MCS_3_4)){

  MCS_STATS_P.4[3 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_3_4[i, ])] <-
    df_MCS_3_4$MCS.p.val[i]


}



## MCS 4

MCS_4_4 <- data.frame(estMCS(residuals.4 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_4_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_4_4 <- MCS_4_4[MCS_4_4$MCS.p.val >= alpha , ]

df_MCS_4_4 <- MCS_4_4



for (i in 1:nrow(df_MCS_4_4)){

  MCS_STATS_P.4[4 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_4_4[i, ])] <-
    df_MCS_4_4$MCS.p.val[i]

}



## MCS 5


MCS_5_4 <- data.frame(estMCS(residuals.5 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_5_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_5_4 <- MCS_5_4[MCS_5_4$MCS.p.val >= alpha , ]

df_MCS_5_4 <- MCS_5_4



for (i in 1:nrow(df_MCS_5_4)){

  MCS_STATS_P.4[5 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_5_4[i, ])] <-
    df_MCS_5_4$MCS.p.val[i]

}



### MCS Horizonte = 6

MCS_6_4 <- data.frame(estMCS(residuals.6 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_6_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_6_4 <- MCS_6_4[MCS_6_4$MCS.p.val >= alpha , ]

df_MCS_6_4 <- MCS_6_4


for (i in 1:nrow(df_MCS_6_4)){

  MCS_STATS_P.4[6 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_6_4[i, ])] <-
    df_MCS_6_4$MCS.p.val[i]

}



## MCS 7


MCS_7_4 <- data.frame(estMCS(residuals.7 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_7_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_7_4 <- MCS_7_4[MCS_7_4$MCS.p.val >= alpha , ]

df_MCS_7_4 <- MCS_7_4

for (i in 1:nrow(df_MCS_7_4)){

  MCS_STATS_P.4[7 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_7_4[i, ])] <-
    df_MCS_7_4$MCS.p.val[i]

}



## MCS 8



MCS_8_4 <- data.frame(estMCS(residuals.8 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_8_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_8_4 <- MCS_8_4[MCS_8_4$MCS.p.val >= alpha , ]

df_MCS_8_4 <- MCS_8_4


for (i in 1:nrow(df_MCS_8_4)){

  MCS_STATS_P.4[8 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_8_4[i, ])] <-
    df_MCS_8_4$MCS.p.val[i]

}



### MCS Horizonte = 9


MCS_9_4 <- data.frame(estMCS(residuals.9 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_9_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_9_4 <- MCS_9_4[MCS_9_4$MCS.p.val >= alpha , ]

df_MCS_9_4 <- MCS_9_4



for (i in 1:nrow(df_MCS_9_4)){

  MCS_STATS_P.4[9 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_9_4[i, ])] <-
    df_MCS_9_4$MCS.p.val[i]


}




## MCS 10

MCS_10_4 <- data.frame(estMCS(residuals.10 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_10_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_10_4 <- MCS_10_4[MCS_10_4$MCS.p.val >= alpha , ]

df_MCS_10_4 <- MCS_10_4


for (i in 1:nrow(df_MCS_10_4)){

  MCS_STATS_P.4[10 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_10_4[i, ])] <-
    df_MCS_10_4$MCS.p.val[i]

}



## MCS 11


MCS_11_4 <- data.frame(estMCS(residuals.11 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_11_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_11_4 <- MCS_11_4[MCS_11_4$MCS.p.val >= alpha , ]

df_MCS_11_4 <- MCS_11_4

for (i in 1:nrow(df_MCS_11_4)){

  MCS_STATS_P.4[11 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_11_4[i, ])] <-
    df_MCS_11_4$MCS.p.val[i]

}



### MCS Horizonte = 12


MCS_12_4 <- data.frame(estMCS(residuals.12 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_12_4$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_12_4 <- MCS_12_4[MCS_12_4$MCS.p.val >= alpha , ]

df_MCS_12_4 <- MCS_12_4


for (i in 1:nrow(df_MCS_12_4)){

  MCS_STATS_P.4[12 ,colnames(MCS_STATS_P.4) == rownames(df_MCS_12_4[i, ])] <-
    df_MCS_12_4$MCS.p.val[i]



}




# Comparando Modelos 2016-01-01 até 2019-12-12

## Gerando resíduos ao quadrado para cada horizonte



library(MCS) #mcs test
library(forecast) #diebold and mariano test

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


  assign(df , data.frame(aux_AR13 , aux_RW , aux_DDD
                         ,aux_AR13_IC , aux_cluster , aux_SDD , aux_combination ,
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

residuals.1 <- (df.1 - horiz.1)^2
residuals.2 <- (df.2 - horiz.2)^2
residuals.3 <- (df.3 - horiz.3)^2
residuals.4 <- (df.4 - horiz.4)^2
residuals.5 <- (df.5 - horiz.5)^2
residuals.6 <- (df.6 - horiz.6)^2
residuals.7 <- (df.7 - horiz.7)^2
residuals.8 <- (df.8 - horiz.8)^2
residuals.9 <- (df.9 - horiz.9)^2
residuals.10 <- (df.10 - horiz.10)^2
residuals.11 <- (df.11 - horiz.11)^2
residuals.12 <- (df.12 - horiz.12)^2

colnames(residuals.1) <- models

residuals.1.mod <- residuals.1[!duplicated(as.list(residuals.1))]

colnames(residuals.2) <- models

colnames(residuals.3) <- models

colnames(residuals.4) <- models

colnames(residuals.5) <- models

colnames(residuals.6) <- models

colnames(residuals.7) <- models

colnames(residuals.8) <- models

colnames(residuals.9) <- models

colnames(residuals.10) <- models

colnames(residuals.11) <- models

colnames(residuals.12) <- models


#  2016-01-01 até 2019-12-12

first <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2016-01-01')

last <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2019-12-01')



residuals.1 <- residuals.1[first:last , ]
residuals.1.mod <- residuals.1.mod[first:last , ]
residuals.2 <- residuals.2[first:last , ]
residuals.3 <- residuals.3[first:last , ]
residuals.4 <- residuals.4[first:last , ]
residuals.5 <- residuals.5[first:last , ]
residuals.6 <- residuals.6[first:last , ]
residuals.7 <- residuals.7[first:last , ]
residuals.8 <- residuals.8[first:last , ]
residuals.9 <- residuals.9[first:last , ]
residuals.10 <- residuals.10[first:last , ]
residuals.11 <- residuals.11[first:last , ]
residuals.12 <- residuals.12[first:last , ]





## MSFE


### MSFE horizonte = 1


MSFE_1_5 <- as.data.frame((colMeans(residuals.1))^(1/2))


## Dataframe para salvar as estatísticas por recorte de tempo

MSFE_P.5 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MSFE_P.5) <- colnames(residuals.1)


for (i in 1:nrow(MSFE_1_5)){

  MSFE_P.5[1 ,colnames(MSFE_P.5) == rownames(MSFE_1_5)[i]] <-
    MSFE_1_5$`(colMeans(residuals.1))^(1/2)`[i]

  MSFE_H.1[5 ,colnames(MSFE_H.1) == rownames(MSFE_1_5)[i]] <-
    MSFE_1_5$`(colMeans(residuals.1))^(1/2)`[i]

}





### MSFE horizonte = 2

MSFE_2_5 <- as.data.frame((colMeans(residuals.2))^(1/2))


for (i in 1:nrow(MSFE_2_5)){

  MSFE_P.5[2 ,colnames(MSFE_P.5) == rownames(MSFE_2_5)[i]] <-
    MSFE_2_5$`(colMeans(residuals.2))^(1/2)`[i]

}



### MSFE horizonte = 3

MSFE_3_5 <- as.data.frame((colMeans(residuals.3))^(1/2))



for (i in 1:nrow(MSFE_3_5)){

  MSFE_P.5[3 ,colnames(MSFE_P.5) == rownames(MSFE_3_5)[i]] <-
    MSFE_3_5$`(colMeans(residuals.3))^(1/2)`[i]

  MSFE_H.3[5 ,colnames(MSFE_H.3) == rownames(MSFE_3_5)[i]] <-
    MSFE_3_5$`(colMeans(residuals.3))^(1/2)`[i]

}





### MSFE horizonte = 4

MSFE_4_5 <- as.data.frame((colMeans(residuals.4))^(1/2))


for (i in 1:nrow(MSFE_4_5)){

  MSFE_P.5[4 ,colnames(MSFE_P.5) == rownames(MSFE_4_5)[i]] <-
    MSFE_4_5$`(colMeans(residuals.4))^(1/2)`[i]

}



### MSFE horizonte = 5


MSFE_5_5 <- as.data.frame((colMeans(residuals.5))^(1/2))


for (i in 1:nrow(MSFE_5_5)){

  MSFE_P.5[5 ,colnames(MSFE_P.5) == rownames(MSFE_5_5)[i]] <-
    MSFE_5_5$`(colMeans(residuals.5))^(1/2)`[i]

}



### MSFE horizonte = 6

MSFE_6_5 <- as.data.frame((colMeans(residuals.6))^(1/2))


for (i in 1:nrow(MSFE_6_5)){

  MSFE_P.5[6 ,colnames(MSFE_P.5) == rownames(MSFE_6_5)[i]] <-
    MSFE_6_5$`(colMeans(residuals.6))^(1/2)`[i]

  MSFE_H.6[5 ,colnames(MSFE_H.6) == rownames(MSFE_6_5)[i]] <-
    MSFE_6_5$`(colMeans(residuals.6))^(1/2)`[i]

}



### MSFE horizonte = 7

MSFE_7_5 <- as.data.frame((colMeans(residuals.7))^(1/2))


for (i in 1:nrow(MSFE_7_5)){

  MSFE_P.5[7 ,colnames(MSFE_P.5) == rownames(MSFE_7_5)[i]] <-
    MSFE_7_5$`(colMeans(residuals.7))^(1/2)`[i]

}



### MSFE horizonte = 8

MSFE_8_5 <- as.data.frame((colMeans(residuals.8))^(1/2))


for (i in 1:nrow(MSFE_8_5)){

  MSFE_P.5[8 ,colnames(MSFE_P.5) == rownames(MSFE_8_5)[i]] <-
    MSFE_8_5$`(colMeans(residuals.8))^(1/2)`[i]

}



### MSFE horizonte = 9

MSFE_9_5 <- as.data.frame((colMeans(residuals.9))^(1/2))



for (i in 1:nrow(MSFE_9_5)){

  MSFE_P.5[9 ,colnames(MSFE_P.5) == rownames(MSFE_9_5)[i]] <-
    MSFE_9_5$`(colMeans(residuals.9))^(1/2)`[i]

  MSFE_H.9[5 ,colnames(MSFE_H.9) == rownames(MSFE_9_5)[i]] <-
    MSFE_9_5$`(colMeans(residuals.9))^(1/2)`[i]

}



### MSFE horizonte = 10

MSFE_10_5 <- as.data.frame((colMeans(residuals.10))^(1/2))


for (i in 1:nrow(MSFE_10_5)){

  MSFE_P.5[10 ,colnames(MSFE_P.5) == rownames(MSFE_10_5)[i]] <-
    MSFE_10_5$`(colMeans(residuals.10))^(1/2)`[i]

}



### MSFE horizonte = 11

MSFE_11_5 <- as.data.frame((colMeans(residuals.11))^(1/2))


for (i in 1:nrow(MSFE_11_5)){

  MSFE_P.5[11 ,colnames(MSFE_P.5) == rownames(MSFE_11_5)[i]] <-
    MSFE_11_5$`(colMeans(residuals.11))^(1/2)`[i]

}



### MSFE horizonte = 12

MSFE_12_5 <- as.data.frame((colMeans(residuals.12))^(1/2))



for (i in 1:nrow(MSFE_12_5)){

  MSFE_P.5[12 ,colnames(MSFE_P.5) == rownames(MSFE_12_5)[i]] <-
    MSFE_12_5$`(colMeans(residuals.12))^(1/2)`[i]

  MSFE_H.12[5 ,colnames(MSFE_H.12) == rownames(MSFE_12_5)[i]] <-
    MSFE_12_5$`(colMeans(residuals.12))^(1/2)`[i]

}



## MCS
### MCS Horizonte = 1


MCS_1_5 <- data.frame(estMCS(residuals.1.mod , B = bootstrap , l = block_length))

ordernacao <- order(MCS_1_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_1_5 <- MCS_1_5[MCS_1_5$MCS.p.val >= alpha , ]

df_MCS_1_5 <- MCS_1_5

## Dataframe para salvar as estatísticas por recorte de tempo

MCS_STATS_P.5 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MCS_STATS_P.5) <- colnames(residuals.1)


for (i in 1:nrow(df_MCS_1_5)){

  MCS_STATS_P.5[1 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_1_5[i, ])] <-
    df_MCS_1_5$MCS.p.val[i]


}



### MCS Horizonte = 2


MCS_2_5 <- data.frame(estMCS(residuals.2 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_2_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_2_5 <- MCS_2_5[MCS_2_5$MCS.p.val >= alpha , ]

df_MCS_2_5 <- MCS_2_5


for (i in 1:nrow(df_MCS_2_5)){

  MCS_STATS_P.5[2 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_2_5[i, ])] <-
    df_MCS_2_5$MCS.p.val[i]

}



### MCS Horizonte = 3

MCS_3_5 <- data.frame(estMCS(residuals.3 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_3_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_3_5 <- MCS_3_5[MCS_3_5$MCS.p.val >= alpha , ]

df_MCS_3_5 <- MCS_3_5



for (i in 1:nrow(df_MCS_3_5)){

  MCS_STATS_P.5[3 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_3_5[i, ])] <-
    df_MCS_3_5$MCS.p.val[i]


}



## MCS 4

MCS_4_5 <- data.frame(estMCS(residuals.4 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_4_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_4_5 <- MCS_4_5[MCS_4_5$MCS.p.val >= alpha , ]

df_MCS_4_5 <- MCS_4_5


for (i in 1:nrow(df_MCS_4_5)){

  MCS_STATS_P.5[4 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_4_5[i, ])] <-
    df_MCS_4_5$MCS.p.val[i]

}



## MCS 5

MCS_5_5 <- data.frame(estMCS(residuals.5 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_5_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_5_5 <- MCS_5_5[MCS_5_5$MCS.p.val >= alpha , ]

df_MCS_5_5 <- MCS_5_5


for (i in 1:nrow(df_MCS_5_5)){

  MCS_STATS_P.5[5 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_5_5[i, ])] <-
    df_MCS_5_5$MCS.p.val[i]

}



### MCS Horizonte = 6


MCS_6_5 <- data.frame(estMCS(residuals.6 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_6_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_6_5 <- MCS_6_5[MCS_6_5$MCS.p.val >= alpha , ]

df_MCS_6_5 <- MCS_6_5


for (i in 1:nrow(df_MCS_6_5)){

  MCS_STATS_P.5[6 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_6_5[i, ])] <-
    df_MCS_6_5$MCS.p.val[i]


}



## MCS 7

MCS_7_5 <- data.frame(estMCS(residuals.7 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_7_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_7_5 <- MCS_7_5[MCS_7_5$MCS.p.val >= alpha , ]

df_MCS_7_5 <- MCS_7_5

for (i in 1:nrow(df_MCS_7_5)){

  MCS_STATS_P.5[7 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_7_5[i, ])] <-
    df_MCS_7_5$MCS.p.val[i]

}



## MCS 8


MCS_8_5 <- data.frame(estMCS(residuals.8 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_8_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_8_5 <- MCS_8_5[MCS_8_5$MCS.p.val >= alpha , ]

df_MCS_8_5 <- MCS_8_5

for (i in 1:nrow(df_MCS_8_5)){

  MCS_STATS_P.5[8 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_8_5[i, ])] <-
    df_MCS_8_5$MCS.p.val[i]

}



### MCS Horizonte = 9

MCS_9_5 <- data.frame(estMCS(residuals.9 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_9_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_9_5 <- MCS_9_5[MCS_9_5$MCS.p.val >= alpha , ]

df_MCS_9_5 <- MCS_9_5


for (i in 1:nrow(df_MCS_9_5)){

  MCS_STATS_P.5[9 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_9_5[i, ])] <-
    df_MCS_9_5$MCS.p.val[i]


}




## MCS 10

MCS_10_5 <- data.frame(estMCS(residuals.10 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_10_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_10_5 <- MCS_10_5[MCS_10_5$MCS.p.val >= alpha , ]

df_MCS_10_5 <- MCS_10_5


for (i in 1:nrow(df_MCS_10_5)){

  MCS_STATS_P.5[10 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_10_5[i, ])] <-
    df_MCS_10_5$MCS.p.val[i]

}



## MCS 11


MCS_11_5 <- data.frame(estMCS(residuals.11 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_11_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_11_5 <- MCS_11_5[MCS_11_5$MCS.p.val >= alpha , ]

df_MCS_11_5 <- MCS_11_5


for (i in 1:nrow(df_MCS_11_5)){

  MCS_STATS_P.5[11 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_11_5[i, ])] <-
    df_MCS_11_5$MCS.p.val[i]

}



### MCS Horizonte = 12

MCS_12_5 <- data.frame(estMCS(residuals.12 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_12_5$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_12_5 <- MCS_12_5[MCS_12_5$MCS.p.val >= alpha , ]

df_MCS_12_5 <- MCS_12_5

for (i in 1:nrow(df_MCS_12_5)){

  MCS_STATS_P.5[12 ,colnames(MCS_STATS_P.5) == rownames(df_MCS_12_5[i, ])] <-
    df_MCS_12_5$MCS.p.val[i]


}



# Comparando Modelos 2020-04-01 até 2020-12-01

## Gerando resíduos ao quadrado para cada horizonte



library(MCS) #mcs test
library(forecast) #diebold and mariano test

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


  assign(df , data.frame(aux_AR13 , aux_RW , aux_DDD
                         ,aux_AR13_IC , aux_cluster , aux_SDD , aux_combination ,
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

residuals.1 <- (df.1 - horiz.1)^2
residuals.2 <- (df.2 - horiz.2)^2
residuals.3 <- (df.3 - horiz.3)^2
residuals.4 <- (df.4 - horiz.4)^2
residuals.5 <- (df.5 - horiz.5)^2
residuals.6 <- (df.6 - horiz.6)^2
residuals.7 <- (df.7 - horiz.7)^2
residuals.8 <- (df.8 - horiz.8)^2
residuals.9 <- (df.9 - horiz.9)^2
residuals.10 <- (df.10 - horiz.10)^2
residuals.11 <- (df.11 - horiz.11)^2
residuals.12 <- (df.12 - horiz.12)^2

colnames(residuals.1) <- models

residuals.1.mod <- residuals.1[!duplicated(as.list(residuals.1))]

colnames(residuals.2) <- models

colnames(residuals.3) <- models

colnames(residuals.4) <- models

colnames(residuals.5) <- models

colnames(residuals.6) <- models

colnames(residuals.7) <- models

colnames(residuals.8) <- models

colnames(residuals.9) <- models

colnames(residuals.10) <- models

colnames(residuals.11) <- models

colnames(residuals.12) <- models



# 2020-04-01 até 2020-12-01

first <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2020-04-01')

last <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2020-12-01')



residuals.1 <- residuals.1[first:last , ]
residuals.1.mod <- residuals.1.mod[first:last , ]
residuals.2 <- residuals.2[first:last , ]
residuals.3 <- residuals.3[first:last , ]
residuals.4 <- residuals.4[first:last , ]
residuals.5 <- residuals.5[first:last , ]
residuals.6 <- residuals.6[first:last , ]
residuals.7 <- residuals.7[first:last , ]
residuals.8 <- residuals.8[first:last , ]
residuals.9 <- residuals.9[first:last , ]
residuals.10 <- residuals.10[first:last , ]
residuals.11 <- residuals.11[first:last , ]
residuals.12 <- residuals.12[first:last , ]





## MSFE


### MSFE horizonte = 1


MSFE_1_6 <- as.data.frame((colMeans(residuals.1))^(1/2))


## Dataframe para salvar as estatísticas por recorte de tempo

MSFE_P.6 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MSFE_P.6) <- colnames(residuals.1)


for (i in 1:nrow(MSFE_1_6)){

  MSFE_P.6[1 ,colnames(MSFE_P.6) == rownames(MSFE_1_6)[i]] <-
    MSFE_1_6$`(colMeans(residuals.1))^(1/2)`[i]

  MSFE_H.1[6 ,colnames(MSFE_H.1) == rownames(MSFE_1_6)[i]] <-
    MSFE_1_6$`(colMeans(residuals.1))^(1/2)`[i]

}





### MSFE horizonte = 2

MSFE_2_6 <- as.data.frame((colMeans(residuals.2))^(1/2))


for (i in 1:nrow(MSFE_2_6)){

  MSFE_P.6[2 ,colnames(MSFE_P.6) == rownames(MSFE_2_6)[i]] <-
    MSFE_2_6$`(colMeans(residuals.2))^(1/2)`[i]

}



### MSFE horizonte = 3

MSFE_3_6 <- as.data.frame((colMeans(residuals.3))^(1/2))


for (i in 1:nrow(MSFE_3_6)){

  MSFE_P.6[3 ,colnames(MSFE_P.6) == rownames(MSFE_3_6)[i]] <-
    MSFE_3_6$`(colMeans(residuals.3))^(1/2)`[i]

  MSFE_H.3[6 ,colnames(MSFE_H.3) == rownames(MSFE_3_6)[i]] <-
    MSFE_3_6$`(colMeans(residuals.3))^(1/2)`[i]

}





### MSFE horizonte = 4

MSFE_4_6 <- as.data.frame((colMeans(residuals.4))^(1/2))


for (i in 1:nrow(MSFE_4_6)){

  MSFE_P.6[4 ,colnames(MSFE_P.6) == rownames(MSFE_4_6)[i]] <-
    MSFE_4_6$`(colMeans(residuals.4))^(1/2)`[i]

}



### MSFE horizonte = 5


MSFE_5_6 <- as.data.frame((colMeans(residuals.5))^(1/2))


for (i in 1:nrow(MSFE_5_6)){

  MSFE_P.6[5 ,colnames(MSFE_P.6) == rownames(MSFE_5_6)[i]] <-
    MSFE_5_6$`(colMeans(residuals.5))^(1/2)`[i]

}



### MSFE horizonte = 6

MSFE_6_6 <- as.data.frame((colMeans(residuals.6))^(1/2))


for (i in 1:nrow(MSFE_6_6)){

  MSFE_P.6[6 ,colnames(MSFE_P.6) == rownames(MSFE_6_6)[i]] <-
    MSFE_6_6$`(colMeans(residuals.6))^(1/2)`[i]

  MSFE_H.6[6 ,colnames(MSFE_H.6) == rownames(MSFE_6_6)[i]] <-
    MSFE_6_6$`(colMeans(residuals.6))^(1/2)`[i]

}



### MSFE horizonte = 7

MSFE_7_6 <- as.data.frame((colMeans(residuals.7))^(1/2))


for (i in 1:nrow(MSFE_7_6)){

  MSFE_P.6[7 ,colnames(MSFE_P.6) == rownames(MSFE_7_6)[i]] <-
    MSFE_7_6$`(colMeans(residuals.7))^(1/2)`[i]

}



### MSFE horizonte = 8

MSFE_8_6 <- as.data.frame((colMeans(residuals.8))^(1/2))


for (i in 1:nrow(MSFE_8_6)){

  MSFE_P.6[8 ,colnames(MSFE_P.6) == rownames(MSFE_8_6)[i]] <-
    MSFE_8_6$`(colMeans(residuals.8))^(1/2)`[i]

}



### MSFE horizonte = 9

MSFE_9_6 <- as.data.frame((colMeans(residuals.9))^(1/2))


for (i in 1:nrow(MSFE_9_6)){

  MSFE_P.6[9 ,colnames(MSFE_P.6) == rownames(MSFE_9_6)[i]] <-
    MSFE_9_6$`(colMeans(residuals.9))^(1/2)`[i]

  MSFE_H.9[6 ,colnames(MSFE_H.9) == rownames(MSFE_9_6)[i]] <-
    MSFE_9_6$`(colMeans(residuals.9))^(1/2)`[i]

}



### MSFE horizonte = 10

MSFE_10_6 <- as.data.frame((colMeans(residuals.10))^(1/2))


for (i in 1:nrow(MSFE_10_6)){

  MSFE_P.6[10 ,colnames(MSFE_P.6) == rownames(MSFE_10_6)[i]] <-
    MSFE_10_6$`(colMeans(residuals.10))^(1/2)`[i]

}



### MSFE horizonte = 11

MSFE_11_6 <- as.data.frame((colMeans(residuals.11))^(1/2))


for (i in 1:nrow(MSFE_11_6)){

  MSFE_P.6[11 ,colnames(MSFE_P.6) == rownames(MSFE_11_6)[i]] <-
    MSFE_11_6$`(colMeans(residuals.11))^(1/2)`[i]

}



### MSFE horizonte = 12

MSFE_12_6 <- as.data.frame((colMeans(residuals.12))^(1/2))


for (i in 1:nrow(MSFE_12_6)){

  MSFE_P.6[12 ,colnames(MSFE_P.6) == rownames(MSFE_12_6)[i]] <-
    MSFE_12_6$`(colMeans(residuals.12))^(1/2)`[i]

  MSFE_H.12[6 ,colnames(MSFE_H.12) == rownames(MSFE_12_6)[i]] <-
    MSFE_12_6$`(colMeans(residuals.12))^(1/2)`[i]

}



## MCS
### MCS Horizonte = 1


MCS_1_6 <- data.frame(estMCS(residuals.1.mod , B = bootstrap , l = 2))

ordernacao <- order(MCS_1_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_1_6 <- MCS_1_6[MCS_1_6$MCS.p.val >= alpha , ]

df_MCS_1_6 <- MCS_1_6

## Dataframe para salvar as estatísticas por recorte de tempo

MCS_STATS_P.6 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MCS_STATS_P.6) <- colnames(residuals.1)

for (i in 1:nrow(df_MCS_1_6)){

  MCS_STATS_P.6[1 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_1_6[i, ])] <-
    df_MCS_1_6$MCS.p.val[i]


}



### MCS Horizonte = 2


MCS_2_6 <- data.frame(estMCS(residuals.2 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_2_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_2_6 <- MCS_2_6[MCS_2_6$MCS.p.val >= alpha , ]

df_MCS_2_6 <- MCS_2_6



for (i in 1:nrow(df_MCS_2_6)){

  MCS_STATS_P.6[2 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_2_6[i, ])] <-
    df_MCS_2_6$MCS.p.val[i]

}



### MCS Horizonte = 3

MCS_3_6 <- data.frame(estMCS(residuals.3 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_3_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_3_6 <- MCS_3_6[MCS_3_6$MCS.p.val >= alpha , ]

df_MCS_3_6 <- MCS_3_6


for (i in 1:nrow(df_MCS_3_6)){

  MCS_STATS_P.6[3 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_3_6[i, ])] <-
    df_MCS_3_6$MCS.p.val[i]


}



## MCS 4

MCS_4_6 <- data.frame(estMCS(residuals.4 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_4_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_4_6 <- MCS_4_6[MCS_4_6$MCS.p.val >= alpha , ]

df_MCS_4_6 <- MCS_4_6



for (i in 1:nrow(df_MCS_4_6)){

  MCS_STATS_P.6[4 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_4_6[i, ])] <-
    df_MCS_4_6$MCS.p.val[i]

}



## MCS 5

MCS_5_6 <- data.frame(estMCS(residuals.5 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_5_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_5_6 <- MCS_5_6[MCS_5_6$MCS.p.val >= alpha , ]

df_MCS_5_6 <- MCS_5_6



for (i in 1:nrow(df_MCS_5_6)){

  MCS_STATS_P.6[5 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_5_6[i, ])] <-
    df_MCS_5_6$MCS.p.val[i]

}



### MCS Horizonte = 6


MCS_6_6 <- data.frame(estMCS(residuals.6 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_6_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_6_6 <- MCS_6_6[MCS_6_6$MCS.p.val >= alpha , ]

df_MCS_6_6 <- MCS_6_6



for (i in 1:nrow(df_MCS_6_6)){

  MCS_STATS_P.6[6 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_6_6[i, ])] <-
    df_MCS_6_6$MCS.p.val[i]


}



## MCS 7

MCS_7_6 <- data.frame(estMCS(residuals.7 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_7_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_7_6 <- MCS_7_6[MCS_7_6$MCS.p.val >= alpha , ]

df_MCS_7_6 <- MCS_7_6



for (i in 1:nrow(df_MCS_7_6)){

  MCS_STATS_P.6[7 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_7_6[i, ])] <-
    df_MCS_7_6$MCS.p.val[i]

}



## MCS 8

MCS_8_6 <- data.frame(estMCS(residuals.8 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_8_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_8_6 <- MCS_8_6[MCS_8_6$MCS.p.val >= alpha , ]

df_MCS_8_6 <- MCS_8_6


for (i in 1:nrow(df_MCS_8_6)){

  MCS_STATS_P.6[8 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_8_6[i, ])] <-
    df_MCS_8_6$MCS.p.val[i]

}



### MCS Horizonte = 9

MCS_9_6 <- data.frame(estMCS(residuals.9 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_9_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_9_6 <- MCS_9_6[MCS_9_6$MCS.p.val >= alpha , ]

df_MCS_9_6 <- MCS_9_6


for (i in 1:nrow(df_MCS_9_6)){

  MCS_STATS_P.6[9 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_9_6[i, ])] <-
    df_MCS_9_6$MCS.p.val[i]


}




## MCS 10

MCS_10_6 <- data.frame(estMCS(residuals.10 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_10_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_10_6 <- MCS_10_6[MCS_10_6$MCS.p.val >= alpha , ]

df_MCS_10_6 <- MCS_10_6


for (i in 1:nrow(df_MCS_10_6)){

  MCS_STATS_P.6[10 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_10_6[i, ])] <-
    df_MCS_10_6$MCS.p.val[i]

}



## MCS 11

MCS_11_6 <- data.frame(estMCS(residuals.11 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_11_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_11_6 <- MCS_11_6[MCS_11_6$MCS.p.val >= alpha , ]

df_MCS_11_6 <- MCS_11_6


for (i in 1:nrow(df_MCS_11_6)){

  MCS_STATS_P.6[11 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_11_6[i, ])] <-
    df_MCS_11_6$MCS.p.val[i]

}



### MCS Horizonte = 12


MCS_12_6 <- data.frame(estMCS(residuals.12 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_12_6$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_12_6 <- MCS_12_6[MCS_12_6$MCS.p.val >= alpha , ]

df_MCS_12_6 <- MCS_12_6



for (i in 1:nrow(df_MCS_12_6)){

  MCS_STATS_P.6[12 ,colnames(MCS_STATS_P.6) == rownames(df_MCS_12_6[i, ])] <-
    df_MCS_12_6$MCS.p.val[i]


}




# Comparando Modelos 2020-04-01 até 2021-12-01

## Gerando resíduos ao quadrado para cada horizonte



library(MCS) #mcs test
library(forecast) #diebold and mariano test

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


  assign(df , data.frame(aux_AR13 , aux_RW , aux_DDD
                         ,aux_AR13_IC , aux_cluster , aux_SDD , aux_combination ,
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

residuals.1 <- (df.1 - horiz.1)^2
residuals.2 <- (df.2 - horiz.2)^2
residuals.3 <- (df.3 - horiz.3)^2
residuals.4 <- (df.4 - horiz.4)^2
residuals.5 <- (df.5 - horiz.5)^2
residuals.6 <- (df.6 - horiz.6)^2
residuals.7 <- (df.7 - horiz.7)^2
residuals.8 <- (df.8 - horiz.8)^2
residuals.9 <- (df.9 - horiz.9)^2
residuals.10 <- (df.10 - horiz.10)^2
residuals.11 <- (df.11 - horiz.11)^2
residuals.12 <- (df.12 - horiz.12)^2

colnames(residuals.1) <- models

residuals.1.mod <- residuals.1[!duplicated(as.list(residuals.1))]

colnames(residuals.2) <- models

colnames(residuals.3) <- models

colnames(residuals.4) <- models

colnames(residuals.5) <- models

colnames(residuals.6) <- models

colnames(residuals.7) <- models

colnames(residuals.8) <- models

colnames(residuals.9) <- models

colnames(residuals.10) <- models

colnames(residuals.11) <- models

colnames(residuals.12) <- models



# 2020-04-01 até 2020-12-01

first <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2020-04-01')

last <- which(
  DATA$`data$date[(window_size + 1):(nrow(data) - 11)]` == '2021-12-01')



residuals.1 <- residuals.1[first:last , ]
residuals.1.mod <- residuals.1.mod[first:last , ]
residuals.2 <- residuals.2[first:last , ]
residuals.3 <- residuals.3[first:last , ]
residuals.4 <- residuals.4[first:last , ]
residuals.5 <- residuals.5[first:last , ]
residuals.6 <- residuals.6[first:last , ]
residuals.7 <- residuals.7[first:last , ]
residuals.8 <- residuals.8[first:last , ]
residuals.9 <- residuals.9[first:last , ]
residuals.10 <- residuals.10[first:last , ]
residuals.11 <- residuals.11[first:last , ]
residuals.12 <- residuals.12[first:last , ]





## MSFE


### MSFE horizonte = 1


MSFE_1_7 <- as.data.frame((colMeans(residuals.1))^(1/2))


## Dataframe para salvar as estatísticas por recorte de tempo

MSFE_P.7 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MSFE_P.7) <- colnames(residuals.1)


for (i in 1:nrow(MSFE_1_7)){

  MSFE_P.7[1 ,colnames(MSFE_P.7) == rownames(MSFE_1_7)[i]] <-
    MSFE_1_7$`(colMeans(residuals.1))^(1/2)`[i]

  MSFE_H.1[7 ,colnames(MSFE_H.1) == rownames(MSFE_1_7)[i]] <-
    MSFE_1_7$`(colMeans(residuals.1))^(1/2)`[i]

}





### MSFE horizonte = 2

MSFE_2_7 <- as.data.frame((colMeans(residuals.2))^(1/2))


for (i in 1:nrow(MSFE_2_7)){

  MSFE_P.7[2 ,colnames(MSFE_P.7) == rownames(MSFE_2_7)[i]] <-
    MSFE_2_7$`(colMeans(residuals.2))^(1/2)`[i]

}



### MSFE horizonte = 3

MSFE_3_7 <- as.data.frame((colMeans(residuals.3))^(1/2))


for (i in 1:nrow(MSFE_3_7)){

  MSFE_P.7[3 ,colnames(MSFE_P.7) == rownames(MSFE_3_7)[i]] <-
    MSFE_3_7$`(colMeans(residuals.3))^(1/2)`[i]

  MSFE_H.3[7 ,colnames(MSFE_H.3) == rownames(MSFE_3_7)[i]] <-
    MSFE_3_7$`(colMeans(residuals.3))^(1/2)`[i]

}





### MSFE horizonte = 4

MSFE_4_7 <- as.data.frame((colMeans(residuals.4))^(1/2))


for (i in 1:nrow(MSFE_4_7)){

  MSFE_P.7[4 ,colnames(MSFE_P.7) == rownames(MSFE_4_7)[i]] <-
    MSFE_4_7$`(colMeans(residuals.4))^(1/2)`[i]

}



### MSFE horizonte = 5


MSFE_5_7 <- as.data.frame((colMeans(residuals.5))^(1/2))


for (i in 1:nrow(MSFE_5_7)){

  MSFE_P.7[5 ,colnames(MSFE_P.7) == rownames(MSFE_5_7)[i]] <-
    MSFE_5_7$`(colMeans(residuals.5))^(1/2)`[i]

}



### MSFE horizonte = 6

MSFE_6_7 <- as.data.frame((colMeans(residuals.6))^(1/2))


for (i in 1:nrow(MSFE_6_7)){

  MSFE_P.7[6 ,colnames(MSFE_P.7) == rownames(MSFE_6_7)[i]] <-
    MSFE_6_7$`(colMeans(residuals.6))^(1/2)`[i]

  MSFE_H.6[7 ,colnames(MSFE_H.6) == rownames(MSFE_6_7)[i]] <-
    MSFE_6_7$`(colMeans(residuals.6))^(1/2)`[i]

}



### MSFE horizonte = 7

MSFE_7_7 <- as.data.frame((colMeans(residuals.7))^(1/2))


for (i in 1:nrow(MSFE_7_7)){

  MSFE_P.7[7 ,colnames(MSFE_P.7) == rownames(MSFE_7_7)[i]] <-
    MSFE_7_7$`(colMeans(residuals.7))^(1/2)`[i]

}



### MSFE horizonte = 8

MSFE_8_7 <- as.data.frame((colMeans(residuals.8))^(1/2))


for (i in 1:nrow(MSFE_8_7)){

  MSFE_P.7[8 ,colnames(MSFE_P.7) == rownames(MSFE_8_7)[i]] <-
    MSFE_8_7$`(colMeans(residuals.8))^(1/2)`[i]

}



### MSFE horizonte = 9

MSFE_9_7 <- as.data.frame((colMeans(residuals.9))^(1/2))


for (i in 1:nrow(MSFE_9_7)){

  MSFE_P.7[9 ,colnames(MSFE_P.7) == rownames(MSFE_9_7)[i]] <-
    MSFE_9_7$`(colMeans(residuals.9))^(1/2)`[i]

  MSFE_H.9[7 ,colnames(MSFE_H.9) == rownames(MSFE_9_7)[i]] <-
    MSFE_9_7$`(colMeans(residuals.9))^(1/2)`[i]

}



### MSFE horizonte = 10

MSFE_10_7 <- as.data.frame((colMeans(residuals.10))^(1/2))


for (i in 1:nrow(MSFE_10_7)){

  MSFE_P.7[10 ,colnames(MSFE_P.7) == rownames(MSFE_10_7)[i]] <-
    MSFE_10_7$`(colMeans(residuals.10))^(1/2)`[i]

}



### MSFE horizonte = 11

MSFE_11_7 <- as.data.frame((colMeans(residuals.11))^(1/2))


for (i in 1:nrow(MSFE_11_7)){

  MSFE_P.7[11 ,colnames(MSFE_P.7) == rownames(MSFE_11_7)[i]] <-
    MSFE_11_7$`(colMeans(residuals.11))^(1/2)`[i]

}



### MSFE horizonte = 12

MSFE_12_7 <- as.data.frame((colMeans(residuals.12))^(1/2))


for (i in 1:nrow(MSFE_12_7)){

  MSFE_P.7[12 ,colnames(MSFE_P.7) == rownames(MSFE_12_7)[i]] <-
    MSFE_12_7$`(colMeans(residuals.12))^(1/2)`[i]

  MSFE_H.12[7 ,colnames(MSFE_H.12) == rownames(MSFE_12_7)[i]] <-
    MSFE_12_7$`(colMeans(residuals.12))^(1/2)`[i]

}



## MCS
### MCS Horizonte = 1

MCS_1_7 <- data.frame(estMCS(residuals.1.mod , B = bootstrap , l = block_length))

ordernacao <- order(MCS_1_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_1_7 <- MCS_1_7[MCS_1_7$MCS.p.val >= alpha , ]

df_MCS_1_7 <- MCS_1_7

## Dataframe para salvar as estatísticas por recorte de tempo

MCS_STATS_P.7 <- data.frame(matrix(nrow = 12 , ncol = ncol(residuals.1)))

colnames(MCS_STATS_P.7) <- colnames(residuals.1)

for (i in 1:nrow(df_MCS_1_7)){

  MCS_STATS_P.7[1 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_1_7[i, ])] <-
    df_MCS_1_7$MCS.p.val[i]

}



### MCS Horizonte = 2

MCS_2_7 <- data.frame(estMCS(residuals.2 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_2_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_2_7 <- MCS_2_7[MCS_2_7$MCS.p.val >= alpha , ]

df_MCS_2_7 <- MCS_2_7

for (i in 1:nrow(df_MCS_2_7)){

  MCS_STATS_P.7[2 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_2_7[i, ])] <-
    df_MCS_2_7$MCS.p.val[i]

}



### MCS Horizonte = 3

MCS_3_7 <- data.frame(estMCS(residuals.3 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_3_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_3_7 <- MCS_3_7[MCS_3_7$MCS.p.val >= alpha , ]

df_MCS_3_7 <- MCS_3_7


for (i in 1:nrow(df_MCS_3_7)){

  MCS_STATS_P.7[3 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_3_7[i, ])] <-
    df_MCS_3_7$MCS.p.val[i]


}



## MCS 4

MCS_4_7 <- data.frame(estMCS(residuals.4 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_4_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_4_7 <- MCS_4_7[MCS_4_7$MCS.p.val >= alpha , ]

df_MCS_4_7 <- MCS_4_7



for (i in 1:nrow(df_MCS_4_7)){

  MCS_STATS_P.7[4 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_4_7[i, ])] <-
    df_MCS_4_7$MCS.p.val[i]

}



## MCS 5


MCS_5_7 <- data.frame(estMCS(residuals.5 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_5_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_5_7 <- MCS_5_7[MCS_5_7$MCS.p.val >= alpha , ]

df_MCS_5_7 <- MCS_5_7



for (i in 1:nrow(df_MCS_5_7)){

  MCS_STATS_P.7[5 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_5_7[i, ])] <-
    df_MCS_5_7$MCS.p.val[i]

}



### MCS Horizonte = 6


MCS_6_7 <- data.frame(estMCS(residuals.6 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_6_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_6_7 <- MCS_6_7[MCS_6_7$MCS.p.val >= alpha , ]

df_MCS_6_7 <- MCS_6_7


for (i in 1:nrow(df_MCS_6_7)){

  MCS_STATS_P.7[6 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_6_7[i, ])] <-
    df_MCS_6_7$MCS.p.val[i]


}



## MCS 7


MCS_7_7 <- data.frame(estMCS(residuals.7 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_7_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_7_7 <- MCS_7_7[MCS_7_7$MCS.p.val >= alpha , ]

df_MCS_7_7 <- MCS_7_7



for (i in 1:nrow(df_MCS_7_7)){

  MCS_STATS_P.7[7 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_7_7[i, ])] <-
    df_MCS_7_7$MCS.p.val[i]

}



## MCS 8


MCS_8_7 <- data.frame(estMCS(residuals.8 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_8_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_8_7 <- MCS_8_7[MCS_8_7$MCS.p.val >= alpha , ]

df_MCS_8_7 <- MCS_8_7


for (i in 1:nrow(df_MCS_8_7)){

  MCS_STATS_P.7[8 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_8_7[i, ])] <-
    df_MCS_8_7$MCS.p.val[i]

}



### MCS Horizonte = 9

MCS_9_7 <- data.frame(estMCS(residuals.9 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_9_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_9_7 <- MCS_9_7[MCS_9_7$MCS.p.val >= alpha , ]

df_MCS_9_7 <- MCS_9_7



for (i in 1:nrow(df_MCS_9_7)){

  MCS_STATS_P.7[9 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_9_7[i, ])] <-
    df_MCS_9_7$MCS.p.val[i]


}




## MCS 10


MCS_10_7 <- data.frame(estMCS(residuals.10 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_10_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_10_7 <- MCS_10_7[MCS_10_7$MCS.p.val >= alpha , ]

df_MCS_10_7 <- MCS_10_7


for (i in 1:nrow(df_MCS_10_7)){

  MCS_STATS_P.7[10 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_10_7[i, ])] <-
    df_MCS_10_7$MCS.p.val[i]

}



## MCS 11

MCS_11_7 <- data.frame(estMCS(residuals.11 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_11_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_11_7 <- MCS_11_7[MCS_11_7$MCS.p.val >= alpha , ]

df_MCS_11_7 <- MCS_11_7


for (i in 1:nrow(df_MCS_11_7)){

  MCS_STATS_P.7[11 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_11_7[i, ])] <-
    df_MCS_11_7$MCS.p.val[i]

}



### MCS Horizonte = 12


MCS_12_7 <- data.frame(estMCS(residuals.12 , B = bootstrap , l = block_length))

ordernacao <- order(MCS_12_7$MCS.p.val , decreasing = TRUE)[indice_max]

alpha <- ALPHA

MCS_12_7 <- MCS_12_7[MCS_12_7$MCS.p.val >= alpha , ]

df_MCS_12_7 <- MCS_12_7


for (i in 1:nrow(df_MCS_12_7)){

  MCS_STATS_P.7[12 ,colnames(MCS_STATS_P.7) == rownames(df_MCS_12_7[i, ])] <-
    df_MCS_12_7$MCS.p.val[i]


}

save.image(
  paste0("\\Workspaces\\", 'mcs_previa_alpha25_correct_', popo))

rm(list=ls()[! ls() %in% c('MCS_12_7', 'MCS_11_7', 'MCS_10_7', 'MCS_9_7', 'MCS_8_7',
                           'MCS_7_7', 'MCS_6_7', 'MCS_5_7', 'MCS_4_7', 'MCS_3_7',
                           'MCS_2_7', 'MCS_1_7', 'MCS_12_6', 'MCS_11_6', 'MCS_10_6',
                           'MCS_9_6', 'MCS_8_6', 'MCS_7_6', 'MCS_6_6', 'MCS_5_6', 'MCS_4_6',
                           'MCS_3_6', 'MCS_2_6', 'MCS_1_6', 'MCS_12_5', 'MCS_11_5',
                           'MCS_10_5', 'MCS_9_5', 'MCS_8_5', 'MCS_7_5', 'MCS_6_5',
                           'MCS_5_5', 'MCS_4_5', 'MCS_3_5', 'MCS_2_5', 'MCS_1_5',
                           'MCS_12_4', 'MCS_11_4', 'MCS_10_4', 'MCS_9_4', 'MCS_8_4',
                           'MCS_7_4', 'MCS_6_4', 'MCS_5_4', 'MCS_4_4', 'MCS_3_4', 'MCS_2_4',
                           'MCS_1_4', 'MCS_12_3', 'MCS_11_3', 'MCS_10_3', 'MCS_9_3',
                           'MCS_8_3', 'MCS_7_3', 'MCS_6_3', 'MCS_5_3', 'MCS_4_3', 'MCS_3_3',
                           'MCS_2_3', 'MCS_1_3', 'MCS_12_2', 'MCS_11_2', 'MCS_10_2',
                           'MCS_9_2', 'MCS_8_2', 'MCS_7_2', 'MCS_6_2', 'MCS_5_2', 'MCS_4_2',
                           'MCS_3_2', 'MCS_2_2', 'MCS_1_2', 'MCS_12_1', 'MCS_11_1',
                           'MCS_10_1', 'MCS_9_1', 'MCS_8_1', 'MCS_7_1', 'MCS_6_1',
                           'MCS_5_1', 'MCS_4_1', 'MCS_3_1', 'MCS_2_1', 'MCS_1_1',
                           'MCS_STATS_P.7', 'MCS_STATS_P.6',
                           'MCS_STATS_P.5', 'MCS_STATS_P.4', 'MCS_STATS_P.3'
                           , 'MCS_STATS_P.2', 'MCS_STATS_P.1', 'MCS_STATS_H.12'
                           , 'MCS_STATS_H.9'
                           , 'MCS_STATS_H.6'
                           , 'MCS_STATS_H.3'
                           , 'MCS_STATS_H.1',
                           'MSFE_P.7', 'MSFE_P.6',
                           'MSFE_P.5', 'MSFE_P.4', 'MSFE_P.3'
                           , 'MSFE_P.2', 'MSFE_P.1', 'MSFE_H.12'
                           , 'MSFE_H.9'
                           , 'MSFE_H.6'
                           , 'MSFE_H.3'
                           , 'MSFE_H.1', 'MSFE_12_7', 'MSFE_11_7', 'MSFE_10_7',
                           'MSFE_9_7', 'MSFE_8_7',
                           'MSFE_7_7', 'MSFE_6_7', 'MSFE_5_7', 'MSFE_4_7', 'MSFE_3_7',
                           'MSFE_2_7', 'MSFE_1_7', 'MSFE_12_6', 'MSFE_11_6', 'MSFE_10_6',
                           'MSFE_9_6', 'MSFE_8_6', 'MSFE_7_6', 'MSFE_6_6', 'MSFE_5_6',
                           'MSFE_4_6',
                           'MSFE_3_6', 'MSFE_2_6', 'MSFE_1_6', 'MSFE_12_5', 'MSFE_11_5',
                           'MSFE_10_5', 'MSFE_9_5', 'MSFE_8_5', 'MSFE_7_5', 'MSFE_6_5',
                           'MSFE_5_5', 'MSFE_4_5', 'MSFE_3_5', 'MSFE_2_5', 'MSFE_1_5',
                           'MSFE_12_4', 'MSFE_11_4', 'MSFE_10_4', 'MSFE_9_4', 'MSFE_8_4',
                           'MSFE_7_4', 'MSFE_6_4', 'MSFE_5_4', 'MSFE_4_4', 'MSFE_3_4',
                           'MSFE_2_4',
                           'MSFE_1_4', 'MSFE_12_3', 'MSFE_11_3', 'MSFE_10_3', 'MSFE_9_3',
                           'MSFE_8_3', 'MSFE_7_3', 'MSFE_6_3', 'MSFE_5_3', 'MSFE_4_3',
                           'MSFE_3_3',
                           'MSFE_2_3', 'MSFE_1_3', 'MSFE_12_2', 'MSFE_11_2', 'MSFE_10_2',
                           'MSFE_9_2', 'MSFE_8_2', 'MSFE_7_2', 'MSFE_6_2', 'MSFE_5_2',
                           'MSFE_4_2',
                           'MSFE_3_2', 'MSFE_2_2', 'MSFE_1_2', 'MSFE_12_1', 'MSFE_11_1',
                           'MSFE_10_1', 'MSFE_9_1', 'MSFE_8_1', 'MSFE_7_1', 'MSFE_6_1',
                           'MSFE_5_1', 'MSFE_4_1', 'MSFE_3_1', 'MSFE_2_1', 'MSFE_1_1' , 'popo')])



