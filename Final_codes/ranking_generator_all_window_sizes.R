
##########################


library(modelconf)
library(MCS) #mcs test
library(forecast) #diebold and mariano test

bootstrap <- 10000
block_length <- 2
indice_max <- 3
ALPHA <- 0.25

load("\\auxiliar\\residuals_final.RData")


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



# Comparando Modelos 2014-12 até 2015-12
## Gerando resíduos ao quadrado para cada horizonte

load("\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\residuals_final.RData")

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

load("\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\residuals_final.RData")

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

load("\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\residuals_final.RData")


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


load("\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\residuals_final.RData")


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


load("\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\residuals_final.RData")


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
  paste0('\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\mcs_procedure.RData'))

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

save.image(
  paste0('\\Users\\megda\\OneDrive\\Dissertação\\auxiliar\\MCS_final.RData'))

#### Generating Ranking

aux_list_ranking <- list(MCS_STATS_P.1  , MCS_STATS_P.3 ,
                         MCS_STATS_P.4 , MCS_STATS_P.5  , MCS_STATS_P.6 ,
                         MCS_STATS_P.7)


aux_list_names <- c('MCS_STATS_P.1'  , 'MCS_STATS_P.3' ,
                    'MCS_STATS_P.4' , 'MCS_STATS_P.5'  , 'MCS_STATS_P.6' ,
                    'MCS_STATS_P.7')

index <- 0


for (i in aux_list_ranking){
  
  index <- index + 1
  
  df_aux <- i
  
  name_ranking_matrix <- paste0(aux_list_names[index] , '_ranking')
  
  presence_na_1 <- df_aux[1 , is.na(df_aux[1, ])]
  
  
  STEP_AHEAD_1 <-
    paste(
      names(sort(unlist(df_aux[1 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[1 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  
  STEP_AHEAD_2 <-
    paste(
      names(sort(unlist(df_aux[2 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[2 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_3 <-
    paste(
      names(sort(unlist(df_aux[3 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[3 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_4 <-
    paste(
      names(sort(unlist(df_aux[4 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[4 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_5 <-
    paste(
      names(sort(unlist(df_aux[5 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[5 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_6 <-
    paste(
      names(sort(unlist(df_aux[6 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[6 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_7 <-
    paste(
      names(sort(unlist(df_aux[7 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[7 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_8 <-
    paste(
      names(sort(unlist(df_aux[8 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[8 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_9 <-
    paste(
      names(sort(unlist(df_aux[9 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[9 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_10 <-
    paste(
      names(sort(unlist(df_aux[10 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[10 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_11 <-
    paste(
      names(sort(unlist(df_aux[11 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[11 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  STEP_AHEAD_12 <-
    paste(
      names(sort(unlist(df_aux[12 , ]) ,
                 na.last = TRUE , decreasing = TRUE))  ,
      sort(unlist(df_aux[12 , ]) ,
           na.last = TRUE , decreasing = TRUE) , sep = " - stat:")
  
  
  assign(
    
    name_ranking_matrix ,
    data.frame(STEP_AHEAD_1 , STEP_AHEAD_2 , STEP_AHEAD_3 , STEP_AHEAD_4 ,
               STEP_AHEAD_5 , STEP_AHEAD_6 , STEP_AHEAD_7 , STEP_AHEAD_8 ,
               STEP_AHEAD_9 , STEP_AHEAD_10 , STEP_AHEAD_11 , STEP_AHEAD_12)
    
  )
  
  rm(STEP_AHEAD_1 , STEP_AHEAD_2 , STEP_AHEAD_3 , STEP_AHEAD_4 ,
     STEP_AHEAD_5 , STEP_AHEAD_6 , STEP_AHEAD_7 , STEP_AHEAD_8 ,
     STEP_AHEAD_9 , STEP_AHEAD_10 , STEP_AHEAD_11 , STEP_AHEAD_12)
  
  
  
}

rm(list=ls()[! ls() %in% c('MCS_STATS_P.7_ranking', 'MCS_STATS_P.6_ranking',
                           'MCS_STATS_P.5_ranking', 'MCS_STATS_P.4_ranking', 'MCS_STATS_P.3_ranking'
                           , 'MCS_STATS_P.2_ranking', 'MCS_STATS_P.1_ranking')])
