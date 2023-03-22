#' ---
#'
#' title: "Your title here"
#' Author: "Renam Megda"
#' date: "Todays date"
#' output:
#'   html_document:
#'     theme: journal
#'     highlight: espresso
#'     toc: true
#'     toc_depth: 4
#'     toc_float: true
#' ---
#' # 60 Observações
#'
#' # Base e análise dos dados
#'
#' ## Libraries
#'
## ---- message=FALSE---------------------------------------------------------------------------------------------------------


library(tvReg)
library(data.table)
library(sidrar)
library(ggplot2)
library(zoo)
library(readr)
library(quantmod)
library(lmtest)
library(aTSA)
library(matrixStats)
library(dplyr)
library(strucchange)
library(reshape)
library(TSclust)
library(dynlm)
library(dtwclust)
library(quantreg)
library(MTS)
library(MSwM)
library(glmnet)
library(xtable)


set.seed(1000)

setwd("\\Users\\megda\\OneDrive\\Dissertação")


#'
#'
#' ## Arrumando base de dados
#'
#' 1. Produção industrial desasonalizada $= \Delta_{12}prod\_ind$.
#' 2. Primeira diferença do produto desasonalizado $= \Delta\Delta_{12}prod\_ind_{t} =d\_desas\_prod$.
#' 3. Produto diferenciado pela média móvel de 4 $=  \Delta_{12}prod\_ind{t} - \frac{1}{4}\sum_{i=1}^{4}\Delta_{12}prod\_ind_{t-i} = sdd\_desas\_prod$
#'
## ---------------------------------------------------------------------------------------------------------------------------


data <- read.csv("Data\\data_prod_ind.csv")

data <- data[13:nrow(data),]

sdd_desas_prod <- 0
sdd_MA <- 0

### Gerando a primeira diferença do produto desasonalizado

data$d_desas_prod <- c(NA, diff(data$desas_prod))

### Gerando médias móveis do produto desasonalizado (PI em sua décima segunda
### diferença)

for (i in 5:nrow(data)){
  sdd_desas_prod[i-4] <- data$desas_prod[i] -
    (data$desas_prod[i-1]+data$desas_prod[i-2]+data$desas_prod[i-3]+
       data$desas_prod[i-4])/4
  sdd_MA[i-4] <- (data$desas_prod[i-1]+data$desas_prod[i-2]+data$desas_prod[i-3]+
       data$desas_prod[i-4])/4
}

### Como as 4 primeiras observações não possuem uma média móvel de 4 períodos,
### retirei-as.

data <- data[c(-1:-4),]

data$sdd_MA <- sdd_MA
data$sdd_desas_prod <- sdd_desas_prod

rownames(data) <- c(1:nrow(data))

rm(list = c("sdd_MA" , "sdd_desas_prod"))

write_csv(data , "Data\\data_tratada_.csv")


#'
#' ## Parâmetros
#'
#' 1. Window_size = tamanho da janela fixa utilizada.
#' 2. repetitions = Número de estimações realizadas à medida que andamos com
#' a janela
#' 3. Horizonte = horizonte de previsão
#'
## ---------------------------------------------------------------------------------------------------------------------------

repetitions <- nrow(data) - 12 - window_size
horizonte <- 12


#'
#'
#' ## Listas e dataframes
#'
#' Os dataframes que iniciam em *level_sample* são dataframes que contém nas linhas a
#' amostra junto com os 12 valores preditos para o nível da variável para cada
#' repetição das estimações (estou trabalhando com janela fixa). O data frame
#' *sample_and_predicted* irá conter a amostra da janela fixa junto com estimativas
#' feitas para a variável desasonalizada ($\Delta_{12}prod\_ind$) para cada um dos
#' modelos durante as repetições das estimações. Isso facilitará na hora de
#' converter nossas estimativas da produção desasonalizada para o nível estimado da
#' produção industrial.
#'
## ---------------------------------------------------------------------------------------------------------------------------

### Listas para salvar os resultados das regressões ###
#######################################

saved_models_AR13 <- list()

saved_models_AR1 <- list()

saved_models_RW <- list()

saved_models_DDD <- list()

saved_models_SDD <- list()

saved_models_AR13_IC <- list()

saved_models_tvVAR<- list()

saved_models_lasso_ar13 <- list()

saved_models_adalasso_ar13 <- list()

### Listas para salvar os valores da amostra junto com os valores preditos ###
#######################################

sample_and_predicted <- replicate(1:(window_size + horizonte),
                                  n = repetitions+1) %>%
  as.data.frame()

sample_and_predicted <- as.data.frame(sample_and_predicted)

level_sample_cluster <- sample_and_predicted

level_sample_cluster_combination <- sample_and_predicted

level_sample_cluster_rls <- sample_and_predicted

level_sample_cluster_rls_combination <- sample_and_predicted

level_sample_AR1<- sample_and_predicted

level_sample_cluster_check<- sample_and_predicted

level_sample_AR13 <- sample_and_predicted

level_sample_msAR13 <- sample_and_predicted

level_sample_RW <- sample_and_predicted

level_sample_DDD <-sample_and_predicted

level_sample_SDD <-sample_and_predicted

level_sample_AR13_IC <- sample_and_predicted

level_sample_tvVAR <-  sample_and_predicted

level_sample_lasso_AR13 <- sample_and_predicted

level_sample_adalasso_AR13 <- sample_and_predicted


data$date <- as.Date(data$date)

## Aqui abaixo temos um dataframe auxiliar para a parte de markov switch e
### LASSO-AR(13)

tf <- length(data[, 1]) * 14

matrix_desas_prod <-
  matrix(rep(NA , times =  tf) , nrow = nrow(data) , ncol = 14 ) |>
  as.data.frame()

matrix_d_desas_prod <-
  matrix(rep(NA , times =  tf) , nrow = nrow(data) , ncol = 14 ) |>
  as.data.frame()


for (i in 1:14){

  matrix_desas_prod[(i:nrow(matrix_desas_prod)) , i] <-
    data$desas_prod[1:(nrow(matrix_desas_prod) + 1 - i)]

  aux <- i - 1

  colnames(matrix_desas_prod)[i] <- paste0("desas_prod.-" , aux)


}
colnames(matrix_desas_prod)[1] <- "desas_prod"

for (i in 1:14){

  matrix_d_desas_prod[(i:nrow(matrix_d_desas_prod)) , i] <-
    data$d_desas_prod[1:(nrow(matrix_d_desas_prod) + 1 - i)]

  aux <- i - 1

  colnames(matrix_d_desas_prod)[i] <- paste0("d_desas_prod.-" , aux)


}
colnames(matrix_d_desas_prod)[1] <- "d_desas_prod"


#'
#'
#' # Método Direto
#'
#' ## Modelo AR(13)
#'
#' A seguir, a estimação do modelo AR(13). Aqui temos o modelo estimado de maneira
#' *direta*, ou seja, para a previsão para h passos a frente, a forma funcional
#' será: $\Delta_{12}prod\_ind_{t} = \beta_{0}^{h} + \sum_{i = h}^{i = h +12}\beta_{1 + i - h }^{h}\Delta_{12}prod\_ind_{t-i} + \epsilon_{t}$, onde $h$ é o horizonte de
#' previsão e estimamos coeficientes a cada horizonte analisado. Temos então que
#' $\hat{\Delta_{12}prod\_ind_{t+h}} = \hat{\beta_{0}^{h}} + \sum_{i = 1}^{i = 13}\hat{\beta_{i}^{h}}\Delta_{12}prod\_ind_{t}$. Note que $prod\_ind_{t}$ está
#' dentro da amostra. Para os demais modelos, a lógica é a mesma.
#'
#' Para recuperar o nível da série utilizando as
#' estimativas basta fazer $\hat{prod\_ind_{t+h}} = \hat{\Delta_{12}prod\_ind_{t+h}} + prod\_ind_{t + h - 12}$.
#' Como o horizonte vai até 12, $prod\_ind_{t + h - 12}$ está sempre dentro da
#' amostra.
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]

  level_sample_AR13[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)

  print(i)

  for (j in 1:horizonte){

    ### low será o índice da observação no instante "t" e up o índice da
    ### defasagem de ordem 13

    low <- window_size
    up <- window_size - 12

    saved_models_AR13[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample , j:(j+12)))[[1]]

    aux_vec_lags <- as.matrix(rep(1 , times = 14), nrow = 1 , ncol = 14)

    ### O aux_vec_lags é um vetor que contém as 13 primeiras defasagens

    aux_vec_lags[2:14] <- sample_and_predicted[low:up , (1+i)]

    aux_vec_lags <- t(aux_vec_lags)

    ### saved_models_AR13[[i + 1]] contém os coeficientes da repetição i

    aux_factor <- aux_vec_lags %*%
      saved_models_AR13[[i + 1]]

    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <- aux_factor[[1]]

    level_sample_AR13[(nrow(in_sample) + j) , (i + 1)] <- aux_factor[[1]] +
      level_sample_AR13[(nrow(in_sample) + j - 12 ) , (i + 1)]


  }

}


#'
#' ## AR 1
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]

  level_sample_AR1[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)

  print(i)

  for (j in 1:horizonte){

    ### low será o índice da observação no instante "t" e up o índice da
    ### defasagem de ordem 13

    low <- window_size

    saved_models_AR1[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample , j))[[1]]

    aux_vec_lags <- as.matrix(rep(1 , times = 2), nrow = 1 , ncol = 2)

    ### O aux_vec_lags é um vetor que contém as 13 primeiras defasagens

    aux_vec_lags[2] <- sample_and_predicted[low, (1+i)]

    aux_vec_lags <- t(aux_vec_lags)

    ### saved_models_AR13[[i + 1]] contém os coeficientes da repetição i

    aux_factor <- aux_vec_lags %*%
      saved_models_AR1[[i + 1]]

    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <- aux_factor[[1]]

    level_sample_AR1[(nrow(in_sample) + j) , (i + 1)] <- aux_factor[[1]] +
      level_sample_AR1[(nrow(in_sample) + j - 12 ) , (i + 1)]


  }

}


#'
#' ## Modelo RW
#'
#' Aqui temos um modelo de random walk sem drift, ou seja, $\Delta_{12}prod\_ind_{t+h} = \Delta_{12}prod\_ind_{t+h-1} + \epsilon_{t} = \Delta_{12}prod\_ind_{t} + \epsilon_{t}$.
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------
for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  #out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  level_sample_RW[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind
  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod


  print(i)

  for (j in 1:horizonte){

    low <- window_size + i + j
    up <- window_size + i + j - 12

    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <-
      sample_and_predicted[(nrow(in_sample)) , (i + 1)]

    level_sample_RW[(nrow(in_sample) + j) , (i + 1)] <-
      sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] +
      level_sample_RW[(nrow(in_sample) + j - 12) , (i + 1)]


  }

}

################################
 ## DOUBLE DIFFERENCE DEVICE AR 1 (DDD)
################################

### Criando objeto para salvar a produção desasonalizada, visto que agora a
### variável que eu salvo no sample_and_predicted é a dupla diferença e não mais
### a produção desasonalizada


desas_prod_DDD <-  replicate(1:(window_size + horizonte),
                         n = repetitions+1) %>%
  as.data.frame()


#####


for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]

  level_sample_DDD[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$d_desas_prod

  desas_prod_DDD[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)


  print(i)

  for (j in 1:horizonte){

    ### Criando variável x_t

    x_t <- sum(in_sample$d_desas_prod[(nrow(in_sample)-j+1):nrow(in_sample)])

    ### estimando o modelo 

    saved_models_AR1[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample , j))[[1]]

    saved_models_DDD[[i+1]] <- saved_models_AR1[[i+1]][2]

    ###

    ### criando o índice de "t" (defas_1) e o índice de "t + h - 12"

    defas_1 <- window_size
    defas_12 <- window_size + j - 12

    ###

    ### Obtendo a estimativa para a produção industrial desasonalizada no
    ### período "t + h"

   desas_prod_DDD[(nrow(in_sample) + j) , (i + 1)] <-
      desas_prod_DDD[defas_1 , (i+1)] + #produção desasonalizada no período t
      saved_models_DDD[[i+1]][[1]]*x_t

    ###

   ### Obtendo a estimativa para a dupla diferença 

   sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <-
      saved_models_DDD[[i + 1]][[1]]*sample_and_predicted[defas_1 , (i+1)]

   ###

   ### Recuperando o nível da variável

    level_sample_DDD[(nrow(in_sample) + j) , (i + 1)] <-
      level_sample_DDD[defas_12 , (1+i)] + # prod_ind no tempo "t+h-12"
      desas_prod_DDD[(nrow(in_sample) + j) , (i + 1)] # prod_ind desasonalizada
                                                        # estimada
   ###


  }
}


############
#  SMOOTHED DIFFERENCED DEVICE (SDD)
##########
## ---------------------------------------------------------------------------------------------------------------------------

rm(desas_prod)
sdd_ma <- replicate(1:(window_size + horizonte),
                                  n = repetitions+1) %>%
  as.data.frame()

defas_sdd_ma <- 0
desas_prod <-  replicate(1:(window_size + horizonte),
                     n = repetitions+1) %>%
  as.data.frame()

###

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  level_sample_SDD[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$sdd_desas_prod

  sdd_ma[(1:nrow(in_sample)) , (i+1)] <- in_sample$sdd_MA

  desas_prod[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)


  print(i)

  for (j in 1:horizonte){



    saved_models_AR1[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample , j))[[1]]

    saved_models_SDD[[i+1]] <- saved_models_AR1[[i+1]][2]


    low <- window_size

    sdd_ma[(nrow(in_sample) + j) , (i + 1)] <-
      (desas_prod[(nrow(in_sample) + j - 1), (i + 1)] +
         desas_prod[(nrow(in_sample) + j - 2), (i + 1)] +
         desas_prod[(nrow(in_sample) + j - 3), (i + 1)] +
         desas_prod[(nrow(in_sample) + j - 4), (i + 1)])/4

    ## 1.
    aux_factor <-
      sample_and_predicted[low , (i+1)]*saved_models_SDD[[i + 1]][[1]]

    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <- aux_factor

    ##

    ## 2.
    desas_prod[(nrow(in_sample) + j), (i + 1)] <-
      sdd_ma[(nrow(in_sample) + j) , (i + 1)] + # Média móvel em "t" de
                                                ## desas_prod

      sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] #sdd_desas_prod

    ##

    ##3.
    level_sample_SDD[(nrow(in_sample) + j) , (i + 1)] <-
      level_sample_SDD[(nrow(in_sample) + j - 12) , (i + 1)] +
      desas_prod[(nrow(in_sample) + j), (i + 1)]

  }
}


##############
#' ## Modelo AR13 + INTERCEPT CORRECTION (AR13 + IC)
#############


### AR 13 + IC ###
##################

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  level_sample_AR13_IC[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)

  print(i)

  for (j in 1:horizonte){

    saved_models_AR13_IC[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample , (j):(j+12)))[[1]]


      low <- window_size - j  #índice para a observação "t-j"
      up <- window_size  - j - 12 #índice para a obserção de defasagem 13

      aux_vec_lags <- as.matrix(rep(1 , times = 14), nrow = 1 , ncol = 14)

      aux_vec_lags[2:14] <- sample_and_predicted[low:up , (1+i)]

      aux_vec_lags <- t(aux_vec_lags)

      aux_factor <- aux_vec_lags %*%
        saved_models_AR13_IC[[i + 1]]


      resid_estimated <- sample_and_predicted[window_size , (1+i)] - # observação
                                                                      ## em "t"
        aux_factor[[1]] # estimativa para t da produção desasonalizada

      level_sample_AR13_IC[(nrow(in_sample) + j) , (i + 1)] <-
        level_sample_AR13[(nrow(in_sample) + j) , (i + 1)] + # Estimativa usando
                                                              ## modelo AR13

        resid_estimated # resíduo estimado do tempo "t"

  }



}


#'
#'
#' ## cOMBINATION DDD + SDD
#'
## ---------------------------------------------------------------------------------------------------------------------------

level_sample_combination <- (level_sample_DDD + level_sample_SDD) * 1/2


#'
#'
#' ## ROBUST MODELS COMBINATION 
#'
## ---------------------------------------------------------------------------------------------------------------------------

level_sample_combination_robust <- (level_sample_DDD + level_sample_SDD +
                                      level_sample_AR13_IC) * 1/3


#'
#'
#' ## NON-ROBUST MODELS COMBINATION
#'
## ---------------------------------------------------------------------------------------------------------------------------

level_sample_combination_non_robust <- (level_sample_AR1 + level_sample_AR13) * 1/2


#'
#'
#' ## CLUSTER METHODS (DSA - PINTO AND CASTLE (2022))
#'
#' Aqui faço aplico a clusterização hierárquica utilizando a métrica DTW (date
#' time warping) utilizando tuple de 3 observações, agregadas sequencialmente.
#' Após isso, comparo se o cluster da última tuple é igual ao da anterior ou da
#' antepenúltima. Se forem iguais, o valor predito do nível será o do modelo *AR13*
#' e se forem diferentes, isso indica quebra estrutural e, portanto, o valor
#' predito será o do modelo *DDD*. As observações da tuple é a produção industrial
#' desasonalizada. Portanto, estou avaliando se houve quebra na produção industrial
#' desasonalizada.
#'
#'
## ---------------------------------------------------------------------------------------------------------------------------

struc_break_cluster <- c()

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  in_sample_cluster <- in_sample
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  level_sample_cluster[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  level_sample_cluster_combination[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  # Aqui vou elimando as primeiras observações até que o total de observações
  ## seja divisível por 3 (para garantir que consigo agregar as observações
  ### em tuples de 3 observações)

  while( length(in_sample[,1]) %% 3 == 1){

    in_sample_cluster <- in_sample[2:nrow(in_sample) , ]

  }

  ###

  # Aqui ordeno de forma decrescente minha amostra, para que a primeira
  ## observação na amostra na verdade seja a última observação da amostra
  ### original

  in_sample_cluster <- in_sample_cluster[order(in_sample$date , decreasing = TRUE),]

  ###

  # Organizando em tuples

  ClusterData <- rollapply(data = in_sample_cluster$desas_prod, 3 , by = 3, c)

  ###

  # Aplicando a clusterização com o número de clusters iguais a 3 (k=3).

  DtwClust <- tsclust(ClusterData, k = 3L,
                      type="hierarchical", distance="dtw", preproc = zscore)

  ###

  # Comparando o último cluster com o penúltimo e o antepenúltimo e atribuindo
  ## os valores da variável em nível

  if(DtwClust@cluster[1]==DtwClust@cluster[2]|DtwClust@cluster[1]==DtwClust@cluster[3]){

    level_sample_cluster[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_AR13[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    level_sample_cluster_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_combination_non_robust[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    struc_break_cluster[i+1] <- NA

  } else {

    level_sample_cluster[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_DDD[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    level_sample_cluster_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_combination[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    struc_break_cluster[i+1] <- as.character(data$date[(50+i+1)])

  }

  ###

  print(i)

}

#'
#' ### Quebras Identicadas via clusterização
#'


print(struc_break_cluster[!is.na(struc_break_cluster)])


## ---------------------------------------------------------------------------------------------------------------------------

DATA <- as.data.frame(data$date[(window_size+1):(nrow(data)-11)])


#'
#'
#'
#' # ITERATIVE METHOD
#'
#' ## Listas
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------

interative_saved_models_AR13 <- list()

interative_saved_models_AR1 <- list()

interative_saved_models_RW <- list()

interative_saved_models_DDD <- list()

interative_saved_models_DDD_AR <- list()

interative_saved_models_DDD_AR_LASSO <- list()

interative_saved_models_DDD_AR_ADALASSO <- list()

interative_saved_models_SDD <- list()

interative_saved_models_AR13_IC <- list()

sample_and_predicted <- replicate(1:(window_size + horizonte),
                                  n = repetitions+1) %>%
  as.data.frame()

sample_and_predicted <- as.data.frame(sample_and_predicted)

interative_level_sample_AR13 <- sample_and_predicted
interative_level_sample_AR1 <- sample_and_predicted
interative_level_sample_RW <- sample_and_predicted
interative_level_sample_DDD <-sample_and_predicted
interative_level_sample_SDD <-sample_and_predicted
interative_level_sample_AR13_IC <- sample_and_predicted
interative_level_sample_cluster <- sample_and_predicted
interative_level_sample_cluster_combination <- sample_and_predicted
interative_level_sample_cluster_rls <- sample_and_predicted
interative_level_sample_cluster_rls_combination <- sample_and_predicted
interative_level_sample_cluster_check <- sample_and_predicted
interative_level_sample_DDD_AR <-sample_and_predicted
interative_level_sample_DDD_AR_LASSO <-sample_and_predicted
interative_level_sample_DDD_AR_ADALASSO <-sample_and_predicted



#'
#' ## AR 13 Iterativo
#'
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------
for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  interative_level_sample_AR13[(1:nrow(in_sample)) , (i+1)] <-
    in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)

  interative_saved_models_AR13[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample , 1:13))[[1]]

  # A função ARIMA() retorna ao invés do intercepto, a média calculada. Estou
  ## Transformando novamente em intercepto


  ###

  print(i)

  for (j in 1:horizonte){

    ### A grande diferença é que aqui os índices dependem de j

    low <- window_size + j - 1
    up <- window_size + j - 13

    ###

    aux_vec_lags <- as.matrix(rep(1 , times = 14), nrow = 1 , ncol = 14)

    # Como os índices andam com a amostra, estou utilizando as predições
    ## anteriores como "observação"

    aux_vec_lags[2:14] <- sample_and_predicted[low:up , (1+i)]

    ###

    aux_vec_lags <- t(aux_vec_lags)

    aux_factor <- aux_vec_lags %*%
      interative_saved_models_AR13[[i + 1]]


    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <- aux_factor[[1]]

    interative_level_sample_AR13[(nrow(in_sample) + j) , (i + 1)] <-
      aux_factor[[1]] +
      interative_level_sample_AR13[(nrow(in_sample) + j - 12) , (i + 1)]



  }

}

#'
#'
#' ## AR 1
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------
for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  interative_level_sample_AR1[(1:nrow(in_sample)) , (i+1)] <-
    in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)

  interative_saved_models_AR1[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample))[[1]]

  # A função ARIMA() retorna ao invés do intercepto, a média calculada. Estou
  ## Transformando novamente em intercepto

  print(i)

  for (j in 1:horizonte){

    ### A grande diferença é que aqui os índices dependem de j

    low <- window_size + j - 1
    up <- window_size + j - 13

    ###

    aux_vec_lags <- as.matrix(rep(1 , times = 2), nrow = 1 , ncol = 2)

    # Como os índices andam com a amostra, estou utilizando as predições
    ## anteriores como "observação"

    aux_vec_lags[2] <- sample_and_predicted[low, (1+i)]

    ###

    aux_vec_lags <- t(aux_vec_lags)

    aux_factor <- aux_vec_lags %*%
      interative_saved_models_AR1[[i + 1]]


    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <- aux_factor[[1]]

    interative_level_sample_AR1[(nrow(in_sample) + j) , (i + 1)] <-
      aux_factor[[1]] +
      interative_level_sample_AR1[(nrow(in_sample) + j - 12) , (i + 1)]



  }

}

#'
#' ## DDD AR 13 (I)
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------

### Gerando objetos

desas_prod <-  replicate(1:(window_size + horizonte),
                         n = repetitions+1) %>%
  as.data.frame()

###


for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  interative_level_sample_DDD_AR[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$d_desas_prod

  desas_prod[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$d_desas_prod)


  interative_saved_models_DDD_AR[[i+1]] <- interative_saved_models_AR13[[i+1]][2:14]


  print(i)

  for (j in 1:horizonte){

    low <- window_size + j - 1
    up <- window_size + j - 13

    defas_12 <- window_size + j - 12

    aux_vec_lags <- as.matrix(rep(1 , times = 13), nrow = 1 , ncol = 13)

    # Como os índices andam com a amostra, estou utilizando as predições
    ## anteriores como "observação"

    aux_vec_lags[1:13] <- sample_and_predicted[low:up , (1+i)]

    ###

    aux_vec_lags <- t(aux_vec_lags)

    aux_factor <- aux_vec_lags %*%
      interative_saved_models_DDD_AR[[i + 1]]

    ## 1.
    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <-
      aux_factor

    ##

    ## 2.
    desas_prod[(nrow(in_sample) + j) , (i + 1)] <-
      sample_and_predicted[(nrow(in_sample) + j) , (i + 1)]+
      desas_prod[low , (1+i)]

    ##

    ##3.
    interative_level_sample_DDD_AR[(nrow(in_sample) + j) , (i + 1)] <-
      desas_prod[(nrow(in_sample) + j) , (i + 1)] +
      interative_level_sample_DDD_AR[defas_12 , (1+i)]

    ##

  }
}

#'
#' ## ITERATIVE DDD AR 1

### Gerando objetos

desas_prod <-  replicate(1:(window_size + horizonte),
                         n = repetitions+1) %>%
  as.data.frame()

###


for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  interative_level_sample_DDD[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$d_desas_prod

  desas_prod[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- xts(x = in_sample$d_desas_prod,
                       order.by = as.Date(in_sample$date))


  interative_saved_models_DDD[[i+1]] <- interative_saved_models_AR1[[i+1]][2]


  print(i)

  for (j in 1:horizonte){

    defas_1 <- window_size + j - 1

    defas_2 <- window_size + j - 2

    defas_12 <- window_size + j - 12

    defas_13 <-  window_size + j - 13

    defas_14 <-  window_size+ j - 14

    ## 1.
    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <-
      interative_saved_models_DDD[[i + 1]][[1]]*
      sample_and_predicted[defas_1 , (i+1)]

    ##

    ## 2.
    desas_prod[(nrow(in_sample) + j) , (i + 1)] <-
      sample_and_predicted[(nrow(in_sample) + j) , (i + 1)]+
      desas_prod[defas_1 , (1+i)]

    ##

    ##3.
    interative_level_sample_DDD[(nrow(in_sample) + j) , (i + 1)] <-
      desas_prod[(nrow(in_sample) + j) , (i + 1)] +
      interative_level_sample_DDD[defas_12 , (1+i)]

    ##

  }
}



#'
#' ## ITERATIVE SDD

## Limpando e gerando objetos

rm(desas_prod)
sdd_ma <- replicate(1:(window_size + horizonte),
                                  n = repetitions+1) %>%
  as.data.frame()

defas_sdd_ma <- 0
desas_prod <-  replicate(1:(window_size + horizonte),
                     n = repetitions+1) %>%
  as.data.frame()

###

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  interative_level_sample_SDD[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$sdd_desas_prod

  sdd_ma[(1:nrow(in_sample)) , (i+1)] <- in_sample$sdd_MA

  desas_prod[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- xts(x = in_sample$sdd_desas_prod,
                       order.by = as.Date(in_sample$date))


  interative_saved_models_SDD[[i+1]] <- interative_saved_models_AR1[[i+1]][2]


  print(i)

  for (j in 1:horizonte){

    low <- window_size + j - 1

    up <- window_size + j - 12

    sdd_ma[(nrow(in_sample) + j) , (i + 1)] <-
      (desas_prod[(nrow(in_sample) + j - 1), (i + 1)] +
         desas_prod[(nrow(in_sample) + j - 2), (i + 1)] +
         desas_prod[(nrow(in_sample) + j - 3), (i + 1)] +
         desas_prod[(nrow(in_sample) + j - 4), (i + 1)])/4

    ## 1.
    aux_factor <-
      sample_and_predicted[low , (i+1)]*interative_saved_models_SDD[[i + 1]][[1]]

    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <- aux_factor

    ##

    ## 2.
    desas_prod[(nrow(in_sample) + j), (i + 1)] <-
      sdd_ma[(nrow(in_sample) + j) , (i + 1)] + # Média móvel em "t" de
                                                ## desas_prod

      sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] #sdd_desas_prod

    ##

    ##3.
    interative_level_sample_SDD[(nrow(in_sample) + j) , (i + 1)] <-
      interative_level_sample_SDD[(nrow(in_sample) + j - 12) , (i + 1)] +
      desas_prod[(nrow(in_sample) + j), (i + 1)]

  }
}


#'
#'
#' ## AR 13 + IC Iterativo
#'
## ---- results = FALSE-------------------------------------------------------------------------------------------------------

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  interative_level_sample_AR13_IC[(1:nrow(in_sample)) , (i+1)] <-
    in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  xts_in_sample <- ts(in_sample$desas_prod)

  interative_saved_models_AR13_IC[[i+1]] <- dynlm(xts_in_sample ~ L(xts_in_sample , 1:13))[[1]]


      ## Aqui estou criando o resíduo estimado no meu forecast origin, ou seja,
      ## denotando como o horizonte projetado como T + h com h = 1,..., H , estou
      ## estimando o resíduo do meu modelo no tempo "T".

  low <- window_size - 1
  up <- window_size  - 13

  aux_vec_lags <- as.matrix(rep(1 , times = 14), nrow = 1 , ncol = 14)

  aux_vec_lags[2:14] <- sample_and_predicted[low:up , (1+i)]

  aux_vec_lags <- t(aux_vec_lags)

  aux_factor <- aux_vec_lags %*%
    interative_saved_models_AR13_IC[[i + 1]]

  resid_estimated <- sample_and_predicted[window_size , (1+i)] - aux_factor[[1]]

  print(i)

  for (j in 1:horizonte){


    interative_level_sample_AR13_IC[(nrow(in_sample) + j) , (i + 1)] <-
        interative_level_sample_AR13[
          (nrow(in_sample) + j) , (i + 1)] + # Estimativa usando
                                                              ## modelo AR13

        resid_estimated # resíduo estimado do tempo "t"


  }

}



#'
#' ## AR (13) with LASSO
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  level_sample_lasso_AR13[(1:nrow(in_sample)) , (i+1)] <-
    in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod


  ## choosing the best model

  xts_in_sample <- matrix_desas_prod[(1 + i):(window_size + i), ]

  xts_in_sample <- xts_in_sample[complete.cases(xts_in_sample) , ]

  y <- xts_in_sample$desas_prod
  x <- data.matrix(xts_in_sample[ , -1])

  cv_model <- cv.glmnet(x , y , alpha = 1)

  best_lambda <- cv_model$lambda.min

  coef(cv_model)

  best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

  saved_models_lasso_ar13[[i+1]] <- coef(best_model)


  print(i)

  for (j in 1:horizonte){

    ### A grande diferença é que aqui os índices dependem de j

    low <- window_size + j - 1
    up <- window_size + j - 13

    ###

    aux_vec_lags <- as.matrix(rep(1 , times = 13), nrow = 1 , ncol = 13)

    # Como os índices andam com a amostra, estou utilizando as predições
    ## anteriores como "observação"

    aux_vec_lags <- sample_and_predicted[low:up , (1+i)]

    ###

    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <-
      predict(best_model, s = best_lambda, newx = aux_vec_lags)

    level_sample_lasso_AR13[(nrow(in_sample) + j) , (i + 1)] <-
      sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] +
      level_sample_lasso_AR13[(nrow(in_sample) + j - 12) , (i + 1)]



  }

}

#'
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------
## AR (13) with ADALASSO


for(i in 0:repetitions){


  in_sample <- data[(1 + i):(window_size + i), ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  level_sample_adalasso_AR13[(1:nrow(in_sample)) , (i+1)] <-
    in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod


  ## choosing the best model

  xts_in_sample <- matrix_desas_prod[(1 + i):(window_size + i), ]

  xts_in_sample <- xts_in_sample[complete.cases(xts_in_sample) , ]

  y <- xts_in_sample$desas_prod
  x <- data.matrix(xts_in_sample[ , -1])

  ridge1_cv <- cv.glmnet(x , y ,
                       ## type.measure: loss to use for cross-validation.
                       type.measure = "mse",
                       ## K = 10 is the default.
                       nfold = 10,
                       ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                       alpha = 0)

  best_ridge_coef <- as.numeric(coef(ridge1_cv, s = ridge1_cv$lambda.min))[-1]

  alasso1_cv <- cv.glmnet(x, y,
                        ## type.measure: loss to use for cross-validation.
                        type.measure = "mse",
                        ## K = 10 is the default.
                        nfold = 10,
                        ## ‘alpha = 1’ is the lasso penalty, and ‘alpha = 0’ the ridge penalty.
                        alpha = 1,
                        ##
                        ## penalty.factor: Separate penalty factors can be applied to each
                        ##           coefficient. This is a number that multiplies ‘lambda’ to
                        ##           allow differential shrinkage. Can be 0 for some variables,
                        ##           which implies no shrinkage, and that variable is always
                        ##           included in the model. Default is 1 for all variables (and
                        ##           implicitly infinity for variables listed in ‘exclude’). Note:
                        ##           the penalty factors are internally rescaled to sum to nvars,
                        ##           and the lambda sequence will reflect this change.
                        penalty.factor = 1 / abs(best_ridge_coef),
                        ## prevalidated array is returned
                        keep = TRUE)

  best_lambda <- alasso1_cv$lambda.min

  coef(alasso1_cv, s = alasso1_cv$lambda.min)

  best_model <- glmnet(x, y, alpha = 1 , lambda = best_lambda ,
                       penalty.factor = 1 / abs(best_ridge_coef),
                        ## prevalidated array is returned
                        keep = TRUE)

  coef(best_model)

  saved_models_adalasso_ar13[[i+1]] <- best_model


  print(i)

  for (j in 1:horizonte){

    ### A grande diferença é que aqui os índices dependem de j

    low <- window_size + j - 1
    up <- window_size + j - 13

    ###

    aux_vec_lags <- as.matrix(rep(1 , times = 13), nrow = 1 , ncol = 13)

    # Como os índices andam com a amostra, estou utilizando as predições
    ## anteriores como "observação"

    aux_vec_lags <- sample_and_predicted[low:up , (1+i)]

    ###

    sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] <-
      predict(best_model, s = best_lambda, newx = aux_vec_lags)

    level_sample_adalasso_AR13[(nrow(in_sample) + j) , (i + 1)] <-
      sample_and_predicted[(nrow(in_sample) + j) , (i + 1)] +
      level_sample_adalasso_AR13[(nrow(in_sample) + j - 12) , (i + 1)]
  }

}


#'

#'
#'
#' ## ITERATIVE COMBINATION DDD+SDD
#'
## ---------------------------------------------------------------------------------------------------------------------------
interative_level_sample_combination <- (interative_level_sample_DDD + interative_level_sample_SDD) * 1/2

#'
#'
#' ## ROBUST COMBINATION
#'
## ---------------------------------------------------------------------------------------------------------------------------
interative_level_sample_combination_robust <- (interative_level_sample_DDD +
                                          interative_level_sample_SDD +
                                          interative_level_sample_AR13_IC) * 1/3

#'
#'
#' # Autometrics
#'
## ---- results ='hide'-------------------------------------------------------------------------------------------------------

sample <- as.data.frame(matrix(nrow = 12 , ncol = repetitions+1))

for (i in 1:horizonte){

  sample[i , 1:ncol(sample) ] <-
    data$prod_ind[(window_size-12+i):(window_size-12+i+repetitions)]

}

if (window_size == 60){

dados_oxmetrics_desas_prod <-
  readxl::read_xlsx("Data\\previsoes_autometrix.xlsx", col_names = TRUE) |>
  as.data.frame()

} else {

  if (window_size == 90){

    dados_oxmetrics_desas_prod <-
      readxl::read_xlsx("Data\\previsoes_autometrix_90.xlsx", col_names = TRUE) |>
      as.data.frame()

  }else{

    dados_oxmetrics_desas_prod <-
      readxl::read_xlsx("Data\\previsoes_autometrix_120.xlsx", col_names = TRUE) |>
      as.data.frame()

  }

}

dados_oxmetrics_desas_prod <- dados_oxmetrics_desas_prod[ , -1]

dados_oxmetrics <- data.frame(
  matrix(nrow = nrow(level_sample_adalasso_AR13) ,
         ncol = ncol(level_sample_adalasso_AR13)))

dados_oxmetrics[1:window_size , ] <- level_sample_adalasso_AR13[1:window_size, ]

dados_oxmetrics[(window_size+1):nrow(dados_oxmetrics) , ] <- dados_oxmetrics_desas_prod + sample





#'
#' ## Combination non robust
#'
## ---------------------------------------------------------------------------------------------------------------------------
interative_level_sample_combination_non_robust <- (interative_level_sample_AR1 +
                                          interative_level_sample_AR13 +
                                          level_sample_adalasso_AR13 +
                                            level_sample_lasso_AR13 +
                                            dados_oxmetrics) * 1/5

#'
#' ## Clustering (Pinto-Castle (2022))
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------
struc_break_cluster <- c()

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  in_sample_cluster <- in_sample
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  aux_variable <- 0

  interative_level_sample_cluster[(1:nrow(in_sample)) , (i+1)] <-
    in_sample$prod_ind

  interative_level_sample_cluster_combination[(1:nrow(in_sample)) , (i+1)] <-
    in_sample$prod_ind

  sample_and_predicted[(1:nrow(in_sample)) , (i+1)] <- in_sample$desas_prod

  while( length(in_sample_cluster[,1]) %% 3 == 1){

    in_sample_cluster <- in_sample_cluster[2:nrow(in_sample_cluster) , ]

  }

  in_sample_cluster <- in_sample_cluster[order(in_sample_cluster$date ,
                                               decreasing = TRUE),]


  ClusterData <- rollapply(data = in_sample_cluster$desas_prod, 3 , by = 3, c)

  DtwClust <- tsclust(ClusterData, k = 3L,
                      type="hierarchical", distance="dtw", preproc = zscore)


  if(DtwClust@cluster[1]==DtwClust@cluster[2]|
     DtwClust@cluster[1]==DtwClust@cluster[3]){

    interative_level_sample_cluster[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_AR13[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    interative_level_sample_cluster_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_combination_non_robust[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    struc_break_cluster[i+1] <- NA

  } else {

    interative_level_sample_cluster[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_DDD[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    interative_level_sample_cluster_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_combination[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    struc_break_cluster[i+1] <- as.character(data$date[(50+i)])

  }

  print(i)

}

#'
#' ## Clustering Recursive Least Squares (RLS)
#'
## ---- results='hide'--------------------------------------------------------------------------------------------------------

## Recursive least squares

data_recursive <- data

desas_prod_1 <- Lag(data$desas_prod)[-1]
desas_prod <- data$desas_prod[-1]
intercept <- c(rep(1 , times = length(desas_prod_1)))

matrix_x <- as.matrix(data.frame(desas_prod_1 , intercept))

recursive_ls <- RLS(desas_prod, matrix_x)

test <- recursive_ls$beta

test <- as.data.frame(test)

data$intercept_rls <- c(rep(NA , times = 31) , test$intercept)

struc_break_cluster_rls <- c()

for(i in 0:repetitions){

  in_sample <- data[(1 + i):(window_size + i), ]
  in_sample_cluster <- in_sample[!is.na(in_sample$intercept_rls) , ]
  out_sample <- data[(window_size + i + 1):(nrow(data)), ]

  level_sample_cluster_rls[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  interative_level_sample_cluster_rls[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  level_sample_cluster_rls_combination[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind

  interative_level_sample_cluster_rls_combination[(1:nrow(in_sample)) , (i+1)] <- in_sample$prod_ind



  # Aqui vou elimando as primeiras observações até que o total de observações
  ## seja divisível por 3 (para garantir que consigo agregar as observações
  ### em tuples de 3 observações)


  while( length(in_sample_cluster[,1]) %% 3 == 1){

    in_sample_cluster <- in_sample_cluster[2:nrow(in_sample_cluster) , ]

  }

  ###

  # Aqui ordeno de forma decrescente minha amostra, para que a primeira
  ## observação na amostra na verdade seja a última observação da amostra
  ### original

  in_sample_cluster <- in_sample_cluster[order(in_sample_cluster$date , decreasing = TRUE),]

  ###

  # Organizando em tuples

  ClusterData <- rollapply(data = in_sample_cluster$intercept_rls, 3 , by = 3, c)

  ###

  # Aplicando a clusterização com o número de clusters iguais a 3 (k=3).

  DtwClust <- tsclust(ClusterData, k = 3L,
                      type="hierarchical", distance="dtw", preproc = zscore)

  ###
  print("b")
  # Comparando o último cluster com o penúltimo e o antepenúltimo e atribuindo
  ## os valores da variável em nível

  if(DtwClust@cluster[1]==DtwClust@cluster[2]|DtwClust@cluster[1]==DtwClust@cluster[3]){

    level_sample_cluster_rls[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_AR13[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    interative_level_sample_cluster_rls[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_AR13[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    level_sample_cluster_rls_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_combination_non_robust[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    interative_level_sample_cluster_rls_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_combination_non_robust[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    struc_break_cluster_rls[i+1] <- NA

  } else {

    level_sample_cluster_rls[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_DDD[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    interative_level_sample_cluster_rls[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_DDD[
        (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    level_sample_cluster_rls_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      level_sample_combination[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    interative_level_sample_cluster_rls_combination[
      (nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)] <-
      interative_level_sample_combination[(nrow(in_sample) + 1):(nrow(in_sample) + horizonte), (i + 1)]

    struc_break_cluster_rls[i+1] <- as.character(data$date[(50+i+1)])

  }

  ###

  print(i)

}

#'
#' ### Structural breaks identified by clustering (only for window_size = 60)
#'
## ---- echo = FALSE----------------------------------------------------------------------------------------------------------

##

#structural_breaks <- struc_break_cluster[!is.na(struc_break_cluster)]

#structural_breaks_pc <- matrix(ncol= 5 , nrow =  (round(length(structural_breaks)/5)+1))

#testeee <- as.matrix(rollapply(data = structural_breaks, 5 , by = 5, c))

#structural_breaks_pc[1:12 , 1:5] <- testeee

#structural_breaks_pc[13, 1:2] <- structural_breaks[61:length(structural_breaks)]

#structural_breaks <- struc_break_cluster_rls[!is.na(struc_break_cluster_rls)]

#structural_breaks_RLS <- matrix(ncol= 5 , nrow =  12)

#testeee <- as.matrix(rollapply(data = structural_breaks, 5 , by = 5, c))

#structural_breaks_RLS[1:11 , 1:5] <- testeee

#structural_breaks_RLS[12 , 1] <- structural_breaks[56]

#xtable(structural_breaks_RLS)

