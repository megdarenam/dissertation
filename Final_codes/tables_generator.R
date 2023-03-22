library(dplyr)

setwd(paste0(rank , '\\ranking_tables_periods'))

aux_list_ranking <- list(MCS_STATS_P.1 , MCS_STATS_P.2  , MCS_STATS_P.3 ,
                          MCS_STATS_P.4 , MCS_STATS_P.5  , MCS_STATS_P.6 ,
                          MCS_STATS_P.7)


aux_list_names <- c('MCS_STATS_P.1' , 'MCS_STATS_P.2'  , 'MCS_STATS_P.3' ,
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


##########
## MSFE ###

aux_list_ranking <- list(
                         MSFE_P.1 , MSFE_P.2  , MSFE_P.3 ,
                         MSFE_P.4 , MSFE_P.5  , MSFE_P.6 ,
                         MSFE_P.7)


aux_list_names <- c(
                    'MSFE_P.1' , 'MSFE_P.2'  , 'MSFE_P.3' ,
                    'MSFE_P.4' , 'MSFE_P.5'  , 'MSFE_P.6' ,
                    'MSFE_P.7')

index <- 0



for (i in aux_list_ranking){

  index <- index + 1

  df_aux <- i

  name_ranking_matrix <- paste0(aux_list_names[index] , '_ranking')

  presence_na_1 <- df_aux[1 , is.na(df_aux[1, ])]


  STEP_AHEAD_1 <-
    paste(
      names(sort(unlist(df_aux[1 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[1 , ]) ,
           na.last = TRUE ) , sep = " - stat:")



  STEP_AHEAD_2 <-
    paste(
      names(sort(unlist(df_aux[2 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[2 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_3 <-
    paste(
      names(sort(unlist(df_aux[3 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[3 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_4 <-
    paste(
      names(sort(unlist(df_aux[4 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[4 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_5 <-
    paste(
      names(sort(unlist(df_aux[5 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[5 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_6 <-
    paste(
      names(sort(unlist(df_aux[6 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[6 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_7 <-
    paste(
      names(sort(unlist(df_aux[7 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[7 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_8 <-
    paste(
      names(sort(unlist(df_aux[8 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[8 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_9 <-
    paste(
      names(sort(unlist(df_aux[9 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[9 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_10 <-
    paste(
      names(sort(unlist(df_aux[10 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[10 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_11 <-
    paste(
      names(sort(unlist(df_aux[11 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[11 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


  STEP_AHEAD_12 <-
    paste(
      names(sort(unlist(df_aux[12 , ]) ,
                 na.last = TRUE ))  ,
      sort(unlist(df_aux[12 , ]) ,
           na.last = TRUE ) , sep = " - stat:")


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

rm(list=ls()[! ls() %in% c('MCS_STATS_P.1_ranking' , 'MCS_STATS_P.2_ranking'  , 'MCS_STATS_P.3_ranking' ,
                           'MCS_STATS_P.4_ranking' , 'MCS_STATS_P.5_ranking'  , 'MCS_STATS_P.6_ranking' ,
                           'MCS_STATS_P.7_ranking' ,
                           'MSFE_P.1_ranking' , 'MSFE_P.2_ranking'  , 'MSFE_P.3_ranking' ,
                           'MSFE_P.4_ranking' , 'MSFE_P.5_ranking'  , 'MSFE_P.6_ranking' ,
                           'MSFE_P.7_ranking' , 'rank')])

save.image(
  paste0('tables_periods_', rank , ".RData"))

xtable::xtable(MCS_STATS_P.1_ranking)
xtable::xtable(MCS_STATS_P.2_ranking)
xtable::xtable(MCS_STATS_P.3_ranking)
xtable::xtable(MCS_STATS_P.4_ranking)
xtable::xtable(MCS_STATS_P.5_ranking)
xtable::xtable(MCS_STATS_P.6_ranking)
xtable::xtable(MCS_STATS_P.7_ranking)



