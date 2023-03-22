
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(dplyr)
library(forcats)

setwd(paste0( ws , "\\Boxplots_MSFE"))

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x)))
  # experiment with the multiplier to find the perfect position
}

aux_list <- list(
             MSFE_P.7, MSFE_P.6,
             MSFE_P.5, MSFE_P.4, MSFE_P.3
             , MSFE_P.2, MSFE_P.1, MSFE_H.12
             , MSFE_H.9
             , MSFE_H.6
             , MSFE_H.3
             , MSFE_H.1)

aux_list_2 <- list( MSFE_P.7, MSFE_P.6,
                   MSFE_P.5, MSFE_P.4, MSFE_P.3
                   , MSFE_P.2, MSFE_P.1)

names(aux_list) <- c(
                     'MSFE_P.7', 'MSFE_P.6',
                     'MSFE_P.5', 'MSFE_P.4 ', 'MSFE_P.3'
                     , 'MSFE_P.2', 'MSFE_P.1', 'MSFE_H.12'
                     , 'MSFE_H.9'
                     , 'MSFE_H.6'
                     , 'MSFE_H.3'
                     , 'MSFE_H.1')

plots_list <- aux_list

index_list <- 0

neworder <- c("AR 1" , "AR 13" , "Combination Non-Robust", "AR 1 (I)",
              "AR 13 (I)" , "Lasso AR 13 (I)", "Adalasso AR 13 (I)",
              "Autometrics AR 13 (I)", "Combination Non-Robust (I)",
              "RW", "DDD AR 1", "SDD AR 1", "AR + IC", "Combination Robust",
              "Combination SDD + DDD", "DDD AR 1 (I)" , "DDD AR 13 (I)" ,
              'SDD AR 1 (I)', 'AR + IC (I)','Combination Robust (I)',
              'Combination SDD + DDD (I)' , 'Cluster Pinto-Castle' ,
              "Cluster RLS" , 'Cluster Pinto-Castle Combination',
              'Cluster RLS Combination' , 'Cluster Pinto-Castle (I)',
              'Cluster RLS (I)' , 'Cluster Pinto-Castle Combination (I)',
              'Cluster RLS Combination (I)'
)

sumario <- data.frame(neworder , c(1:ncol(MCS_STATS_P.1)))

colnames(sumario) <- c('Models' , 'Index')
xtable::xtable(sumario)
median_values <- c()

for (i in aux_list) {

  df_aux <- i

  df_aux <- df_aux[ , neworder]

  median_values <- apply(df_aux , MARGIN = 2 , median)

  minimum_median <- min(median_values , na.rm = TRUE)

  colnames(df_aux) <- as.character(c(1:ncol(df_aux)))

  df_aux<- df_aux[ , colSums(is.na(df_aux)) < nrow(df_aux)]

  data_mod <- cbind(stack(df_aux))

  index_list <- index_list + 1

  aux_name <- 0

  if(index_list < 8 | (index_list > 12 & index_list < 20)){

    aux_name <- "Forecast Period"

  } else {aux_name <- "Forecast Period"}

  data_mod$Horizon <- rep(c(1:nrow(df_aux)) ,
                                 times = nrow(data_mod)/nrow(df_aux)) |>
    as.factor()


  data_mod$ind <- data_mod$ind |> as.factor()

  #data_mod$x_axis <- as.character(rep(c(1:ncol(df_aux)) , each = nrow(df_aux)))


  data_mod <- data_mod %>%
    dplyr::mutate(ind = forcats::fct_inorder(ind))

  name_object <- names(aux_list)[index_list]

  temp_plot <- data_mod %>%
    ggplot(aes(x=ind , y=values) ) +
    guides(fill = "none")+
    #geom_bar(aes(fill=as.factor(values)), position = "dodge", stat = "summary", fun = "median")+
    geom_boxplot(coef = 100) +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_point(aes(shape = Horizon, color = Horizon) ,size=2, alpha=0.6) +
    scale_color_manual(values = c("1" = "black", "2" = "blue" , "3" = "black", "4" = "blue",
                                  "5" = "black", "6" = "blue","7" = "black" , "8" = "blue",
                                    "9" = "black","10" = "blue" , "11" = "black" , "12" = "blue"))+
    scale_shape_manual(values=c("1"=15, "2" = 16 ,"3"= 17, "4" =18 ,
                                "5"= 7,"6" = 8, "7"=9 , "8" = 0, "9" = 1,
                                "10" = 2 ,"11"=11 , "12"=4))+
    theme_ipsum() +
    theme(
      plot.title = element_text(size=11),
      plot.margin=grid::unit(c(2,2,2,2), "mm"),
      axis.title.x = element_text(color="black", size=12, face="bold"),
      axis.title.y = element_text(color="black", size=12, face="bold")
    ) +
    labs(
       x ="Model", y = "Stat") +
    geom_hline(yintercept = minimum_median, color = 'green' , linetype = 'dashed')
    #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                 #position = position_dodge(width = 0.75) ,
                 #vjust=0.5 , size = 5)

  ggsave(width = 9 , temp_plot, file=paste0(ws,"_",name_object,".png"), bg="white" )

}




