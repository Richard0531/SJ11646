

AUC_plot <- function(data, cancer){
  
  All_Panel = subset(data, data$PanelName == cancer)

  p = ggplot(All_Panel, aes(x =factor(Concentration), 
                             y=EndpointValue, group = Compound)) +
    facet_wrap(~CellLineName,ncol = 3)+
    theme(
      axis.title = element_text(family = "Helvetica", size = (15)),
      axis.text = element_text(family = "Courier", size = (10),angle=60,vjust=.8, hjust=1),
      plot.title = element_text(family = "Courier", size = (15), hjust=1)
    )+
    ###geom_line(size=1.2,aes(color =Compound))+
    xlab("Concentration") + ylab("EndpointValue")+
    ###geom_smooth(method = "gam", formula = y ~ poly(x, 2),aes(color = Compound),se = TRUE)+
    geom_smooth(method = lm,aes(color = Compound),se = TRUE)+
    ggtitle(All_Panel$PanelName)
    ###+stat_regline_equation(label.x = 3, label.y = 7)
  
  print(p)
  
    data_summary <- function(data, varname, groupnames){
      require(plyr)
      summary_func <- function(x, col){
        c(mean = mean(x[[col]], na.rm=TRUE),
          sd = sd(x[[col]], na.rm=TRUE))
      }
      data_sum<-ddply(data, groupnames, .fun=summary_func,
                      varname)
      data_sum <- rename(data_sum, c("mean" = varname))
      return(data_sum)
    }
    All_summ <- data_summary(All_Panel, varname="EndpointValue", 
                                   groupnames=c("CellLineName", "Compound","Concentration"))
    
    AUC = sapply(split(All_summ , list(All_summ$CellLineName,All_summ$Compound)),
                   function(All_summ) trapz(All_summ$Concentration,All_summ$EndpointValue))
    df = as.data.frame(AUC)
    df$Compound = sapply(strsplit(rownames(df), "\\."), "[", -1)
    df$CellLine = sapply(strsplit(rownames(df), "\\."), "[", 1)
    
    df_dasatinib = subset(df, df$Compound == "dasatinib")
    df_ponatinib = subset(df, df$Compound == "ponatinib")
    df_SJ11646 = subset(df, df$Compound == "SJ11646")
    df2 <- cbind(df_dasatinib$AUC, df_ponatinib$AUC, df_SJ11646$AUC)
    rownames(df2)= df_SJ11646$CellLine
    colnames(df2)= unique(df$Compound)
    df3 = as.data.frame(df2)
    
  print(df2)
  p2 = pheatmap(df2,                         
           display_numbers = TRUE,main = cancer)
 
  print(p2)
  slopes =  function(data,x,y) {diff(y)/diff(x)}
  slope = sapply(split(All_summ , list(All_summ$CellLineName,All_summ$Compound)),
                 function(All_summ) slopes(All_summ, All_summ$Concentration,All_summ$EndpointValue))
  slope = t(slope)
  df_slope = as.data.frame(slope)
  df_slope$Compound = sapply(strsplit(rownames(df_slope), "\\."), "[", -1)
  df_slope$CellLine = sapply(strsplit(rownames(df_slope), "\\."), "[", 1)
  df_slope_dasa = subset(df_slope, df_slope$Compound == "dasatinib")
  df_slope_pono = subset(df_slope, df_slope$Compound == "ponatinib")
  df_slope_SJ = subset(df_slope, df_slope$Compound == "SJ11646")
  df_slope2 <- cbind(df_slope_dasa[,c(1:4)],df_slope_pono[,c(1:4)],df_slope_SJ[,c(1:4)])
  rownames(df_slope2)= df_slope_dasa$CellLine
  colnames(df_slope2)= c("dasatinib_1","dasatinib_2","dasatinib_3","dasatinib_4",
                         "ponatinib_1","ponatinib_2","ponatinib_3","ponatinib_4",
                         "SJ11646_1","SJ11646_2","SJ11646_3","SJ11646_4")
  df_slope3 = as.data.frame(df_slope2)
  print(df_slope3)
  
  p3 = pheatmap(df_slope3,   cluster_rows = FALSE,
                cluster_cols = FALSE,                      
                display_numbers = TRUE,
                main = cancer)
  p3
  
  write.csv(df3,"AUC.csv")
  write.csv(df_slope3,"Slope.csv")
  
}


AUC_plot(All,"Leukemia")
AUC_plot(All,"Non-Small Cell Lung")
AUC_plot(All,"Colon")
AUC_plot(All,"Central Nervous System")
AUC_plot(All,"Melanoma")
AUC_plot(All,"Ovarian")
AUC_plot(All,"Renal")
AUC_plot(All,"Prostate")
AUC_plot(All,"Breast")








































