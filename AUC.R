
library(pracma)
library(ggRandomForests)
library(pROC)
library(PKNCA)
library(caTools)
library(pheatmap)

### NSCLC 

All_NSCLC = subset( All, All$PanelName==  "Non-Small Cell Lung")

####Example
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





####NSCLC_ Add Error Bar

All_NSCLC_summ <- data_summary(All_NSCLC, varname="EndpointValue", 
                    groupnames=c("CellLineName", "Compound","Concentration"))

dasa_summ <- data_summary(dasatinib, varname="EndpointValue", 
                               groupnames=c("CellLineName", "Compound","Concentration","PanelName"))
dasa_summ_Renal = subset(dasa_summ, dasa_summ$PanelName == "Renal")

ggplot(data=All_NSCLC_summ, aes(x =factor(Concentration), 
                      y=EndpointValue, group = CellLineName)) +
  ###geom_point(size=2.5,aes(shape=Compound,color = Compound)) +
  ###facet_wrap(~CellLineName,ncol = 3)+
  theme(
    axis.title = element_text(family = "Helvetica", size = (15)),
    axis.text = element_text(family = "Courier", size = (10),angle=60,vjust=.8, hjust=1),
    plot.title = element_text(family = "Courier", size = (15), hjust=1)
  )+
  geom_line(size=1.2,aes(color =CellLineName))+
  xlab("Concentration") + ylab("EndpointValue")
  ###ggtitle("NSCLC")+
  ###geom_smooth(method = "gam", formula = y ~ poly(x, 2),aes(color = Compound),se = FALSE)+
  ###geom_errorbar(aes(ymin=EndpointValue-sd, ymax=EndpointValue+sd), width=.2) 

#### AUC
AUC_1 = sapply(split(All_NSCLC_summ , list(All_NSCLC_summ$CellLineName,All_NSCLC_summ$Compound)),
               function(All_NSCLC_summ) trapz(All_NSCLC_summ$Concentration,All_NSCLC_summ$EndpointValue))
AUC_0 =sapply(split(All_NSCLC_summ , list(All_NSCLC_summ$CellLineName,All_NSCLC_summ$Compound)),
              function(All_NSCLC_summ) auc_test_fuction(All_NSCLC_summ$Concentration,All_NSCLC_summ$EndpointValue))



df = as.data.frame(AUC_1)
df$Compound = sapply(strsplit(rownames(df), "\\."), "[", -1)
df$CellLine = sapply(strsplit(rownames(df), "\\."), "[", 1)

df_dasa = subset(df, df$Compound == "dasatinib")
df_pono = subset(df, df$Compound == "ponatinib")
df_SJ = subset(df, df$Compound == "SJ11646")
df2 <- cbind(df_dasa$AUC_1, df_pono$AUC_1, df_SJ$AUC_1)
rownames(df2)= df_SJ$CellLine
colnames(df2)= unique(df$Compound)
df3 = as.data.frame(df2)





AUC_0 =sum(diff(dasatinib_K_562_median$Concentration) * 
             (head(dasatinib_K_562_median$EndpointValue,-1)+
                tail(dasatinib_K_562_median$EndpointValue,-1)))/2
test = auc(dasatinib_K_562_median$Concentration,dasatinib_K_562_median$EndpointValue)

df <- datasets::USArrests

All_summ <- data_summary(All, varname="EndpointValue", 
                               groupnames=c("CellLineName", "Compound","Concentration"))


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

slopes =  function(data,x,y) {diff(y)/diff(x)}

###Slope for linear regression

simple.fit = lm(EndpointValue~Concentration, data=All_NSCLC)
summary(simple.fit)

slope_One = sapply(split(All_NSCLC , list(All_NSCLC$CellLineName,All_NSCLC$Compound)),
               function(All_NSCLC) lm(EndpointValue~Concentration, data=All_NSCLC))
summary(slope_One)


split = split(All_NSCLC , list(All_NSCLC$CellLineName,All_NSCLC$Compound))



####Leukemia
All_Leukemia = subset( All, All$PanelName== "Leukemia" |All$PanelName==  "Non-Small Cell Lung")
All_NSCLC = subset( All, All$PanelName==  "Non-Small Cell Lung")
All_Colon = subset( All, All$PanelName== "Colon")
All_CNS = subset( All, All$PanelName==  "Central Nervous System")
All_Breast = subset( All, All$PanelName==  "Breast")


p1 =ggplot(data=All_Breast, aes(x =factor(Concentration), 
                           y=EndpointValue, group = Compound)) +
  ###geom_point(size=2.5,aes(shape=Compound,color = Compound)) +
  facet_wrap(~CellLineName,ncol = 3)+
  theme(
    axis.title = element_text(family = "Helvetica", size = (15)),
    axis.text = element_text(family = "Courier", size = (10),angle=60,vjust=.8, hjust=1),
    plot.title = element_text(family = "Courier", size = (15), hjust=1)
  )+
  ###geom_line(size=1.2,aes(linetype=Compound,color =Compound))+
  xlab("Concentration") + ylab("EndpointValue")+
  ggtitle("CNS")+
  geom_smooth(method = "gam", formula = y ~ poly(x, 2),aes(color = Compound),se = FALSE)
  ###geom_errorbar(width = 0.2)




All_MCF7 = subset(All, CellLineName == "MCF7" & Compound == "dasatinib")


AUC_1 = trapz(All_MCF7$Concentration,All_MCF7$EndpointValue)

AUC_0 =sum(diff(dasatinib_K_562_median$Concentration) * 
             (head(dasatinib_K_562_median$EndpointValue,-1)+
            tail(dasatinib_K_562_median$EndpointValue,-1)))/2

### 

All_Breast = subset( All, All$PanelName== "Breast")

ggplot(data=All, aes(x =factor(Concentration), 
                              y=EndpointValue, group = Compound)) +
  geom_line(aes(linetype=Compound,color =Compound))+
  geom_point(aes(shape=Compound,color = Compound)) +
  facet_grid(PanelName~CellLineName)+
  theme(
    axis.title = element_text(family = "Helvetica", size = (15)),
    axis.text = element_text(family = "Courier", size = (10),angle=60,vjust=.8, hjust=1),
    ###panel.grid.major.x = element_line(color = "black",
    #size = 0.25,
    #linetype = 2)
  )+
  xlab("Concentration") + ylab("EndpointValue")



AUC_test1 =  sapply(split(All_NSCLC_summ , list(All_NSCLC_summ$CellLineName,All_NSCLC_summ$Compound)),
                           function(All_NSCLC_summ) trapz(All_NSCLC_summ$Concentration,((All_NSCLC_summ$EndpointValue)+100)))

  
AUC_test3 = sapply(split(All_NSCLC_summ , list(All_NSCLC_summ$CellLineName,All_NSCLC_summ$Compound)),
                   function(All_NSCLC_summ) auc_test_fuction(All_NSCLC_summ$Concentration,(All_NSCLC_summ$EndpointValue)+100))

auc_test_fuction = function(x,y){sum((head(y,-1)+tail(y,-1)))/2}
test3 = split(All_NSCLC_summ , list(All_NSCLC_summ$CellLineName,All_NSCLC_summ$Compound))
              


123


















dasatinib_K_562 = filter(dasatinib, dasatinib$CellLineName== "K-562")
dasatinib_K_562_median = dasatinib_K_562 %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))


AUC_0 =sum(diff(dasatinib_K_562_median$Concentration) * (head(dasatinib_K_562_median$EndpointValue,-1)+
                                                tail(dasatinib_K_562_median$EndpointValue,-1)))/2

### K-562
dasatinib_K_562 = filter(dasatinib, dasatinib$CellLineName== "K-562")
dasatinib_K_562_median = dasatinib_K_562 %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

AUC_1 = trapz(dasatinib_K_562_median$Concentration,dasatinib_K_562_median$EndpointValue)

###HL-60
dasatinib_H60 = filter(dasatinib, dasatinib$CellLineName== "HL-60(TB)")
dasatinib_H60_median = dasatinib_H60 %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

AUC_2 = trapz(dasatinib_H60_median$Concentration,dasatinib_H60_median$EndpointValue)
AUC_3 =sum(diff(dasatinib_K_562_median$Concentration) * (head(dasatinib_K_562_median$EndpointValue,-1)+
                                                           tail(dasatinib_K_562_median$EndpointValue,-1)))/2

###SNB-75
dasatinib_SNB75 = filter(dasatinib, dasatinib$CellLineName== "SNB-75")
dasatinib_SNB75_median = dasatinib_SNB75 %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

AUC_4 = trapz(dasatinib_SNB75_median$Concentration,dasatinib_SNB75_median$EndpointValue)
AUC_5 = sum(diff(dasatinib_SNB75_median$Concentration) * 
              (head(dasatinib_SNB75_median$EndpointValue,-1)+
              tail(dasatinib_SNB75_median$EndpointValue,-1)))/2




data(mtcars)
car.pca <- prcomp(mtcars, scale = TRUE, center = TRUE)
car.pca$rotation[, 1] * (mtcars[1,] - summary(car.pca)$center) / summary(car.pca)$scale
