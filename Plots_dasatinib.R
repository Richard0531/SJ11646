

library(ggplot2)
library(dplyr)
library(gridExtra)
library(forcats)
library(plotly)
library(zoo)

dasatinib = DoseResponseData_dasatinib_


### Leukemia
dasatinib_Leukemia = filter(dasatinib, dasatinib$PanelName == "Leukemia")

dasatinib_Leukemia_median = dasatinib_Leukemia %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p1 = ggplot(data=dasatinib_Leukemia_median, aes(x=Concentration, 
                                    y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))


### NSCLC
dasatinib_NSCLC = filter(dasatinib, dasatinib$PanelName == "Non-Small Cell Lung")

dasatinib_NSCLC_median = dasatinib_NSCLC %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p2 =ggplot(data=dasatinib_NSCLC_median, aes(x=Concentration, 
                                           y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))


### Colon
dasatinib_Colon = filter(dasatinib, dasatinib$PanelName == "Colon")

dasatinib_Colon_median = dasatinib_Colon%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p3=ggplot(data=dasatinib_Colon_median, aes(x=Concentration, 
                                        y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))


### CNS
dasatinib_CNS = filter(dasatinib, dasatinib$PanelName == "CNS Cancer")

dasatinib_CNS_median = dasatinib_CNS%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p4 = ggplot(data=dasatinib_CNS_median, aes(x=Concentration, 
                                           y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))

### Melanoma
dasatinib_Melanoma = filter(dasatinib, dasatinib$PanelName == "Melanoma")

dasatinib_Melanoma_median = dasatinib_Melanoma%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p5 = ggplot(data=dasatinib_Melanoma_median, aes(x=Concentration, 
                                           y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))


### Ovarian
dasatinib_Ovarian = filter(dasatinib, dasatinib$PanelName == "Ovarian")

dasatinib_Ovarian_median = dasatinib_Ovarian%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p6 = ggplot(data=dasatinib_Ovarian_median, aes(x=Concentration, 
                                                y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))


### Renal
dasatinib_Renal = filter(dasatinib, dasatinib$PanelName == "Renal")

dasatinib_Renal_median = dasatinib_Renal%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p7 = ggplot(data=dasatinib_Renal_median, aes(x=Concentration, 
                                               y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))

### Prostate
dasatinib_Prostate = filter(dasatinib, dasatinib$PanelName == "Prostate")

dasatinib_Prostate_median = dasatinib_Prostate%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p8 = ggplot(data=dasatinib_Prostate_median, aes(x=Concentration, 
                                             y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))

### Breast
dasatinib_Breast = filter(dasatinib, dasatinib$PanelName == "Breast")

dasatinib_Breast_median = dasatinib_Breast%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

p9 = ggplot(data=dasatinib_Breast_median, aes(x=Concentration, 
                                                y=EndpointValue, group=CellLineName)) +
  geom_line(aes(linetype=CellLineName,color = CellLineName))+
  geom_point(aes(shape=CellLineName,color = CellLineName))
  




#ALL
p_All = grid.arrange(p1, p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3)










#All
dasatinib_median = dasatinib%>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))

ggplot(data=dasatinib_median, aes(x=Concentration, 
                                           y=EndpointValue, group=CellLineName)) +
  facet_wrap( ~PanelName,ncol = 3)+
  geom_line(aes(linetype=PanelName,color =  PanelName))+
  geom_point(aes(shape= PanelName,color = PanelName))
  
















