library(ggplot2)
library(dplyr)
library(gridExtra)
library(forcats)
library(plotly)
library(zoo)
library(devtools)

SJ11646 = DoseResponseData_Ours
SJ11646[,5] = "SJ11646"
colnames(SJ11646)[5] = "Compound"

dasatinib = DoseResponseData_dasatinib_
dasatinib = filter(dasatinib, dasatinib$CellLineName != "SR")
dasatinib = filter(dasatinib, dasatinib$PanelName != "CNS Cancer")
dasatinib = dasatinib %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))
dasatinib[,5] = "dasatinib"
colnames(dasatinib)[5] = "Compound"



ponatinib = DoseResponseData_ponatinib_
ponatinib = filter(ponatinib, ponatinib$Concentration != -9)
ponatinib = filter(ponatinib, ponatinib$CellLineName != "SR")
ponatinib = filter(ponatinib, ponatinib$PanelName != "CNS Cancer")
ponatinib = ponatinib %>%
  group_by(PanelName,CellLineName,Concentration) %>%
  summarise_at(vars(EndpointValue), list(EndpointValue = median))
ponatinib[,5] = "ponatinib"
colnames(ponatinib)[5] = "Compound"

All = union(SJ11646, dasatinib)
All = union(All, ponatinib)


