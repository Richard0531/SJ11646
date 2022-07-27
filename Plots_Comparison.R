Comp = Comparison
### Leukemia
Comp_Leukemia = filter(Comp, Comp$PanelName == "Leukemia")

ggplot(data=Comp_Leukemia, aes(x=CellLineName, 
                                           y=logValue, group=Compound)) +
  geom_line(aes(linetype=Compound,color = Compound))+
  geom_point(aes(shape=Compound,color = Compound))

model <- aov(logValue ~ Compound, data = Comp_Leukemia)
summary(model)
aov()
 ggplot(data=Comp_Leukemia, aes(x=Compound, 
                               y=logValue, group=Compound)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))
  

### Non-Small Cell Lung
Comp_NSC = filter(Comp, Comp$PanelName == "Non-Small Cell Lung")

ggplot(data=Comp_NSC, aes(x=CellLineName, 
                               y=logValue, group=Compound)) +
  geom_line(aes(linetype=Compound,color = Compound))+
  geom_point(aes(shape=Compound,color = Compound))

model <- aov(logValue ~ Compound, data = Comp_NSC)
summary(model)

ggplot(data=Comp_NSC, aes(x=Compound,
                      y=logValue, group=Compound)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))



### Melanoma
Comp_Melanoma = filter(Comp, Comp$PanelName == "Melanoma")

ggplot(data=Comp_Melanoma, aes(x=CellLineName, 
                          y=logValue, group=Compound)) +
  geom_line(aes(linetype=Compound,color = Compound))+
  geom_point(aes(shape=Compound,color = Compound))

model <- aov(logValue ~ Compound, data = Comp_Melanoma)
summary(model)

ggplot(data=Comp_Melanoma, aes(x=Compound, 
                          y=logValue, group=Compound)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))









### Ovarian
Comp_Ovarian = filter(Comp, Comp$PanelName == "Ovarian")

ggplot(data=Comp_Ovarian, aes(x=CellLineName, 
                               y=logValue, group=Compound)) +
  geom_line(aes(linetype=Compound,color = Compound))+
  geom_point(aes(shape=Compound,color = Compound))

model <- aov(logValue ~ Compound, data = Comp_Ovarian)
summary(model)

ggplot(data=Comp_Ovarian, aes(x=Compound, 
                               y=logValue, group=Compound)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))

### Renal
Comp_Renal = filter(Comp, Comp$PanelName == "Renal")

ggplot(data=Comp_Renal, aes(x=CellLineName, 
                              y=logValue, group=Compound)) +
  geom_line(aes(linetype=Compound,color = Compound))+
  geom_point(aes(shape=Compound,color = Compound))

model <- aov(logValue ~ Compound, data = Comp_Renal)
summary(model)
linear_reg <- summary(lm(logValue~Compound, na.action=na.omit, data = Comp_Renal))

ggplot(data=Comp_Renal, aes(x=Compound, 
                              y=logValue, group=Compound)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))

Comp_Renal = filter(Comp, Comp$Compound != "ponatinib")
Comp_Ours_ponatinib = filter(Comp, Comp$Compound != "dasatanib")
Comp_Dasatinib_ponatinib = filter(Comp, Comp$Compound != "Ours")









### Compare

Comp_Ours_Dasatinib = filter(Comp, Comp$Compound != "ponatinib")
Comp_Ours_ponatinib = filter(Comp, Comp$Compound != "dasatanib")
Comp_Dasatinib_ponatinib = filter(Comp, Comp$Compound != "Ours")

model <- aov(logValue ~ Compound, data = Comp_Ours_Dasatinib)
summary(model)




###LinePlot
 p3 = ggplot(data=Comp, aes(x =factor(CellLineName), 
                              y=logValue, group = Compound)) +
  geom_line(aes(linetype=Compound,color =Compound))+
  geom_point(aes(shape=Compound,color = Compound)) +
  ###facet_grid(~PanelName)+
  theme(
    axis.title = element_text(family = "Helvetica", size = (15)),
    axis.text = element_text(family = "Courier", size = (10),angle=60,vjust=.8, hjust=1),
    ###panel.grid.major.x = element_line(color = "black",
                                    #size = 0.25,
                                    #linetype = 2)
  )
  ###+geom_smooth()

 p4 = ggplot(data=Comp, aes(x =reorder(CellLineName,logValue), 
                            y=logValue, group = Compound)) +
   geom_line(aes(linetype=Compound,color =Compound))+
   geom_point(aes(shape=Compound,color = Compound)) +
   ###facet_wrap(vars(PanelName))+
   ###facet_grid(PanelName~.,scales="free_y")+
   theme(
     axis.title = element_text(family = "Helvetica", size = (15)),
     axis.text = element_text(family = "Courier", size = (10),angle=60,vjust=.8, hjust=1),
     ###panel.grid.major.x = element_line(color = "black",
     #size = 0.25,
     #linetype = 2)
   )+
   xlab("CellLines") + ylab("logValue")
   
###BoxPlot
p5 = ggplot(data=Comp, aes(x=Compound, 
                              y=logValue, group=Compound)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  theme(
     axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
     axis.text = element_text(family = "Courier", size = (10))
   )
   ###geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3)

###HeatMap
 p1 = ggplot(data = Comp, aes(x = factor(Compound,c("Ours","dasatinib","ponatinib")), y = reorder(CellLineName,logValue))) +
   geom_tile(aes(fill = logValue)) +
   scale_fill_gradient(low="blue", high="white")+
   theme(axis.text.y = element_text(size = 5),
         axis.text = element_text(size = 1))+
   xlab("Compound") + ylab("CellLineName")+
   facet_grid(PanelName~., scales="free_y")
 
 
 p2 = ggplot(data = Comp, aes(x = factor(Compound,c("Ours","dasatinib","ponatinib")), y = reorder(CellLineName,logValue))) +
   geom_tile(aes(fill = logValue)) +
   scale_fill_gradient(low="blue", high="white")+
   theme(axis.text.y = element_text(size = 5),
         axis.text = element_text(size = 1))+
   xlab("Compound") + ylab("CellLineName")

###ANCOVA
model <- aov(logValue ~ Compound, data = Comp)
summary(model)






















ggplot(Comp, aes(x=PanelName, y=CellLineName, color=Compound,group = PanelName)) + 
  geom_point(size=6) 
 
 
 
 
 theme(
   plot.title = element_text(family = "Helvetica", face = "bold", size = (15)),
   legend.title = element_text(colour = "steelblue", face = "bold.italic", family = "Helvetica"),
   legend.text = element_text(face = "italic", colour = "steelblue4", family = "Helvetica"),
   axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
   axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10))
 )


