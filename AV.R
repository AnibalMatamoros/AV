library(tidyverse)
library(dplyr)
library(knitr)
library(bookdown)
library(ggplot2)
library(Epi)
AV<-read_csv("AV.csv")

D1<- AV %>% group_by(Grupo) %>%
  summarize(Mean.Pre1 = mean(Pre1), 
            Mean.Pos1 = mean(Pos1), 
            Mean.Pre2 = mean(Pre2),  
            Mean.Pos2 = mean(Pos2), 
            Mean.Pre3 = mean(Pre3), 
            Mean.Pos3 = mean(Pos3), 
            Mean.Pre4 = mean(Pre4), 
            Mean.Pos4 = mean(Pos4), 
            Mean.Pre5 = mean(Pre5), 
            Mean.Pos5 = mean(Pos5), 
            Mean.Pre6 = mean(Pre6), 
            Mean.Pos6 = mean(Pos6), 
            Mean.Pre7 = mean(Pre7), 
            Mean.Pos7 = mean(Pos7), 
            Mean.Pre8 = mean(Pre8), 
            Mean.Pos8 = mean(Pos8), 
            Mean.Pre9 = mean(Pre9), 
            Mean.Pos9 = mean(Pos9)) %>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "Media")

##esto aun no lo trabajo
D3<- D4%>% mutate(Media_post= Med)

D2<- AV %>% group_by(Grupo) %>%
  summarize(SD.Pre1 = sd(Pre1), 
            SD.Pos1 = sd(Pos1),  
            SD.Pre2 = sd(Pre2), 
            SD.Pos2 = sd(Pos2), 
            SD.Pre3 = sd(Pre3),
            SD.Pos3 = sd(Pos3), 
            SD.Pre4 = sd(Pre4), 
            SD.Pos4 = sd(Pos4), 
            SD.Pre5 = sd(Pre5), 
            SD.Pos5 = sd(Pos5), 
            SD.Pre6 = sd(Pre6), 
            SD.Pos6 = sd(Pos6), 
            SD.Pre7 = sd(Pre7), 
            SD.Pos7 = sd(Pos7), 
            SD.Pre8 = sd(Pre8), 
            SD.Pos8 = sd(Pos8), 
            SD.Pre9 = sd(Pre9), 
            SD.Pos9 = sd(Pos9))%>%
  pivot_longer(starts_with("SD"), names_to = "Regiones", values_to = "SD")




##Tablas de la presentación

D3<- AV %>% group_by(Grupo) %>%
  summarize (R1 = mean(Pre1), 
             R2 = mean(Pre2), 
             R3 = mean(Pre3), 
             R4 = mean(Pre4), 
             R5 = mean(Pre5), 
             R6 = mean(Pre6), 
             R7 = mean(Pre7), 
             R8 = mean(Pre8), 
             R9 = mean(Pre9)) %>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "Media")





D4<- AV %>% group_by(Grupo) %>%
  summarize(R1 = mean(Pos1), 
            R2 = mean(Pos2),  
            R3 = mean(Pos3), 
            R4 = mean(Pos4), 
            R5 = mean(Pos5), 
            R6 = mean(Pos6),
            R7 = mean(Pos7), 
            R8 = mean(Pos8), 
            R9 = mean(Pos9)) %>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "Med")




#### tablas interesantes para ejemplificar un antes y un después,
#tx A y B
D5<- AV %>% group_by(Grupo) %>%
  summarize(R1 = mean(Pos1), 
            R2 = mean(Pos2),  
            R3 = mean(Pos3), 
            R4 = mean(Pos4), 
            R5 = mean(Pos5), 
            R6 = mean(Pos6),
            R7 = mean(Pos7), 
            R8 = mean(Pos8), 
            R9 = mean(Pos9)) %>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "Med")%>%
  pivot_wider(names_from = Grupo, values_from=Med)



A2<- D3%>% 
  pivot_wider(names_from = Region, values_from=Media, values_fn=mean)

### Aqui hice las medias generales
A3<- A2%>% group_by(Grupo)%>%
  transmute(MediaGen_= (R1+R2+R3+R4+R5+R6+R7+R8+R9)/9)


# haciendo Join. 

A<- D4%>% 
  pivot_wider(names_from = Grupo, values_from=Med)


S3<- AV %>% group_by(Grupo) %>%
  summarize(R1 = sd(Pre1),  
            R2 = sd(Pre2), 
            R3 = sd(Pre3),
            R4 = sd(Pre4), 
            R5 = sd(Pre5),
            R6 = sd(Pre6),
            R7 = sd(Pre7), 
            R8 = sd(Pre8), 
            R9 = sd(Pre9))%>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "SDpre")
  

S4<- AV %>% group_by(Grupo) %>%
  summarize(R1 = sd(Pos1),
            R2 = sd(Pos2),
            R3 = sd(Pos3),
            R4 = sd(Pos4), 
            R5 = sd(Pos5), 
            R6 = sd(Pos6),  
            R7 = sd(Pos7), 
            R8 = sd(Pos8), 
            R9 = sd(Pos9))%>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "SDpos")

# creando una tabla join con todos los valores
F4<- full_join(D3, S3, D4)
J2<- full_join(D3,S3)
J3<- full_join(D4, S4)
j4<- full_join(J2, J3)


ggplot(AV, aes(x = Edad, y = Pre7)) +
  geom_point(aes(shape = Sexo , color = Grupo, size=Pre7  ), alpha=0.4,) + 
  theme_bw()

#este entra
ggplot(j4, aes(x = Grupo, y = Media)) +
  geom_point(aes(shape = Region , color = Grupo), alpha=1) + 
  theme_bw()

Ja<- J%>%  pivot_longer(starts_with("Me"), names_to = "An", values_to = "Me")
Jb<- Ja%>% mutate(An, labels(c "Ante", "Despues"))
 
### Creando tabla antes y despues
##ojo son las mismas tablas que d3 y d4, solo que  las hice asi
#para no perderme

## secuencia de grafico

A1<- AV %>% group_by(Grupo) %>%
  summarize (R1 = mean(Pre1), 
             R2 = mean(Pre2), 
             R3 = mean(Pre3), 
             R4 = mean(Pre4), 
             R5 = mean(Pre5), 
             R6 = mean(Pre6), 
             R7 = mean(Pre7), 
             R8 = mean(Pre8), 
             R9 = mean(Pre9)) %>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "1Antes")


A2<- AV %>% group_by(Grupo) %>%
  summarize(R1 = mean(Pos1), 
            R2 = mean(Pos2),  
            R3 = mean(Pos3), 
            R4 = mean(Pos4), 
            R5 = mean(Pos5), 
            R6 = mean(Pos6),
            R7 = mean(Pos7), 
            R8 = mean(Pos8), 
            R9 = mean(Pos9)) %>%
  pivot_longer(starts_with("R"), names_to = "Region", values_to = "1Depues")

A3<- full_join (A1, A2)%>% 
  pivot_longer(starts_with("1"), names_to = "Resultado", values_to = "Media")


ggplot(A3, aes(x= Resultado, y= Media, col=Region, group=Region)) +
  geom_point(aes(shape=Grupo), size=3)+
  geom_line(show.legend = F)+
  theme_classic()+
  ylab ("Grosor de la Macula")+
xlab("Resultado")

# usar este grafico

ggplot(A3, aes(x=Grupo, y=Media, fill=Resultado ))+
  geom_boxplot()+
  geom_jitter(aes(color = Resultado))+
  scale_fill_manual(values = c('#d8b365','#5ab4ac'))+
ylab ("Grosor de la Macula")+
  xlab("Grupo de tratamiento")



  

 # Vamos hacer un pivot para toda la tabla a ver si
#sale un grafico buen
#usaremos P para unir


C<- AV%>% 
  pivot_longer(starts_with("P"), names_to = "Regiones", values_to = "Grosor")

ggplot(C, aes(x=Regiones, y=Grosor, fill=Grupo ))+
  geom_boxplot()+
  coord_flip()+
  ylab ("Grosor de la Macula")+
  xlab("Grupo de tratamiento")

ggplot(C, aes(x= fct_relevel(Regiones, "Pre1" ,"Pos1","Pre2" ,"Pos2","Pre3" ,"Pos4", 
"Pre5","Pos5", "Pre6" ,"Pos6", "Pre7" ,"Pos7", "Pre8" ,"Pos8", "Pre9" ,"Pos9" ), y=Grosor, fill=Grupo ))+
  geom_boxplot()+
  ylab ("Grosor de la Macula")+
  xlab("Grupo de tratamiento")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#Vamos hacer el grafico solo para los pos, este es el grafico que quiero
C2<- AV%>% 
  pivot_longer(starts_with("Pos"), names_to = "Regiones", values_to = "Grosor")

ggplot(C2, aes(x=Regiones, y=Grosor, fill=Grupo ))+
  geom_boxplot()+
  ylab ("Grosor de la Macula")+
  xlab("Grupo de tratamiento")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

par(mfrow=c(2,2))

ggplot(AV, aes(x= fct_relevel("Pre1" ,"Pos1"), y=Grosor, fill=Grupo ))+
  geom_boxplot()+
  ylab ("Grosor de la Macula")+
  xlab("Grupo de tratamiento")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


####################Probleamas aqui#######
##################333##
###########

par(mfrow=c(2,1))


par(mfrow=c(1,2))

dev.off() # Desactivamos todas las ventanas gr�ficas o dispositivos
layout(matrix(c(1:4), nrow=2, byrow=FALSE))

layout.show(4)

ggplot(AV, aes(x = Grupo, y = Pre2)) + 
  geom_boxplot(aes(fill = Grupo)) + theme_bw()

ggplot(AV, aes(x = Grupo, y = Pre1)) + 
  geom_boxplot(aes(fill = Grupo)) + theme_bw()
