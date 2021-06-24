library(tidyverse)
library(readxl)
library(splitstackshape)


#install.packages("wesanderson")
# Load
library(wesanderson)
library(paletteer)

#################### Flores! ###########################################################################

###########################################################################################################
#######################         Atributos personales y marco socio-económico
###########################################################################################################

# GENERO
genero <- tabla[,3]
genero_group <- genero %>% 
  group_by(genero) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),4)*100) 

estudia_group
ggplot(data = genero_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = genero))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=c("#A66FA6","#983352")) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="Genero", 
        caption="Atributos personales y marco socio-económico")



#EDADES

edades <- tabla[,2]
edades <- edades %>% mutate(grupo= case_when(edad>18 & edad<30 ~ '18 a 30',
                                             edad>30 & edad<40 ~ '30 a 40',
                                             edad>40 & edad<50 ~ '40 a 50',
                                             edad>50 & edad<60 ~ '50 a 60',
                                             edad>60 & edad<70 ~ '60 a 70',
                                             edad>70 ~ 'Más de 70'
                                             
                                             )) %>% 
                      group_by(grupo) %>% 
                      count() %>% 
                      ungroup() %>% 
                      arrange(desc(n)) %>% 
                      mutate(percentage = round(n/sum(n),4)*100) 
                     
edades <- edades[1:6,]
edades       

ggplot(data = edades, mapping = aes(x=grupo, y=n)) +
  #scale_fill_manual(name="",
                 #   values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = .5, fill="#A66FA6")+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    #axis.title.x = element_blank(),
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="Edades", 
        caption="Atributos personales y marco socio-económico")


############################################

# ESTUDIA
estudia <- tabla[,4]
estudia_group <- estudia %>% 
  group_by(estudiaActualmente) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),4)*100) 

estudia_group
ggplot(data = estudia_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = estudiaActualmente))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Estudia actualmente?", 
        caption="Atributos personales y marco socio-económico")



# ESTUDIOS Máximos


estudiosMáximos <- tabla[,5]
estudiosMaximos_group <- estudiosMáximos %>% 
                                group_by(estudiosMaximos) %>% 
                                count() %>% 
                                ungroup() %>% 
                                arrange(desc(n)) %>% 
                                mutate(percentage = round(n/sum(n),4)*100) 

estudiosMaximos_group


ggplot(data = estudiosMaximos_group, mapping = aes(x=estudiosMaximos, y=n)) +
  #scale_fill_manual(name="",
  #   values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = .5, fill="#A66FA6")+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    #axis.title.x = element_blank(),
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Cuál es su máximo nivel de estudios?", 
        caption="Atributos personales y marco socio-económico")





# SITUACION LABORAL
laboral <- tabla[,6]

laboral_group <- laboral %>% 
  group_by(situacionLaboral) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 

laboral_group

ggplot(data = laboral_group, mapping = aes(x=situacionLaboral, y=n)) +
  #scale_fill_manual(name="",
  #   values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = .5, fill="#A66FA6")+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.ticks.x=element_blank(),
    #axis.title.x = element_blank(),
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    axis.text.x = element_text(size=19),
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿En qué situación laboral se encuentra?", 
        caption="Atributos personales y marco socio-económico")




######## ORGANIZACCIONES SOCIALES



organizaciones_group <- organizacionesLong %>% 
  group_by(organizacionesSociales) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)



ggplot(data = organizaciones_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = organizacionesSociales))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="Tipos",
                    values = c("#AC1F25", "#FF9933","#272727", "#828788", "#96804B")) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="white",
            size=8) +
  xlim(.2,2.6)+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Conoce alguna de estas organizaciones sociales \n que se encuentra en el barrio?", 
        caption="Organizaciones sociales")


#########Sección III. Percepciones sobre problemas ambientales y efectos sobre la salud.




###########################################################################################################
#######################         Percepciones sobre problemas \n ambientales y efectos sobre la salud.
###########################################################################################################

aire <- aire[,2]
aire
airecalidad_group <-  aire %>% 
  group_by(aireCalidad) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  filter(n>1) %>% 
  mutate(percentage = round(n/sum(n),2)*100)

airecalidad_group

ggplot(data = airecalidad_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = aireCalidad))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="white",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Cómo evalua la calidad del aire en su barrio?", 
        caption="Percepciones sobre problemas \n ambientales y efectos sobre la salud.")





#Fuentes de Contaminación

airefuentes_group <- aireLong %>% 
  filter(fuentesContaminacion != "No sabe/no contesta") %>% 
  group_by(fuentesContaminacion) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)



ggplot(data = airefuentes_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = fuentesContaminacion))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=6)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="white",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Cuáles son las principales fuentes \n de contaminación del aire en el barrio?", 
        caption="Percepciones sobre problemas \n ambientales y efectos sobre la salud.")



######
aire <- tabla[,c(1,42:46)]

aireLong <- cSplit(aire, "fuentesContaminacion", ",", "long")
eventocalidad_group <- aire %>% 
  group_by(eventoAfectadoCalidadAire) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) %>% 
  mutate(eventoAfectadoCalidadAire= if_else(eventoAfectadoCalidadAire=="Si. ¿Cuál?","Si",eventoAfectadoCalidadAire))

eventocalidad_group

ggplot(data = eventocalidad_group, mapping = aes(x=eventoAfectadoCalidadAire, y=n, fill=eventoAfectadoCalidadAire)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Recuerda algún evento que haya afectado \n la calidad del aire en su barrio?", 
        caption="Percepciones sobre problemas \n ambientales y efectos sobre la salud.")




#Inundaciones
inundaciones_group <- inundaciones %>% 
  group_by(inundaciones) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)

inundaciones_group

ggplot(data = inundaciones_group, mapping = aes(x=inundaciones, y=n, fill=inundaciones)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=20, face="bold.italic",hjust = -0.25,# Ajuste horizontal
                              vjust = .5),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 18),
    legend.title = element_text(color="black", size = 18, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Conoce lugares del barrio donde se estanque el agua o se inunde cuando llueve?", 
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")





#Olor Zanjas

zanjas <- tabla[,c(1,49:50)]

#organizacionesLong <- cSplit(organizaciones, "organizacionesSociales", ",", "long")


zanjas_group <- zanjas %>% 
  group_by(olorZanjas) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)



ggplot(data = zanjas_group, mapping = aes(x=olorZanjas, y=n, fill=olorZanjas)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=20, face="bold.italic",hjust = -0.0,# Ajuste horizontal
                              vjust = .5),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 18),
    legend.title = element_text(color="black", size = 18, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="En los días de lluvia, ¿cambia el olor de las zanjas y/o pozos ciegos?", 
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")



### EMERGENCIAS


emergencias <- tabla[,c(1,51:52)]
emergencias %>% emergenciaInundacionesInstitucion
emergenciasLong <- cSplit(emergencias, "emergenciaInundacionesInstitucion", ",", "long")


emergencias_group <- emergenciasLong %>% 
  group_by(emergenciaInundacionesInstitucion) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)




ggplot(data = emergencias_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = emergenciaInundacionesInstitucion))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=6)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="white",
            size=8) +
  xlim(.2,2.6)+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = -0.15,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="Ante una emergencia vinculada a las inundaciones,\n ¿a qué institución recurre (o recurriría)?", 
        caption="Percepciones sobre problemas \n ambientales y efectos sobre la salud.")



# CONTAMINACION SONORA




sonora <- tabla[,c(1,53:54)]
sonoraLong <- cSplit(sonora, "contaminacionSonora", ",", "long")


sonora_group <- sonoraLong %>% 
  filter(contaminacionSonora != 'https://meet.google.com/') %>% 
  group_by(contaminacionSonora) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) %>% 
  filter(contaminacionSonora !='Describa')

sonora_group


ggplot(data = sonora_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = contaminacionSonora))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=6)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="white",
            size=8) +
  xlim(.2,2.6)+
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="En su opinión ¿cuáles son las principales fuentes de contaminación \n sonora en el barrio?", 
        caption="Percepciones sobre problemas \n ambientales y efectos sobre la salud.")



# MOTIVOS DE SALUD FRECUENTE


salud <- tabla[,c(38:39)]
salud_long <- cSplit(salud, "problemasSaludFrecuentes", ",", "long")


salud_group <- salud_long %>% 
  group_by(problemasSaludFrecuentes) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 





ggplot(data = salud_group, mapping = aes(x= reorder(problemasSaludFrecuentes, -n),
                                         y=n)) +
  
  geom_col(color = "black", width = .75)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    axis.text =  element_text(color = "black", size = 14,angle = 45,vjust = 0.5, hjust=1),
    
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=20, face="bold.italic",hjust = -0.0,# Ajuste horizontal
                              vjust = .5),
    plot.caption = element_text(size = 18)
    
    #legend.text = element_text(color = "black", size = 18),
    #legend.title = element_text(color="black", size = 18, face = "bold"),
    #legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Cuáles son los motivos sanitarios de consulta más frecuentes?", 
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")


#CONSULTA MEDICA ASISTE


medica <- tabla[,c(40:41)]
medica_long <- cSplit(medica, "consultaMedicaAsiste", ",", "long")


medica_group <- medica_long %>% 
  group_by(consultaMedicaAsiste) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 





ggplot(data = medica_group, mapping = aes(x= reorder(consultaMedicaAsiste, -n),
                                          y=n)) +
  
  geom_col(color = "black", width = .75)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    axis.text =  element_text(color = "black", size = 14,angle = 45,vjust = 0.5, hjust=1),
    
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=20, face="bold.italic",hjust = -0.0,# Ajuste horizontal
                              vjust = .5),
    plot.caption = element_text(size = 18)
    
    #legend.text = element_text(color = "black", size = 18),
    #legend.title = element_text(color="black", size = 18, face = "bold"),
    #legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="Ante la necesidad de una consulta médica asiste a… ", 
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")


#










###########################################################################################################
#######################    Percepción de efectos e impactos de las Estaciones de Transferencia
###########################################################################################################

sabe <- tabla[,78]
sabe

sabe_group <- sabe %>% 
  group_by(sabeQueEsET) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 
sabe_group

ggplot(data = sabe_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = sabeQueEsET))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Sabe lo que es una Estación de Transferencia?", 
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")




#################################################

conoce <- tabla[,13]
conoce <- conoce %>% mutate(conoceETCeamse= if_else(str_sub(conoceETCeamse,1,2)=='No','No',conoceETCeamse))

conoce_group <- conoce %>% 
  group_by(conoceETCeamse) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),4)*100) %>% 
  mutate(conoceETCeamse= if_else(conoceETCeamse=='Si (continua)','Si',conoceETCeamse))

conoce_group
ggplot(data = conoce_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = conoceETCeamse))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Conoce la Estación de Transferencia operada \n por el CEAMSE ubicada en su barrio?", 
        
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")


#######################################################################################


conoceAct <- tabla[,15]

conoceAct_group <- conoceAct %>% 
  group_by(actividadesETConoce) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),4)*100) %>% 
  mutate(actividadesETConoce= if_else(actividadesETConoce=='Sí. Describa','Si',actividadesETConoce))

ggplot(data = conoceAct_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = actividadesETConoce))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Conoce las actividades que se realizan \n en la Estación de Transferencia? ", 
        
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")


#######################################################################################


camionesmolestia<- tabla[,19]

camionesmolestia_group <- camionesmolestia %>% 
  group_by(camionesMolestiaET) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)

ggplot(data = camionesmolestia_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = camionesMolestiaET))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Le ocasionan molestias el tránsito de camiones \n de CEAMSE o recolectores?", 
        
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")

##############################################################################

olores<- tabla[,21]

olores_group <- olores %>% 
  group_by(oloresSintioET) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)

ggplot(data = olores_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = oloresSintioET))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
   # panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="Alguna vez, ¿sintió olores provenientes de \n la Estación de Transferencia?", 
        
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")


##############################################################################




##############################################################################

oloresmomento<- tabla[,22]

oloresmomentoLong <- cSplit(oloresmomento, "oloresMomentoET", ",", "long")

olores_group <- oloresmomentoLong %>% 
  group_by(oloresMomentoET) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)

ggplot(data = olores_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = oloresMomentoET))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿En algún momento del día en que percibe \n con más intensidad los olores?", 
        
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")


###########################################  
  
oloresIntensidad<- tabla[,23:24]

oloresIntensidadLong <- cSplit(oloresIntensidad, "oloresMolestiaET", ",", "long")

olores_group <- oloresIntensidadLong %>% 
  group_by(oloresMolestiaET) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)

ggplot(data = olores_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = oloresMolestiaET))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿Cómo le afectan los olores provenientes \n de la Estación de Transferencia? ", 
        
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")














#Higiene urbana y residuos

#residuosManejo",
#"residuosContenedores",
#"residuosContenedoresDificultad",
#"residuosContenedoresDificultad_DET",
#"residuosRecoleccionFrecuencia",


manejoResiduos <- tabla[,c(60)]
#medica_long <- cSplit(medica, "consultaMedicaAsiste", ",", "long")
view(manejoResiduos)

residuos_group <- manejoResiduos %>% 
  group_by(residuosManejo) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 


contenedores <- tabla[,c(61)]
#medica_long <- cSplit(medica, "consultaMedicaAsiste", ",", "long")

contenedores_group <- contenedores %>% 
  group_by(residuosContenedores) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) %>% 
  mutate(residuosContenedores= if_else(residuosContenedores=='No.  ¿A cuantas cuadras se encuentra el contenedor más cercano?:','No','Si'))

contenedores_group



ggplot(data = contenedores_group, mapping = aes(x=residuosContenedores, y=n, fill=residuosContenedores)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=2)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿hay contenedores de residuos?", 
        caption="Higiene urbana y residuos")


# PASA EL CAMION DE RESIDUOS



camionRecolector <- tabla[,c(64)]
#medica_long <- cSplit(medica, "consultaMedicaAsiste", ",", "long")

camionRecolector_group <- camionRecolector %>% 
  group_by(residuosRecoleccionFrecuencia) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 
camionRecolector_group



ggplot(data = camionRecolector_group, mapping = aes(x=residuosRecoleccionFrecuencia, y=n, fill=residuosRecoleccionFrecuencia)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=3)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    #axis.text =  element_text(color = "black", size = 14),
    axis.text =  element_blank(),
    
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Con qué frecuencia pasa el camión \n recolector de residuos?", 
        caption="Higiene urbana y residuos")


# Cooperativa de cartoneros



coopCartoneros <- tabla[,c(65)]
#medica_long <- cSplit(medica, "consultaMedicaAsiste", ",", "long")

coopCartoneros_group <- coopCartoneros %>% 
  group_by(cartonerosCooperativa) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) %>% 
  mutate(cartonerosCooperativa= if_else(cartonerosCooperativa=="No. (Pase a pregunta 46)",
                                                                "No",
                                                                "Si"))
coopCartoneros_group



ggplot(data = coopCartoneros_group, mapping = aes(x=cartonerosCooperativa, y=n, fill=cartonerosCooperativa)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=3)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Conoce alguna cooperativa de cartoneros que trabaje en el barrio?", 
        caption="Higiene urbana y residuos")


# TAREAS CARTONEROS
cartonerosTareas <- tabla[,c(66)]
cartonerosTareas
cartonerosTareas_long <- cSplit(cartonerosTareas, "cartonerosTarea", ",", "long")



cartonerosTareas_long_group <- cartonerosTareas_long %>% 
  group_by(cartonerosTarea) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 
cartonerosTareas_long_group



ggplot(data = cartonerosTareas_long_group, mapping = aes(x=cartonerosTarea, y=n, fill=cartonerosTarea)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=4)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
   # axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Qué tipo de tareas realizan?", 
        caption="Higiene urbana y residuos")


#Alguna vez, ¿realizó reclamos vinculado al manejo de residuos?
  
  
reclamoManejoResiduos <- tabla[,c(68)]

#cartonerosTareas_long <- cSplit(cartonerosTareas, "cartonerosTarea", ",", "long")
reclamoManejoResiduos


reclamoManejoResiduos_group <- reclamoManejoResiduos %>% 
  filter(residuosReclamos != 'Comentarios') %>% 
  group_by(residuosReclamos) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 
reclamoManejoResiduos_group



ggplot(data = reclamoManejoResiduos_group, mapping = aes(x=residuosReclamos, y=n, fill=residuosReclamos)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=4)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    # axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="Alguna vez, ¿realizó reclamos vinculado al manejo de residuos?", 
        caption="Higiene urbana y residuos")





#Usted, ¿realiza algún tipo de separación de residuos?
  
  

separacionResiduos <- tabla[,c(70)]

#cartonerosTareas_long <- cSplit(cartonerosTareas, "cartonerosTarea", ",", "long")



separacionResiduos_group <- separacionResiduos %>% 
  group_by(residuosSeparacion) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 



ggplot(data = separacionResiduos_group, mapping = aes(x=residuosSeparacion, y=n, fill=residuosSeparacion)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=4)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    # axis.title = element_blank(),
    # axis.text =  element_text(color = "black", size = 14),
    #  axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="Usted, ¿realiza algún tipo de separación de residuos?", 
        caption="Higiene urbana y residuos")







#¿Dónde deposita los residuos reciclables?



residuosDonde <- tabla[,69]

#cartonerosTareas_long <- cSplit(cartonerosTareas, "cartonerosTarea", ",", "long")

residuosDonde_group <- residuosDonde %>% 
  group_by(residuosReciclablesDonde) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 


residuosDonde_group
ggplot(data = residuosDonde_group, mapping = aes(x=residuosReciclablesDonde, y=n, fill=residuosReciclablesDonde)) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = .5)+
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = .75), col="black", size=8)+
  theme(
    #  panel.background = element_rect(fill = "white"),
    #panel.grid = element_blank(),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.x = element_blank(),
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(.75, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿Dónde deposita los residuos reciclables?", 
        caption="Higiene urbana y residuos")


########################################


ruidos<- tabla[,25:34]

ruidos
ruidos_group <- ruidos %>% 
  mutate(ruidosET= if_else(str_sub(ruidosET,1,1)=='N','No','Si')) %>% 
  group_by(ruidosET) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100)

ggplot(data = ruidos_group , #Dataset
       aes(x = 2,
           y = percentage,
           fill = ruidosET))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.25,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
    
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Tipos",
        title="¿escuchó ruidos provenientes de la Estación de Transferencia?", 
        
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")


ruidos<- tabla[,25:34]

ruidos2<- ruidos[,2]
ruidos2_long <- cSplit(ruidos2, "ruidosETMomento", ",", "long")


ruidos2_group <- ruidos2_long %>% 
  group_by(ruidosETMomento) %>%
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percentage = round(n/sum(n),2)*100) 






  
  ggplot(data = ruidos2_group , #Dataset
         aes(x = 2,
             y = percentage,
             fill = ruidosETMomento))+ #Valores
  geom_bar(stat = "identity")+
  coord_polar("y", start = 200) +
  scale_fill_manual(name="",
                    values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
  geom_col(color = "black", width = 1) +
  geom_text(aes(label = paste(percentage,"%", sep = "")),
            position = position_stack(vjust = 0.5),
            col="black",
            size=8) +
  xlim(.2,2.6)+
  theme(
    #panel.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                              vjust = 1),
    plot.caption = element_text(size = 18),
    
    legend.text = element_text(color = "black", size = 20),
    legend.title = element_text(color="black", size = 22, face = "bold"),
    legend.key.size = unit(1, 'cm')
  )+
  labs( x=NULL, 
        y=NULL,
        scale_fill_manual="Respuesta",
        title="¿En qué momento del día siente con más  \n intensidad los ruidos?", 
        caption="Percepción de efectos e impactos de las Estaciones de Transferencia")
  
  ruidosIntensidad <- ruidos[,3]
  
  ruidosIntensidad_group <- ruidosIntensidad %>% 
    group_by(ruidosMolestiaET) %>%
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(percentage = round(n/sum(n),2)*100) %>%
    arrange(desc(n))

  ruidosIntensidad_group
  
  
  
  
  ggplot(data = ruidosIntensidad_group, mapping = aes(x= reorder(ruidosMolestiaET,-n), y=n, fill=ruidosMolestiaET)) +
    scale_fill_manual(name="",
                      values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
    geom_col(color = "black", width = .5)+
    geom_text(aes(label = paste(percentage,"%", sep = "")),
              position = position_stack(vjust = .75), col="black", size=8)+
    theme(
      #  panel.background = element_rect(fill = "white"),
      #panel.grid = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.title.x = element_blank(),
      plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                                vjust = 1),
      plot.caption = element_text(size = 18),
      
      legend.text = element_text(color = "black", size = 20),
      legend.title = element_text(color="black", size = 22, face = "bold"),
      legend.key.size = unit(.75, 'cm')
      
    )+
       labs( x=NULL, 
          y=NULL,
          scale_fill_manual="Respuesta",
          title="¿Cómo le afectan estos ruidos?", 
          caption="Percepción de efectos e impactos de las Estaciones de Transferencia")




  
  
  
  ################### VIBRACIONES
  
  vibraciones
  vibraciones_group <- vibraciones %>% 
    group_by(vibraciones) %>%
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(percentage = round(n/sum(n),2)*100) %>% 
    mutate(vibraciones= if_else(vibraciones == 'No. (Pase a pregunta 44)',
                                'No',
                                'Si'))
  vibraciones_group
  
  
  ggplot(data = vibraciones_group, mapping = aes(x=vibraciones, y=n, fill=vibraciones)) +
    scale_fill_manual(name="",
                      values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
    geom_col(color = "black", width = .5)+
    geom_text(aes(label = paste(percentage,"%", sep = "")),
              position = position_stack(vjust = .75), col="black", size=8)+
    theme(
      #  panel.background = element_rect(fill = "white"),
      #panel.grid = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.title.x = element_blank(),
      plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.0,# Ajuste horizontal
                                vjust = 1),
      plot.caption = element_text(size = 18),
      
      legend.text = element_text(color = "black", size = 20),
      legend.title = element_text(color="black", size = 22, face = "bold"),
      legend.key.size = unit(.75, 'cm')
      
    )+
    labs( x=NULL, 
          y=NULL,
          scale_fill_manual="Respuesta",
          title="Alguna vez, ¿sintió vibraciones en su casa o lugar \n donde desarrolla su actividad?", 
          caption="Vibraciones")
  
  vibraciones
  vibracionesLong <- vibracionesLong %>% mutate(vibracionesProveniencia= as.character(vibracionesProveniencia))
  vibracionesProvenencia_group <- vibracionesLong %>% 
    group_by(vibracionesProveniencia) %>%
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(percentage = round(n/sum(n),2)*100) %>% 
    mutate(vibracionesProveniencia= if_else(vibracionesProveniencia==
                                              'De actividades dentro de la Estación de Transferencia',
                                            'De act. dentro de la ET',vibracionesProveniencia))
  vibracionesProvenencia_group
  
  
  
  ggplot(data = vibracionesProvenencia_group, mapping = aes(x=vibracionesProveniencia, y=n, fill=vibracionesProveniencia)) +
    scale_fill_manual(name="",
                      values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
    geom_col(color = "black", width = .5)+
    geom_text(aes(label = paste(percentage,"%", sep = "")),
              position = position_stack(vjust = .75), col="black", size=8)+
    theme(
      #  panel.background = element_rect(fill = "white"),
      #panel.grid = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.title.x = element_blank(),
      plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.0,# Ajuste horizontal
                                vjust = 1),
      plot.caption = element_text(size = 18),
      
      legend.text = element_text(color = "black", size = 20),
      legend.title = element_text(color="black", size = 22, face = "bold"),
      legend.key.size = unit(.75, 'cm')
      
    )+
    labs( x=NULL, 
          y=NULL,
          scale_fill_manual="Respuesta",
          title="¿De dónde provienen estas vibraciones?", 
          caption="Vibraciones")
  
  
  vibracionesIntensidad_group <- vibraciones %>% 
    group_by(vibracionesMolestia) %>%
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(percentage = round(n/sum(n),2)*100)
  
  vibracionesIntensidad_group
  
  ggplot(data = vibracionesIntensidad_group, mapping = aes(x=vibracionesMolestia, y=n, fill=vibracionesMolestia)) +
    scale_fill_manual(name="",
                      values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
    geom_col(color = "black", width = .5)+
    geom_text(aes(label = paste(percentage,"%", sep = "")),
              position = position_stack(vjust = .75), col="black", size=8)+
    theme(
      #  panel.background = element_rect(fill = "white"),
      #panel.grid = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.title.x = element_blank(),
      plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.0,# Ajuste horizontal
                                vjust = 1),
      plot.caption = element_text(size = 18),
      
      legend.text = element_text(color = "black", size = 20),
      legend.title = element_text(color="black", size = 22, face = "bold"),
      legend.key.size = unit(.75, 'cm')
      
    )+
    labs( x=NULL, 
          y=NULL,
          scale_fill_manual="Respuesta",
          title="¿Cómo le afectan las vibraciones?", 
          caption="Vibraciones")

  
  
  
  ######################### BENEFICIOS ET
  
  
  beneficios<- tabla[,35]
  beneficios <- cSplit(beneficios,"beneficiosET",",","long")

  beneficios_group <- beneficios %>% 
    group_by(beneficiosET) %>%
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(percentage = round(n/sum(n),2)*100) 
  
  
  
  ggplot(data = beneficios_group , #Dataset
         aes(x = 2,
             y = percentage,
             fill = beneficiosET))+ #Valores
    geom_bar(stat = "identity")+
    coord_polar("y", start = 200) +
    scale_fill_manual(name="",
                      values=paletteer_d("ggthemes::excel_Feathered", n=6)) +#Colores
    geom_col(color = "black", width = 1) +
    geom_text(aes(label = paste(percentage,"%", sep = "")),
              position = position_stack(vjust = 0.5),
              col="black",
              size=8) +
    xlim(.2,2.6)+
    theme(
      #panel.background = element_rect(fill = "white"),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      
      plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.5,# Ajuste horizontal
                                vjust = 1),
      plot.caption = element_text(size = 18),
      
      legend.text = element_text(color = "black", size = 20),
      legend.title = element_text(color="black", size = 22, face = "bold"),
      legend.key.size = unit(1, 'cm')
    )+
    labs( x=NULL, 
          y=NULL,
          scale_fill_manual="Respuesta",
          title="¿cuáles pueden ser los beneficios de la Estación de \nTransferencia en el barrio? ", 
          caption="")
  
  
  
  
  residuosDonde <- tabla[,69]
  
  #cartonerosTareas_long <- cSplit(cartonerosTareas, "cartonerosTarea", ",", "long")
  
  residuosDonde_group <- residuosDonde %>% 
    group_by(residuosReciclablesDonde) %>%
    count() %>% 
    ungroup() %>% 
    arrange(desc(n)) %>% 
    mutate(percentage = round(n/sum(n),2)*100) %>% 
    mutate(residuosReciclablesDonde=if_else(str_sub(residuosReciclablesDonde,1,1)=='O',
                                            "Otro",residuosReciclablesDonde))
  
  
  residuosDonde_group
  ggplot(data = residuosDonde_group, mapping = aes(x=residuosReciclablesDonde, y=n, fill=residuosReciclablesDonde)) +
    scale_fill_manual(name="",
                      values=paletteer_d("ggthemes::excel_Feathered", n=5)) +#Colores
    geom_col(color = "black", width = .5)+
    geom_text(aes(label = paste(percentage,"%", sep = "")),
              position = position_stack(vjust = .75), col="black", size=8)+
    theme(
      #  panel.background = element_rect(fill = "white"),
      #panel.grid = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      #axis.title.x = element_blank(),
      plot.title = element_text(color="Black", size=24, face="bold.italic",hjust = 0.0,# Ajuste horizontal
                                vjust = 1),
      plot.caption = element_text(size = 18),
      
      legend.text = element_text(color = "black", size = 20),
      legend.title = element_text(color="black", size = 22, face = "bold"),
      legend.key.size = unit(.75, 'cm')
      
    )+
    labs( x=NULL, 
          y=NULL,
          scale_fill_manual="Respuesta",
          title="¿Dónde deposita los residuos reciclables?", 
          caption="Higiene urbana y residuos")
  
