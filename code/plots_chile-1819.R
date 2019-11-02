
# Codigo para replicacion de Meritocracia y redistribucion en Chile: señales de la opinion pública.

# Todos los analisis fueron hechos en Rstudio. Para fuentes, ver README.

library(dplyr)
library(ggplot2)
library(sjPlot)
library(scales)
library(ggthemes)
library(Cairo)
library(grid)
library(gridExtra)
library(sjlabelled)
library(questionr)
rm(list=ls())
options(OutDec = ",")

# load data ---------------------------------------------------------------

load("data/issp2019CL.RData")
elsocl <- sjlabelled::read_stata(path = "data/elsocwide.dta")
issp16 <- sjlabelled::read_stata(path = "data/issp2016gov.dta")

# Figures -----------------------------------------------------------------

# --------------- Figure 1 ------------------------------- 

# Gini: disposable income, post taxes and transfers, new income definition since 2012
# Fuente: OECD Income Distribution database (IDD)
##############################################-

anio <- c(2009,2011,2013,2017)
gini <- c(48.0,47.1,46.5,46.0)

data <- data.frame(anio,gini)

g1=ggplot(data) +
  geom_line(aes(x=anio, y=gini, color = "red")) +
  geom_point(aes(x=anio, y=gini),size=3.0) +
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=10)) +
  scale_x_continuous(breaks = seq(2009,2017,by=2.0))+
  xlab("Año")+
  ylab("Percentaje") + 
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("Coeficiente de Gini") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0.5,size = 10,face = "bold"))+
  guides(colour=FALSE)

g1

# Gasto publico: OECD Social Expenditure Statistics
# Fuente: Gasto social como porcentage del PIB. OECD Social Expenditure Database (SOCX)
########################################################	 -  

gasto <- c(10.4,10.8,11.0,10.9) 
anio <- c(2010,2015,2016,2017)

oecd <- data.frame(anio,gasto)

g2=ggplot(oecd) +
  geom_line(aes(x=anio, y=gasto, color = "red")) +
  geom_point(aes(x=anio, y=gasto),size=3.0) +
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=10)) +
  scale_x_continuous(breaks = seq(2009,2017,by=2.0))+
  xlab("Año")+
  ylab("Porcentaje") + 
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("Gasto social") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0.5,size = 10,face = "bold"))+
  guides(colour=FALSE)
g2


# Graficos juntos
gasto<- grid.arrange(g1,g2,ncol=2,
                     bottom=textGrob(label = "Fuente: OCDE", 
                                     just = "left",hjust = -5.4,
                                     gp=gpar(fontsize=10,font=1)))

ggsave(gasto,filename = "images/Figura1.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")

# --------------- Figure 2 ------------------------------

# GDP per head, US $, constant prices, constant PPPs, reference year 2010. Expenditure approach. OECD Statistics. 

# Histogram: redistribution & GDP

redisGDP <- data.frame(stringsAsFactors=FALSE,
                       Pais = c("Australia", "Austria", "Belgica", "Canada", "Chile",
                                "Republica Checa", "Dinamarca", "Estonia",
                                "Finlandia", "Francia", "Alemania", "Grecia", "Hungria",
                                "Islandia", "Irlanda", "Israel", "Italia", "Japon",
                                "Corea del Sur", "Latvia", "Lituania", "Luxemburgo",
                                "Holanda", "Nueva Zelanda", "Noruega", "Polonia",
                                "Portugal", "Eslovaquia", "Eslovenia", "España",
                                "Suecia", "Suiza", "UK", "US", "Costa Rica", "Rusia"),
                       GiniPost = c(0.33, 0.284, 0.266, 0.307, 0.46, 0.253, 0.261, 0.314, 0.259,
                                    0.291, 0.294, 0.333, 0.282, 0.255, 0.309, 0.346,
                                    0.328, 0.339, 0.355, 0.346, 0.378, 0.304, 0.288,
                                    0.349, 0.262, 0.284, 0.331, 0.241, 0.244, 0.341, 0.282,
                                    0.296, 0.351, 0.391, 0.484, 0.331),
                       GiniPre = c(0.469, 0.501, 0.499, 0.431, 0.495, 0.448, 0.447, 0.456, 0.506,
                                   0.516, 0.505, 0.536, 0.485, 0.386, 0.543, 0.44,
                                   0.517, 0.504, 0.402, 0.475, 0.515, 0.482, 0.446,
                                   0.462, 0.428, 0.459, 0.53, 0.4, 0.452, 0.516, 0.435,
                                   0.386, 0.506, 0.507, 0.533, 0.466),
                       GDP = c(45929, 43247, 41284, 42485, 20855, 30542, 45458, 26655, 39172,
                               37169, 43102, 23703, 24861, 43726, 59694, 31832,
                               33646, 37036, 35035, 22839, 26721, 89528, 46612,
                               34498, 59551, 24928, 27302, 28981, 29064, 32687, 45341,
                               54453, 39142, 52583, 14807, 23094),
                       Year1 = c(2016, 2016, 2016, 2016, 2017, 2016, 2016, 2016, 2016, 2016,
                                 2016, 2016, 2016, 2015, 2016, 2016, 2016, 2015,
                                 2016, 2016, 2016, 2016, 2015, 2014, 2016, 2016, 2016,
                                 2016, 2016, 2016, 2016, 2015, 2016, 2016, 2016,
                                 2016),
                       group = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
)

redisGDP$group <- as.factor(redisGDP$group)
redisGDP$Redistribucion <- 100*(1 - (redisGDP$GiniPost/redisGDP$GiniPre))
redisGDP$Redistribucion2 <- redisGDP$Redistribucion

redist01<- ggplot(redisGDP,aes(x=GDP,y=Redistribucion)) +
  geom_point(aes(color=group,size=Redistribucion2), alpha=0.4) + scale_size(range = c(1, 10)) +
  scale_color_manual(values=c("gray50","red")) +
  geom_text(aes(y=Redistribucion+.8,label=Pais,color=group),size=4, vjust=0) +
  scale_x_continuous(breaks = seq(10000,90000,by=10000),limits=c(12000,93000)) + 
  geom_smooth(method="lm", se=FALSE, color="gray50") + 
  labs(y="Reducción del Gini debido a impuestos y transferencias",x="PIB per capita",
       caption = "Fuente: OCDE") +
  theme_bw() +
  theme(legend.position="none") +
  guides(colour=FALSE)
  
redist01

ggsave(redist01,filename = "images/Figura2.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")

# --------------- Figure 3 ---------------------------------- 

datam1 <- elsocl %>% select(tipo_atricion,starts_with("c18_09"),starts_with("ponderador01")) %>% filter(tipo_atricion==1) %>% select(-tipo_atricion) %>% na.omit()
datam2 <- elsocl %>% select(tipo_atricion,starts_with("c18_10"),starts_with("ponderador01")) %>% filter(tipo_atricion==1) %>% select(-tipo_atricion) %>% na.omit()
datag1 <- elsocl %>% select(tipo_atricion,starts_with("d05_04"),starts_with("ponderador01")) %>% filter(tipo_atricion==1) %>% select(-tipo_atricion) %>% na.omit()

datam1[datam1=="-888" ] <- NA
datam1[datam1=="-999" ] <- NA

datam2[datam2=="-888" ] <- NA
datam2[datam2=="-999" ] <- NA

datag1[datag1=="-888" ] <- NA
datag1[datag1=="-999" ] <- NA

datam1 <- rename(datam1,
                 meffort16=c18_09_w01,
                 meffort17=c18_09_w02,
                 meffort18=c18_09_w03)

datam1$meffort16 <- car::recode(var =datam1$meffort16,recodes = "c(1,2)=1;c(4,5)=2;3=3")
datam1$meffort17 <- car::recode(var =datam1$meffort17,recodes = "c(1,2)=1;c(4,5)=2;3=3")
datam1$meffort18 <- car::recode(var =datam1$meffort18,recodes = "c(1,2)=1;c(4,5)=2;3=3")

datam2 <- rename(datam2,
                 mtalent16=c18_10_w01,
                 mtalent17=c18_10_w02,
                 mtalent18=c18_10_w03)

datam2$mtalent16 <- car::recode(var =datam2$mtalent16,recodes = "c(1,2)=1;c(4,5)=2;3=3")
datam2$mtalent17 <- car::recode(var =datam2$mtalent17,recodes = "c(1,2)=1;c(4,5)=2;3=3")
datam2$mtalent18 <- car::recode(var =datam2$mtalent18,recodes = "c(1,2)=1;c(4,5)=2;3=3")

datag1 <- rename(datag1,
                 mtrdur16=d05_04_w01,
                 mtrdur17=d05_04_w02,
                 mtrdur18=d05_04_w03)

datag1$mtrdur16 <- car::recode(var =datag1$mtrdur16,recodes = "c(1,2)=1;c(4,5)=2;3=3")
datag1$mtrdur17 <- car::recode(var =datag1$mtrdur17,recodes = "c(1,2)=1;c(4,5)=2;3=3")
datag1$mtrdur18 <- car::recode(var =datag1$mtrdur18,recodes = "c(1,2)=1;c(4,5)=2;3=3")

#En Chile las personas son recompensadas por sus esfuerzos
a1 <- prop.table(wtd.table(datam1$meffort16==2,weights = datam1$ponderador01_w01))# % De Acuerdo
a2 <- prop.table(wtd.table(datam1$meffort17==2,weights = datam1$ponderador01_w02))# % De Acuerdo
a3 <- prop.table(wtd.table(datam1$meffort18==2,weights = datam1$ponderador01_w03))# % De Acuerdo
ef1 <- c(a1[[2]],a2[[2]],a3[[2]])

#En Chile las personas son recompensadas por su inteligencia y habilidades
b1 <- prop.table(wtd.table(datam2$mtalent16==2,weights = datam2$ponderador01_w01))# % De Acuerdo
b2 <- prop.table(wtd.table(datam2$mtalent17==2,weights = datam2$ponderador01_w02))# % De Acuerdo
b3 <- prop.table(wtd.table(datam2$mtalent18==2,weights = datam2$ponderador01_w03))# % De Acuerdo
tal1 <- c(b1[[2]],b2[[2]],b3[[2]])

#En Chile ¿cuán importante es para sugir en la vida...?
c1 <- prop.table(wtd.table(datag1$mtrdur16==2,weights = datag1$ponderador01_w01))# % Es importante
c2 <- prop.table(wtd.table(datag1$mtrdur17==2,weights = datag1$ponderador01_w02))# % Es importante
c3 <- prop.table(wtd.table(datag1$mtrdur18==2,weights = datag1$ponderador01_w03))# % Es importante
tdur1 <- c(c1[[2]],c2[[2]],c3[[2]])

merit<- rbind(
  data.frame(merit=ef1, var="esfuerzo",year=c(2016,2017,2018)),
  data.frame(merit=tdur1,var="tduro"  ,year=c(2016,2017,2018)))

pef1 <- ggplot(data = data.frame(merit=ef1, var="esfuerzo",year=c(2016,2017,2018)),
               mapping = aes(x = factor(year),y = merit, label=paste(round(merit*100,2),"%"))) +
  geom_bar(stat = "identity") + 
  geom_label()+
  labs(title = "En Chile las personas son recompensadas por sus esfuerzos.")+
  xlab(label = NULL)+
  scale_y_continuous(limits=c(0,1),name=NULL,labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0,size = 11,face = "italic"))
pef1

ptdu1 <- ggplot(data = data.frame(merit=tdur1,var="tduro"  ,year=c(2016,2017,2018)),
                mapping = aes(x = factor(year),y = merit, label=paste(round(merit*100,2),"%"))) +
  geom_bar(stat = "identity") +
  geom_label() +
  labs(title = "El Trabajo duro es importante para surgir en la vida.")+
  ylab(label = "Nivel de acuerdo")+
  xlab(label = NULL)+
  scale_y_continuous(limits=c(0,1),labels = scales::percent_format(accuracy = 1))+
  theme_bw()+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0,size = 11,face = "italic"))
ptdu1

barplot1<- grid.arrange(ptdu1,pef1, nrow = 1,
                        bottom=textGrob(label = "Estudio Social Longitudinal de Chile (n=2094). Análisis incluyen ponderadores.", 
                                        just = "left",hjust = -0.39,
                                        gp=gpar(fontsize=9,font=1)))

ggsave(barplot1,filename = "images/Figura3.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")

# --------------- Figura 4 ---------------------------------- 

# Preferencias redistributivas: evolucion en el tiempo
##########################################################-

# World value survey ------------------------------------------------------#
#                                (1)                (10)
# Incomes should be made more equal 2	3	4	5	6	7	8	9 We need larger income differences as incentives

ine <- c(26,30,50,39,60) # 1 + 2 + 3 de la escala en %
anio <- c(1990,1996,2000,2006,2012)
aniot <- c("1989-1993","1994-1998","1999-2004","2005-2009","2010-2014")

wvs <- data.frame(anio,aniot,ine)

g3=ggplot(wvs) +
  geom_line(aes(x=anio, y=ine, color = "red")) +
  geom_point(aes(x=anio, y=ine),size=3.0) +
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=10)) +
  scale_x_continuous(breaks = seq(1990,2016,by=4))+
  xlab("Año")+
  ylab("Porcentaje") + 
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("Igualitarismo: 1990-2012") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0.5,size = 11,face = "plain")) + 
  guides(colour=F)+
  labs(caption = "Fuente:  World Value Survey 1990/2019.")
g3

# ISSP junio 2019: 4B
# Es responsabilidad del gobierno reducir las diferencias de ingreso entre las personas 
# con altos ingresos y aquellas con bajos ingresos

bar <- data.frame(dose=c("De acuerdo","Ni de acuerdo ni en desacuerdo","En desacuerdo"),
                  per=c(73.7,17.0,9.3)/100) 


g4=ggplot(data=bar,aes(x=reorder(dose,-per),y=per,label=paste(round(per*100,2),"%"))) +
  geom_bar(stat="identity") + 
  # geom_text(aes(label=per), vjust=1.6, color="white", size=5.5)+
  geom_label()+
  # scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=10)) +
  xlab("") +
  ylab("Porcentaje") +
  scale_y_continuous(limits=c(0,1),labels = scales::percent_format(accuracy = 1 ))+
  theme_bw()+
  theme(legend.position="none") +
  ggtitle("Preferencias redistributivas: abril-junio 2019") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=10),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0.5,size = 11,face = "plain"))+
  labs(caption = "Fuente: ISSP - CEP 2019 (n=1343). Análisis incluyen ponderadores.")

g4
# Graficos juntos:  1000 de width y 400 de height
prefAL <- grid.arrange(g3,g4,ncol=2)
prefAL

ggsave(prefAL,filename = "images/Figura4.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")

# --------------- Figura 5 ----------------- 
my_data <- data.frame(
  Country = c("Australia","Belgica","Chile","Croacia","Republica checa","Dinamarca","Finlandia",
              "Francia","Georgia","Alemania","Hungria","Islandia","Israel","Japon","Corea del sur",
              "Latvia","Lituania","Nueva Zelanda","Noruega","Rusia","Eslovaquia","Eslovenia",
              "Africa del sur","España","Suecia","Suiza","Turquia","UK","US"),
  Redistribution = c(31.1,48.3,69.4,70.0,27.9,29.2,47.4,53.0,54.8,34.8,47.8,54.3,48.9,29.5,
                     38.6,44.4,46.7,33.5,44.9,51.5,49.2,62.0,40.3,60.7,35.3,15.8,
                     55.2,33.3,27.4)/100)
nrow(my_data)

fill1 <- c("gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85",
           "gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85",
           "gray85","gray85","gray85","red","gray85")

g5=ggplot(my_data, aes(x=reorder(Country,Redistribution),y=Redistribution)) + 
  geom_bar(stat="identity", position="dodge",fill=fill1,colour="black", show.legend=FALSE) +
  geom_text(aes(label=paste(Redistribution*100)),hjust=-0.2,size=3.5) +
  geom_hline(yintercept =0.455,size=.5,colour="red",linetype="solid") +
  scale_y_continuous(breaks=seq(0,72,20)/100,limits=c(0,72)/100,labels = scales::percent_format(accuracy = 1 ) ) +
  labs(x=NULL,y="Sí, sin ninguna duda") +
  coord_flip() +
  theme_bw() +
  annotate("text", x="Republica checa", y=0.545, label="Promedio:45,5",size=4,colour="red") +
  ggtitle("¿Es responsabilidad del gobierno reducir las diferencias de ingreso entre ricos y pobres?") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=12, face="bold"),
        plot.title=element_text(hjust=0,size = 12,face = "italic"))+
  labs(caption = "Fuente: ISSP 2016 - Role of Goverment")
g5

# LAPOP
# El Estado chileno debe implementar políticas firmes para reducir la desigualdad de ingresos entre ricos y pobres. ¿Hasta qué punto está de acuerdo o en desacuerdo con esta frase?

# Ahora, vamos a usar una escalera en donde el número 1 representa “muy en desacuerdo” y el número 7 representa “muy de acuerdo”. Un número entre el 1 y el 7, representa un puntaje intermedio.

my_data1 <- data.frame(
  Country = c("Argentina","Bolivia","Brasil","Chile","Colombia","Costa Rica",
              "Rep. Dominicana","Ecuador","El Salvador","Guatemala","Honduras","Mexico",
              "Nicaragua","Panama","Paraguay","Peru","Uruguay","Venezuela"),
  Redistribution = c(63.56,46.25,56.39,65.63,57.94,62.73,
                     65.98,44.66,53.07,50.93,56.20,55.18,
                     53.56,52.96,46.93,51.27,61.05,37.65)/100)
nrow(my_data1)
fill2 <- c("gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85",
           "gray85","gray85","gray85","gray85","gray85","gray85","gray85","gray85","red",
           "gray85")

g6=ggplot(my_data1, aes(x=reorder(Country,Redistribution),y=Redistribution)) + 
  geom_bar(stat="identity", position="dodge",fill=fill2,colour="black", show.legend=FALSE) +
  geom_text(aes(label=paste(Redistribution*100)),hjust=-0.2,size=3.5) +
  geom_hline(yintercept =0.5455,size=.5,colour="red",linetype="solid") +
  scale_y_continuous(breaks=seq(0,70,20)/100,limits=c(0,80)/100,labels = scales::percent_format(accuracy = 1 )) +
  labs(x=NULL,y="Alto nivel de apoyo") +
  coord_flip() +
  theme_bw() +
  annotate("text", x="Ecuador", y=0.67, label="Promedio:54,55",size=4,colour="red") +
  ggtitle("El Estado debe implementar políticas firmes para reducir la desigualdad de ingresos entre ricos y pobres") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=12, face="bold"),
        plot.title=element_text(hjust=0,size = 12,face = "italic")) +
  labs(caption = "Fuente:  Latin American Public Opinion Project (2016/2017)")
g6

pref1 <- grid.arrange(g5,g6,ncol=2)	   

ggsave(pref1,filename = "images/Figura5.png",device = "png",width = 50,height = 25,dpi = "retina",units = "cm")


# --------------- Figura 6 ----------------- 

cl16 <-issp16 %>% filter(country==152) %>% select(gsalud=v14,geduc=v16,gpension=v18,gunemp=v19,WEIGHT) %>% na.omit()

cl16[cl16==8] <- NA
cl16[cl16==9] <- NA

cl16$gsalud   <- car::recode(cl16$gsalud,  recode="1:2=1;4:5=2;3=3")
cl16$geduc    <- car::recode(cl16$geduc   , recode="1:2=1;4:5=2;3=3")
cl16$gpension <- car::recode(cl16$gpension, recode="1:2=1;4:5=2;3=3")
cl16$gunemp   <- car::recode(cl16$gunemp  , recode="1:2=1;4:5=2;3=3")

cl16 <- na.omit(cl16)

prop.table(table(cl16$gsalud))
prop.table(table(cl16$geduc))
prop.table(table(cl16$gpension))
prop.table(table(cl16$gunemp))

prop.table(wtd.table(cl16$gsalud,weights = cl16$WEIGHT))
prop.table(wtd.table(cl16$geduc,weights = cl16$WEIGHT))
prop.table(wtd.table(cl16$gpension,weights = cl16$WEIGHT))
prop.table(wtd.table(cl16$gunemp,weights = cl16$WEIGHT))

margin.table(table(cl16$gsalud))
margin.table(table(cl16$geduc))
margin.table(table(cl16$gpension))
margin.table(table(cl16$gunemp))

# 1= Gastar más
# 2= Gastar menos
# 2= Gastar lo mismo

# "Le voy a leer varias áreas del gasto del gobierno. Para cada una de ellas por favor dígame si a Ud. le gustaría que se gastara más o menos en cada una de ellas"
# "Recuerde que si Ud. dice “mucho más”, podría ser necesario aumentar los impuestos"


gov1<- plot_likert(cl16[,c("gsalud","geduc","gpension","gunemp")],cat.neutral = 3,values="sum.outside",
                   axis.labels = c("Salud","Educación","Mejores pensiones","Beneficio desempleo"),
                   show.n = F,reverse.scale = FALSE,weight.by = cl16$WEIGHT) +
  labs(title ="Areas de gasto del gobierno. Dígame si a Ud. le gustaría que se gastara más o menos en cada una de ellas.",
       subtitle = "'Mucho más', podría ser necesario aumentar los impuestos.", 
       caption = "International Social Survey Programme - Role of Goverment 2016 (n=1349). Análisis incluyen ponderadores.")+ theme_classic()+
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(hjust=0.5,size = 14,face = "bold"),
        legend.position = "bottom",
        plot.subtitle = element_text(hjust=0.5,size = 12,face = "italic")) +
  scale_fill_brewer(labels = c("Gastar lo mismo","Gastar Menos","Gastar más" ),breaks=c("neutral",2,1))
gov1

ggsave(gov1,filename = "images/Figura6.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")


# --------------- Figura 7 ----------------- 

fuente2 <- "International Social Survey Programme - CEP Mayo 2019"
cl19 <- rename(cl19,rgov=M2_P4_2)
cl19$rgov <- car::recode(cl19$rgov, recode="1:2=1;4:5=2;3=3;8:9=NA")


col19 <- cl19 %>% select(M2_P20_1:M2_P20_4,M2_P21_1,M2_P23,FACTOR) %>% rename(sind=M2_P20_1,repres=M2_P20_2,masalary=M2_P20_3,gersalary=M2_P20_4) 
col19[col19==8] <- NA
col19[col19==9] <- NA

col19 <- na.omit(col19)
col19 <- sjlabelled::remove_all_labels(x = col19)

col19$sind      <- car::recode(col19$sind, recode="1:2=1;4:5=2;3=3",as.numeric = TRUE)
col19$repres    <- car::recode(col19$repres, recode="1:2=1;4:5=2;3=3",as.numeric = TRUE)
col19$masalary  <- car::recode(col19$masalary, recode="1:2=1;4:5=2;3=3",as.numeric = TRUE)
col19$gersalary <- car::recode(col19$gersalary, recode="1:2=1;4:5=2;3=3",as.numeric = TRUE)

titles2 <- c("Los trabajadores necesitan sindicatos fuertes para proteger sus intereses.",
             "Los trabajadores deberían tener representantes en el directorio de las grandes empresas.")
titles3 <- c("El Estado debería asegurar que los salarios de los trabajos de bajo sueldo aumenten cuando la economía crece.",
             "El Estado debería tomar medidas para limitar los salarios de los gerentes de grandes empresas.")

state1<- plot_likert(select(col19,"masalary","gersalary"),cat.neutral = 3,show.n = FALSE,values="sum.outside",
                     axis.labels = titles3,
                     reverse.scale = FALSE,weight.by = col19$FACTOR) + theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(hjust=0.5,size = 14,face = "bold"),
        legend.position = "bottom",
        plot.subtitle = element_text(hjust=0,size = 12,face = "italic")) +
  labs(caption = paste(fuente2,"(n=1244).","Análisis incluyen ponderadores."),title = "Rol Estado en la acumulación de riqueza") +
  scale_fill_brewer(labels = c("Gastar lo mismo","Gastar Menos","Gastar más" ),breaks=c("neutral",2,1))
state1

ggsave(state1,filename = "images/Figura7.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")

# --------------- Figura 8 ----------------- 

col19$M2_P21_1 <- car::recode(col19$M2_P21_1, recode="1:2=1;4:5=2;3=3;8:9=NA",as.numeric = TRUE)
cl16b <- col19 %>%  rename(pppublicas=M2_P21_1)

ppolicy1<- ggplot(data =as.data.frame(prop.table(wtd.table(cl16b$pppublicas,weights = cl16b$FACTOR))) ,
                  mapping = aes(y = Freq,x = Var1,label=paste(round(Freq*100,1),"%"))) +
  geom_bar(stat = "identity") +
  geom_label()+
  scale_y_continuous(limits=c(0,1),labels = scales::percent_format(accuracy = 1)) +
  labs(title ="Las políticas públicas generalmente reflejan lo \n que quiere la mayoría de los ciudadanos")+
  scale_x_discrete(limits=1:3,labels=c("De acuerdo", "En desacuerdo\n ", "Neutro"),name=NULL)+
  ylab(label = NULL)+
  theme_bw()+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0.5,size = 10,face = "italic"))
ppolicy1


col19$M2_P23 <- car::recode(col19$M2_P23, recode="1:2=1;3:4=2;88:99=3",as.numeric = TRUE)
cl16c <- col19 %>%  rename(ppubmeb=M2_P23)
prop.table(table(cl16c$ppubmeb))
nrow(cl16c)

ppolicy2<- ggplot(data =as.data.frame(prop.table(wtd.table(cl16c$ppubmeb,weights = cl16c$FACTOR))) ,
                  mapping = aes(y = Freq,x = Var1,label=paste(round(Freq*100,1),"%"))) +
  geom_bar(stat = "identity") +
  geom_label()+
  scale_y_continuous(limits=c(0,1),labels = scales::percent_format(accuracy = 1)) +
  labs(title ="¿Cuán diferentes cree usted serían las políticas públicas si los miembros del Congreso \n fueran más parecidos a los demás ciudadanos en términos de riqueza?")+
  scale_x_discrete(limits=1:3,labels=c("Serían Diferentes", "No serían \n diferentes", "No sabe\n No contesta "),name=NULL)+
  ylab(label = NULL)+
  theme_bw()+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        plot.title=element_text(hjust=0.5,size = 10,face = "italic"))
ppolicy2

pol01<- grid.arrange(ppolicy1,ppolicy2, nrow = 1,
                     bottom=textGrob(label = "International Social Survey Programme - CEP Mayo 2019 (N=1244). Análisis incluyen ponderadores.", 
                                     just = "left",hjust = -0.39,gp=gpar(fontsize=9,font=1)))

ggsave(pol01,filename = "images/Figura8.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")


# --------------- Figura 9 ----------------- 

union2<- plot_likert(col19[,c("sind","repres")],cat.neutral = 3,show.n = FALSE,values="sum.outside",
                     axis.labels = titles2,
                     reverse.scale = FALSE,weight.by = col19$FACTOR) + theme_classic() +
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(hjust=0.5,size = 14,face = "bold"),
        legend.position = "bottom",
        plot.subtitle = element_text(hjust=0,size = 12,face = "italic")) +
  labs(caption = paste(fuente2,"(n=1244)","Análisis incluyen ponderadores."),title = "Rol de los sindicatos en la acumulación de riqueza") +
  scale_fill_brewer(labels = c("Gastar lo mismo","Gastar Menos","Gastar más" ),breaks=c("neutral",2,1))
union2

ggsave(union2,filename = "images/Figura9.png",device = "png",width = 30,height = 15,dpi = "retina",units = "cm")

