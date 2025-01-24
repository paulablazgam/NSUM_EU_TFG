#..........
#R Packages
#..........

library(networkscaleup) # https://cran.r-project.org/web/packages/networkscaleup/vignettes/FittingNetworkScaleup.html
#library(NSUM)

library(readr)
library(digest)
library(readxl)
library(data.table)
library(ggplot2)


#############################################################################################################################################

#FUNCTIONS 
#...................
#Data pre processing
#...................

prep<-function(df){
  #...................
  # Muestreo
  #...................
  df$Age<-(2024-as.numeric(df$Year))
  suma_edades24 <- sum(df$Age >= 18 & df$Age <= 24)
  suma_edades34 <- sum(df$Age >= 25 & df$Age <= 34)
  suma_edades44 <- sum(df$Age >= 35 & df$Age <= 44)
  suma_edades54 <- sum(df$Age >= 45 & df$Age <= 54)
  suma_edades55 <- sum(df$Age >= 55)
  edades<-data.frame("18-24"=suma_edades24,"25-34"=suma_edades34,"35-44"=suma_edades44,"45-55"=suma_edades54,">55"=suma_edades55)
  cat("Las proporciones por rango de edad del muestreo son:","\n")
  print(prop.table(edades)*100)
  #Respondent voting inputs
  voting.inputs <<- colnames(df)[3:9]
  #Sociodemo characteristics
  socio.demo.inputs <<- colnames(df)[1:2]
  #Control inputs
  control.inputs <<-colnames(df)[11:16]
  #Edades inputs (for outlier detection and removal)
  edades.inputs <<- colnames(df)[11:13]
  #Input data set 
  dat <<- df[,c(socio.demo.inputs,voting.inputs,control.inputs)]
  dat[c(voting.inputs,control.inputs)] <<- lapply(dat[c(voting.inputs,control.inputs)], function(x) as.numeric(as.character(x)))
  colnames(dat)
  dim(dat)
  #Null
  num_filas_nulas_vacias <- sum(apply(dat, 1, function(fila) any(is.na(fila) | fila == "")))
  cat("Hay", num_filas_nulas_vacias, "filas con nulos.\n")
  dat <<- na.omit(dat)  # Eliminar filas con valores nulos
  cat("Dimensiones después de eliminar nulos:", dim(dat), "\n")
  invisible(dat)
}
#...................
#Resultados
#..................

results<-function(input.dat,subpopulation.sizes,total,resultados.reales,voting.inputs,control.inputs){
    dat.nsum <- input.dat[,c(voting.inputs,control.inputs)]
    control.ind <-  which(colnames(dat.nsum) %in% control.inputs)
  #Naive approach
  naive.results <- colMeans(dat.nsum[,voting.inputs]/rowSums(dat.nsum[,voting.inputs]),na.rm = TRUE)
  #Naive approach (second alternative Naive2)
  naive2.results <- colSums(dat.nsum[,voting.inputs])/sum(rowSums(dat.nsum[,voting.inputs]),na.rm = TRUE)
  #Ratio of sums approach
  RoS.results <- colSums(dat.nsum[,voting.inputs])/sum(dat.nsum[,voting.inputs])
  #MoS approach (Hebecker 2015)
  MoS.degrees <- total*rowMeans(dat.nsum[,control.inputs]/subpopulation.sizes)
  MoS.sizes <- total*apply(dat.nsum[,voting.inputs],2,function (x) mean(x/MoS.degrees,na.rm = TRUE))
  MoS.results <- prop.table(MoS.sizes)
  #MLE method
  mle.est <- networkscaleup::killworth(dat.nsum,known_sizes=subpopulation.sizes,known_ind=control.ind,N=total,model="MLE")
  mle.results <- prop.table(total*apply(dat.nsum[,voting.inputs],2,function (x) mean(x)/mean(mle.est$degrees)))
  mle.est$sizes/sum(mle.est$sizes)
  #Plug-in MLE method
  pimle.est <- networkscaleup::killworth(dat.nsum,known_sizes=subpopulation.sizes,known_ind=control.ind,N=total,model="PIMLE")
  pimle.results <- prop.table(total*apply(dat.nsum[,voting.inputs],2,function (x) mean(x[pimle.est$degrees>0]/pimle.est$degrees[pimle.est$degrees>0]))) 
  #Overdispersed 
  #overdispersed.est <- networkscaleup::overdispersed(dat.nsum,known_sizes=subpopulation.sizes,known_ind=control.ind,N=total)
  #overdispersed.results <- prop.table(overdispersed.est <- colMeans(overdispersed.est$sizes)[1:7])
  
  #Summary
  Summary <- round(t(data.frame(Naive = naive.results, Naive2 = naive2.results,MoS = MoS.results,RoS = RoS.results,MLE=mle.results,PIMLE=pimle.results,Reales=resultados.reales)),4)*100
  return(Summary)
}
#...................
#Functions for outlier detection
#..................

median_abs_deviation <- function(x) {
  qq.scaled <- quantile(scale(x), c(.25, .5, .75), na.rm = T)
  quantile(abs(x - quantile(x, c(0.5), na.rm = T)), c(0.5), na.rm = T) * 1.4826
}

is_mad_outlier <- function(x,threshold.mad=5) {
  abs(x - quantile(x, c(0.5), na.rm = T)) / median_abs_deviation(x) > threshold.mad
}

#...................
# Función para calcular la distribución de escaños utilizando el método D'Hondt
#..................
#SIN OTROS

dHont <- function(results, seats, reales) {
  # Función auxiliar para calcular los escaños utilizando el método D'Hondt
  compute_dhont_seats <- function(votes, seats) {
    # Crear un vector para almacenar los escaños asignados a cada partido
    party_seats <- rep(0, length(votes))
    
    # Bucle para asignar los escaños
    for (i in 1:seats) {
      # Calcular las ratios de asignación para cada partido
      allocation_ratios <- votes / (party_seats + 1)
      
      # Encontrar el partido con la ratio más alta
      party_with_highest_ratio <- which.max(allocation_ratios)
      
      # Asignar un escaño al partido con la ratio más alta
      party_seats[party_with_highest_ratio] <- party_seats[party_with_highest_ratio] + 1
    }
    
    # Devolver el vector de escaños asignados
    return(party_seats)
  }
  
  # Crear una lista para almacenar los resultados de las distribuciones de escaños
  seat_distributions <- list()
  
  # Iterar sobre las filas de la tabla de resultados (primeras 6 filas)
  for (method in rownames(results[1:6, ])) {
    # Calcular la distribución de escaños para cada método
    seat_distributions[[method]] <- compute_dhont_seats(results[method, 1:6], seats)
  }
  
  # Convertir la lista en un data frame organizado
  seat_table <- do.call(rbind, seat_distributions)
  colnames(seat_table) <- colnames(results)[1:6]  # Asignar nombres de partidos
  
  # Agregar los resultados reales como la última fila
  seat_table <- rbind(seat_table, Reales = reales)
  
  # Devolver la tabla final
  return(seat_table)
}

#CON OTROS 

dHont2 <- function(results, seats, reales) {
  # Función auxiliar para calcular los escaños utilizando el método D'Hondt
  compute_dhont_seats <- function(votes, seats) {
    # Crear un vector para almacenar los escaños asignados a cada partido
    party_seats <- rep(0, length(votes))
    
    # Bucle para asignar los escaños
    for (i in 1:seats) {
      # Calcular las ratios de asignación para cada partido
      allocation_ratios <- votes / (party_seats + 1)
      
      # Encontrar el partido con la ratio más alta
      party_with_highest_ratio <- which.max(allocation_ratios)
      
      # Asignar un escaño al partido con la ratio más alta
      party_seats[party_with_highest_ratio] <- party_seats[party_with_highest_ratio] + 1
    }
    
    # Devolver el vector de escaños asignados
    return(party_seats)
  }
  
  # Crear una lista para almacenar los resultados de las distribuciones de escaños
  seat_distributions <- list()
  
  # Iterar sobre las filas de la tabla de resultados (primeras 6 filas)
  for (method in rownames(results[1:6, ])) {
    # Calcular la distribución de escaños para cada método
    seat_distributions[[method]] <- compute_dhont_seats(results[method, 1:7], seats)
  }
  
  # Convertir la lista en un data frame organizado
  seat_table <- do.call(rbind, seat_distributions)
  colnames(seat_table) <- colnames(results)[1:7]  # Asignar nombres de partidos
  
  # Agregar los resultados reales como la última fila
  seat_table <- rbind(seat_table, Reales = reales)
  
  # Devolver la tabla final
  return(seat_table)
}

#############################################################################################################################################
#SPAIN
#############################################################################################################################################
#Import dataset into R (code used for importing the data)
#.......................................

Pollfish_Survey_Elecciones_Europeas_es <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/UNIVERSIDAD/TFG/Data/Pollfish_Survey_Elecciones_europeas_Espana_389886097_es.xlsx")
df.spain <- as.data.frame(Pollfish_Survey_Elecciones_Europeas_es[,-3]) # The column with the permission is removed
dim(df.spain)
colnames(df.spain) <- c("Gender","Year.Birth","PP","PSOE","Vox","Sumar","UP","Ahora.Republicas","Blanco","NoSabe","edad.18.34","edad.35.54","edad.mas.54","autonomo","desempleado","medico","Education","Employment.Status","Income")

#https://es.wikipedia.org/wiki/Elecciones_al_Parlamento_Europeo_de_2024_(España)#Resultados[66]%E2%80%8B
spain.resultados <- c(0.3421,0.3019,0.0963,0.0467,0.033,0.0491,0.007)
spain.resultados.otros <- c(0.3421,0.3019,0.0963,0.0467,0.033,0.0491,0.1309)

spain.resultados.DHont<-c(22,20,6,3,2,3,5)
spain.escanos <- 61

#.......................................
#Preproc
#.......................................
dat.spain<-prep(df.spain)
# Encuestas que solo respondan su votacion
dat.spain <- dat.spain[rowSums(dat.spain[, 3:15]) > 1, ]



#.......................................
#Real data for control inputs
#.......................................

#spain.subpopulation<- c(18-34,35-54,+55,autonomos,desempleados,medicos)  
#autonomos    #https://www.mites.gob.es/estadisticas/AUT/AUT_06_2024.pdf
#medicos      #https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176781&menu=ultiDatos&idp=1254735573175
#desempleados #https://revista.seg-social.es/-/paro-junio-2023#:~:text=El%20paro%20registrado%20se%20ha,en%20concreto%20desde%20septiembre%202008.
#poblacion    #https://www.ine.es/jaxi/Datos.htm?tpx=68060

spain.subpopulation <- c(7840700,12937733,17005955,3400012,2561067,301684)  
spain.total <- 37784388 
names(spain.subpopulation) <- control.inputs

#.......................................
#Identification of outlying observations
#.......................................

# Functions for outlier detection
median_abs_deviation <- function(x) {
  qq.scaled <- quantile(scale(x), c(.25, .5, .75), na.rm = T)
  quantile(abs(x - quantile(x, c(0.5), na.rm = T)), c(0.5), na.rm = T) * 1.4826
}


is_mad_outlier <- function(x,threshold.mad=5) {
  abs(x - quantile(x, c(0.5), na.rm = T)) / median_abs_deviation(x) > threshold.mad
}

#.......................................
# Raw Data
#.......................................

results(dat.spain,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs = control.inputs)

#.......................................
# 1. Criterio para eliminar a los encuestados con una red por encima de un umbral específico
#.......................................

threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 
spain.input1<- dat.spain[which(apply(dat.spain[,voting.inputs],1,sum)<quantile(apply(dat.spain[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)),]

#Results
(spain.results1<-results(spain.input1,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs = control.inputs))

(spain.Dhont1 <- dHont(spain.results1, spain.escanos,spain.resultados.DHont))

dif1<-as.data.frame(abs(spain.results1 - matrix(rep(spain.resultados*100, each = nrow(spain.results1)), ncol = ncol(spain.results1), byrow = FALSE)))
mean(rowSums(dif1))


(spain.results12<-results(spain.input1,spain.subpopulation,spain.total,spain.resultados.otros,voting.inputs,control.inputs = control.inputs))
dif12<-as.data.frame(abs(spain.results12 - matrix(rep(spain.resultados.otros*100, each = nrow(spain.results1)), ncol = ncol(spain.results1), byrow = FALSE)))
mean(rowSums(dif12))

#.......................................
# 2. Criterio de filtrado para el voto mayoritario de un grupo político 
#.......................................
spain.input2 <- dat.spain[which(apply(dat.spain[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results
(spain.results2<-results(spain.input2,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))

(spain.Dhont2 <- dHont(spain.results2, spain.escanos,spain.resultados.DHont))

dif2<-as.data.frame(abs(spain.results2 - matrix(rep(spain.resultados*100, each = nrow(spain.results2)), ncol = ncol(spain.results2), byrow = FALSE)))
mean(rowSums(dif2))

(spain.results21<-results(spain.input2,spain.subpopulation,spain.total,spain.resultados.otros,voting.inputs,control.inputs))
dif21<-as.data.frame(abs(spain.results21 - matrix(rep(spain.resultados.otros*100, each = nrow(spain.results2)), ncol = ncol(spain.results2), byrow = FALSE)))
mean(rowSums(dif21))
#.......................................
# 3. Criterio de filtrado del voto en blanco inusual 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.spain[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.spain[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
spain.input3 <- dat.spain[which(dat.spain[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
(spain.results3<-results(spain.input3,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))
(spain.Dhont3 <- dHont(spain.results3, spain.escanos,spain.resultados.DHont))
dif3<-as.data.frame(abs(spain.results3 - matrix(rep(spain.resultados*100, each = nrow(spain.results3)), ncol = ncol(spain.results3), byrow = FALSE)))
mean(rowSums(dif3))

(spain.results31<-results(spain.input3,spain.subpopulation,spain.total,spain.resultados.otros,voting.inputs,control.inputs))
dif31<-as.data.frame(abs(spain.results31 - matrix(rep(spain.resultados.otros*100, each = nrow(spain.results2)), ncol = ncol(spain.results2), byrow = FALSE)))
mean(rowSums(dif31))

#.......................................
# Filtro 2 y 3
#.......................................

spain.input4 <- dat.spain[which(apply(dat.spain[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.spain[,voting.inputs[5]]<=threshold.net.size.vote.blank),] 
dim(spain.input4)

sum(rowSums(spain.input4[,voting.inputs]))
mean(rowSums(spain.input4[,voting.inputs]))

(spain.results4<-results(spain.input4,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))

(spain.Dhont4 <- dHont(spain.results4, spain.escanos,spain.resultados.DHont))

dif4<-as.data.frame(abs(spain.results4 - matrix(rep(spain.resultados*100, each = nrow(spain.results4)), ncol = ncol(spain.results4), byrow = FALSE)))
mean(rowSums(dif4))

(spain.results41<-results(spain.input4,spain.subpopulation,spain.total,spain.resultados.otros,voting.inputs,control.inputs))
dif41<-as.data.frame(abs(spain.results41 - matrix(rep(spain.resultados.otros*100, each = nrow(spain.results4)), ncol = ncol(spain.results4), byrow = FALSE)))
mean(rowSums(dif41))

#.......................................
# Filtering criterion together
#.......................................
spain.input5 <- dat.spain[which(apply(dat.spain[,voting.inputs],1,sum) < quantile(apply(dat.spain[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.spain[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.spain[,voting.inputs[5]]<=threshold.net.size.vote.blank),] 
#Results
(spain.results5<-results(spain.input5,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))
(spain.Dhont5 <- dHont(spain.results5, spain.escanos,spain.resultados.DHont))

dif5<-as.data.frame(abs(spain.results5 - matrix(rep(spain.resultados*100, each = nrow(spain.results5)), ncol = ncol(spain.results5), byrow = FALSE)))
mean(rowSums(dif5))

(spain.results51<-results(spain.input5,spain.subpopulation,spain.total,spain.resultados.otros,voting.inputs,control.inputs))
dif51<-as.data.frame(abs(spain.results51 - matrix(rep(spain.resultados.otros*100, each = nrow(spain.results5)), ncol = ncol(spain.results5), byrow = FALSE)))
mean(rowSums(dif51))

#.......................................
# Final Results
#.......................................
spain.input<- dat.spain[which(apply(dat.spain[,voting.inputs],1,sum)<quantile(apply(dat.spain[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)),]

spain.results<-results(input.dat = spain.input,subpopulation.sizes = spain.subpopulation,total = spain.total,resultados.reales = spain.resultados.otros,voting.inputs = voting.inputs,control.inputs = control.inputs)
colnames(spain.results)[7]<-"Otros"
(spain.Dhont <- dHont2(spain.results, spain.escanos,spain.resultados.DHont))



CIS<-c(29.4, 32.4, 10.5, 6.3, 3.8, 3.9, 13.7)
GAD3<-c(34.5, 31.4, 9.3, 5.7, 3.0, 4.5, 11.6 )

escanos.cis<-c(19, 20 ,6 ,4 ,2 ,2 ,8)
escanos.gad<-c(23, 21, 6, 3, 2, 3, 3)

spain.results<-data.frame(rbind(spain.results[-7,],CIS,GAD3))
spain.Dhont<-data.frame(rbind(spain.Dhont[-7,],escanos.cis,escanos.gad))

spain.resultados.otros<- matrix(rep(spain.resultados.otros*100, each = nrow(spain.results)), ncol = ncol(spain.results), byrow = FALSE)
spain.dif<-as.data.frame(abs(spain.results-spain.resultados.otros))
spain.mae.porcentaje<-rowMeans(spain.dif)
spain.mape.porcentaje<- round(rowMeans(spain.dif/spain.resultados.otros)*100,2)

spain.resultados.DHont<-matrix(rep(spain.resultados.DHont, each = nrow(spain.Dhont)), ncol = ncol(spain.Dhont), byrow = FALSE)
spain.dif.dhont<-as.data.frame(abs(spain.Dhont -spain.resultados.DHont ))
spain.mae.escanos<-round(rowMeans(spain.dif.dhont),2)
spain.mape.escanos<- round(rowMeans(spain.dif.dhont/spain.resultados.DHont)*100,2)


##ESTUDIO DE MUESTREO



library(ggplot2)
library(gridExtra)



df.spain$Income <- factor(df.spain$Income,
                          levels = c("high_i", "high_ii", "high_iii", "middle_i","middle_ii","lower_i","lower_ii","prefer_not_to_say"))
levels(df.spain$Income) <- c("Alto_1","Alto_2","Alto_3","Medio_1","Medio_2","Bajo_1","Bajo_2","Prefiere_No_Decir")
Income_props <- as.data.frame(prop.table(table(df.spain$Income)))
colnames(Income_props) <- c("Salario", "Proporcion")  # Renombrar columnas
p1<-ggplot(Income_props, aes(x = Salario, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "lightblue3") +  # "identity" usa los valores de frecuencia
  labs(title = "Distribución por Salario", x = "Salario", y = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


df.spain$Education <- factor(df.spain$Education,
                             levels = c("postgraduate","university","vocational_technical_college","high_school","middle_school"))
levels(df.spain$Education) <- c("Posgraduado","Universidad", "FP", "Bachillerato","Secundaria")
Education_props <- as.data.frame(prop.table(table(df.spain$Education)))
colnames(Education_props) <- c("Educacion", "Proporcion")  # Renombrar columnas
p2<-ggplot(Education_props, aes(x = Educacion, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "lightsalmon3") +  # "identity" usa los valores de frecuencia
  labs(title = "Distribución por Nivel Educativo", x = "Nivel Educativo", y = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


df.spain$Employment.Status <- factor(df.spain$Employment.Status,
                                     levels = c("employed_for_wages","self_employed","military","retired","homemaker","student","unemployed_looking","unemployed_not_looking","unable_to_work","other"))
levels(df.spain$Employment.Status) <- c("Empleado","Autónomo","Militar","Retirado","Casa","Estudiante","Buscando","No_Buscando","Incapaz","Otro")
Empleo_props <- as.data.frame(prop.table(table(df.spain$Employment.Status)))
colnames(Empleo_props) <- c("Empleo", "Proporcion")  # Renombrar columnas
p3<-ggplot(Empleo_props, aes(x = Empleo, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "lightpink3") +  # "identity" usa los valores de frecuencia
  labs(title = "Distribución por Empleo", x = "Empleo", y = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


df.spain$Gender <- factor(df.spain$Gender,
                                     levels = c("male","female"))
levels(df.spain$Gender) <- c("Hombre","Mujer")
Genero_props <- as.data.frame(prop.table(table(df.spain$Gender)))
colnames(Genero_props) <- c("Genero", "Proporcion")  # Renombrar columnas
p4<-ggplot(Genero_props, aes(x = Genero, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "lightgoldenrod3") +  # "identity" usa los valores de frecuencia
  labs(title = "Distribución por Género", x = "Género", y = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si son largas


grid.arrange(p1, p2, p3,p4, nrow = 2, ncol = 2)

#############################################################################################################################################
#FRANCE
#############################################################################################################################################
#Import dataset into R (code used for importing the data)
#.......................................

Pollfish_Survey_Elecciones_Europeas_fr <- read_excel("/Users/paulablazquez/Library/Mobile Documents/com~apple~CloudDocs/UNIVERSIDAD/TFG/Data/Pollfish_Survey_Elections_au_Parlement_europeen_de_2024_389899575_fr.xlsx")
df.france <- as.data.frame(Pollfish_Survey_Elecciones_Europeas_fr[,-3]) # The column with the permission is removed
dim(df.france)
colnames(df.france) <- c("Gender","Year.Birth","BdE","La France Revient","LE","LR","LFI","RlE","Blanco","NoSabe","edad.18.34","edad.35.54","edad.mas.54","autonomo","desempleado","medico","Education","Employment.Status","Income")
#https://es.wikipedia.org/wiki/Elecciones_al_Parlamento_Europeo_de_2024_(Francia)

france.resultados <- c(0.1460,0.3137,0.055,0.0725,0.0989,0.1383,0.0137)
france.resultados.DHont<-c(13,30,5,6,9,13,5)
france.escanos <- 81
france.resultados.otros<-c(0.1460,0.3137,0.055,0.0725,0.0989,0.1383,0.1756)

#.......................................
#Preproc
#.......................................

dat.france<-prep(df.france)
# Encuestas que solo respondan su votacion
dat.france <- dat.france[rowSums(dat.france[, 3:15]) > 1, ]
summary(dat.france)

#.......................................
#Real data for control inputs
#.......................................

#france.subpopulation<- c(18-34,35-54,+55,autonomos,desempleados,medicos)  
#autonomos    #?? He puesto aaprox
#medicos      #https://www.francebleu.fr/infos/sante-sciences/une-legere-hausse-du-nombre-de-medecins-en-france-en-2024-mais-les-inegalites-territoriales-se-creusent-encore-2614040
#desempleados #https://www.insee.fr/en/statistiques/8283457
#poblacion    #https://www.insee.fr/fr/statistiques/2381474
              #https://www.insee.fr/fr/statistiques/8192479#onglet-3


france.subpopulation <- c(11771902 ,17258866,23414389,3700000,2300000,199089)  
france.total <- 49500000 
names(france.subpopulation) <- control.inputs

#.......................................
# Raw Data
#.......................................

results(dat.france,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs)

#.......................................
#Identification of outlying observations
#.......................................

#.......................................
# 1.Criterio para eliminar a los encuestados con una red por encima de un umbral específico
#.......................................

threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 
france.input1<- dat.france[which(apply(dat.france[,voting.inputs],1,sum)<quantile(apply(dat.france[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)),]

#Results
(france.results1<-results(france.input1,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs))
(france.Dhont1 <- dHont(france.results1, france.escanos,france.resultados.DHont))

france.dif1<-as.data.frame(abs(france.results1 - matrix(rep(france.resultados*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif1))

(france.results11<-results(france.input1,france.subpopulation,france.total,france.resultados.otros,voting.inputs,control.inputs = control.inputs))
france.dif11<-as.data.frame(abs(france.results11 - matrix(rep(france.resultados.otros*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif11))

#.......................................
# 2.Criterio de filtrado para el voto mayoritario de un grupo político 
#.......................................
france.input2 <- dat.france[which(apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results

(france.results2<-results(france.input2,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs))
(france.Dhont2 <- dHont(france.results2, france.escanos,france.resultados.DHont))


france.dif2<-as.data.frame(abs(france.results2 - matrix(rep(france.resultados*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif2))

(france.results21<-results(france.input2,france.subpopulation,france.total,france.resultados.otros,voting.inputs,control.inputs = control.inputs))
france.dif21<-as.data.frame(abs(france.results21 - matrix(rep(france.resultados.otros*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif21))

#.......................................
# 3.Criterio de filtrado del voto en blanco inusual 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.france[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.france[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
france.input3 <- dat.france[which(dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
(france.results3<- results(france.input3,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs))
(france.Dhont3 <- dHont(france.results3, france.escanos,france.resultados.DHont))


france.dif3<-as.data.frame(abs(france.results3 - matrix(rep(france.resultados*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif3))

(france.results31<- results(france.input3,france.subpopulation,france.total,france.resultados.otros,voting.inputs,control.inputs = control.inputs))
france.dif31<-as.data.frame(abs(france.results31 - matrix(rep(france.resultados.otros*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif31))

#.......................................
# Filtro 2 y 3
#.......................................

france.input4 <- dat.france[which(apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

dim(france.input4)

sum(rowSums(france.input4[,voting.inputs]))
mean(rowSums(france.input4[,voting.inputs]))

(france.results4<-results(france.input4,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs))

(france.Dhont4 <- dHont(france.results4, france.escanos,france.resultados.DHont))

france.dif4<-as.data.frame(abs(france.results4 - matrix(rep(france.resultados*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif4))

(france.results41<-results(france.input4,france.subpopulation,france.total,france.resultados.otros,voting.inputs,control.inputs))
france.dif41<-as.data.frame(abs(france.results41 - matrix(rep(france.resultados.otros*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif41))


#.......................................
# Filtro 1 y 3
#.......................................

france.input5 <- dat.france[which(apply(dat.france[,voting.inputs],1,sum)<quantile(apply(dat.france[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

(france.results5<-results(france.input5,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs))

(france.Dhont5 <- dHont(france.results4, france.escanos,france.resultados.DHont))

france.dif5<-as.data.frame(abs(france.results5 - matrix(rep(france.resultados*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif5))

(france.results5<-results(france.input5,france.subpopulation,france.total,france.resultados.otros,voting.inputs,control.inputs))
france.dif51<-as.data.frame(abs(france.results5 - matrix(rep(france.resultados.otros*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif51))

#.......................................
# Filtering criterion together
#.......................................
france.input6 <- dat.france[which(apply(dat.france[,voting.inputs],1,sum) < quantile(apply(dat.france[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 
#Results
(france.results6<- results(france.input6,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs))
(france.Dhont6 <- dHont(france.results6, france.escanos,france.resultados.DHont))
france.dif6<-as.data.frame(abs(france.results6 - matrix(rep(france.resultados*100, each = nrow(france.results4)), ncol = ncol(france.results4), byrow = FALSE)))
mean(rowSums(france.dif6))

(france.results61<- results(france.input6,france.subpopulation,france.total,france.resultados.otros,voting.inputs,control.inputs = control.inputs))
france.dif61<-as.data.frame(abs(france.results61 - matrix(rep(france.resultados.otros*100, each = nrow(france.results1)), ncol = ncol(france.results1), byrow = FALSE)))
mean(rowSums(france.dif61))

#.......................................
# Final Results
#.......................................
france.input<-dat.france[which(apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 
(france.results<-results(france.input,france.subpopulation,france.total,france.resultados.otros,voting.inputs,control.inputs))
colnames(france.results)[7]<-"Otros"
(france.Dhont <- dHont2(france.results, france.escanos,france.resultados.DHont))

ipsos<-data.frame(15, 32 ,5, 7, 9.5, 14.5 ,17) 
dHont2(ipsos,seats = 81,reales = france.resultados.DHont)
escanos.ipsos<-c(12 , 26  ,4  ,5    ,8    ,12  ,14)
ipsos<-c(15, 32 ,5, 7, 9.5, 14.5 ,17) 

IFOP<-data.frame(14.5, 33, 5.5, 7, 9, 13, 18)
dHont2(IFOP,seats = 81,reales = france.resultados.DHont)
escanos.IFOP<-c( 12 , 27,  4 , 5 ,   7   , 11 ,15)
IFOP<-c(14.5, 33, 5.5, 7, 9, 13, 18)


france.results<-data.frame(rbind(france.results[-7,],ipsos,IFOP))
france.Dhont<-data.frame(rbind(france.Dhont[-7,],escanos.ipsos,escanos.IFOP))

france.resultados.otros<- matrix(rep(france.resultados.otros*100, each = nrow(france.results)), ncol = ncol(france.results), byrow = FALSE)
france.dif<-as.data.frame(abs(france.results-france.resultados.otros))
france.mae.porcentaje<-round(rowMeans(france.dif),2)
france.mape.porcentaje<- round(rowMeans(france.dif/france.resultados.otros)*100,2)

france.resultados.DHont<-matrix(rep(france.resultados.DHont, each = nrow(france.Dhont)), ncol = ncol(france.Dhont), byrow = FALSE)
france.dif.dhont<-as.data.frame(abs(france.Dhont -france.resultados.DHont ))
france.mae.escanos<-round(rowMeans(france.dif.dhont),2)
france.mape.escanos<- round(rowMeans(france.dif.dhont/france.resultados.DHont)*100,2)


#############################################################################################################################################
#ITALY
#############################################################################################################################################
#Import dataset into R (code used for importing the data)
#.......................................

Pollfish_Survey_Elecciones_Europeas_it <- read_excel("/Users/paulablazquez/Library/Mobile Documents/com~apple~CloudDocs/UNIVERSIDAD/TFG/Data/Pollfish_Survey_Elezioni_del_Parlamento_Europeo_del_2024_389899149_it.xlsx")

df.italy <- as.data.frame(Pollfish_Survey_Elecciones_Europeas_it[,-3]) # The column with the permission is removed
dim(df.italy)
colnames(df.italy) <- c("Gender","Year.Birth","FdI","PD","M5S","LSP","FI","IV","Blanco","NoSabe","edad.18.34","edad.35.54","edad.mas.54","autonomo","desempleado","medico","Education","Employment.Status","Income")

#https://it.wikipedia.org/wiki/Elezioni_europee_del_2024_in_Italia
#ITALIA VIVA está dentro de Stati Uniti d'Europa

italy.resultados <- c(0.2876,0.2411,0.0998,0.0897,0.0957,0.0377,0.0221)
italy.resultados.DHont<-c(24,21,8,8,8,0,7)
italy.escanos <- 76
italy.resultados.otros<- c(0.2876,0.2411,0.0998,0.0897,0.0957,0.0377,0.1484)

#.......................................
#Preproc
#.......................................

dat.italy<-prep(df.italy)
# Encuestas que solo respondan su votacion
dat.italy <- dat.italy[rowSums(dat.italy[, 10:15]) > 1, ]
dat.italy <- dat.italy[rowSums(dat.italy[, 3:9]) > 1, ]
summary(dat.italy)

#.......................................
#Real data for control inputs
#.......................................

#https://www.tuttitalia.it/statistiche/popolazione-eta-sesso-stato-civile-2023/

#italy.subpopulation<- c(18-34,35-54,+55,autonomos,desempleados,medicos)  
#autonomos    #https://www.istat.it/it/files/2024/03/GENNAIO-2024-CS-Occupati-e-disoccupati.pdf
#medicos      # https://www.statista.com/statistics/462075/physicians-employment-in-italy/
#desempleados #https://tradingeconomics.com/italy/unemployment-rate
#poblacion    #https://demo.istat.it/app/?i=POS&l=it


italy.subpopulation <- c(10326180,16435669,23154188,5045000,1588000,250813)  
italy.total <- 49916037 
names(italy.subpopulation) <- control.inputs

#.......................................
# Raw Data
#.......................................

results(dat.italy,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs)


#.......................................
#Identification of outlying observations
#.......................................

#.......................................
# 1.Criterio para eliminar a los encuestados con una red por encima de un umbral específico
#.......................................

threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 
italy.input1<- dat.italy[which(apply(dat.italy[,voting.inputs],1,sum)<quantile(apply(dat.italy[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)),]

#Results
(italy.results1<-results(italy.input1,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont1 <- dHont(results = italy.results1, seats = italy.escanos,reales = italy.resultados.DHont))

italy.dif1<-as.data.frame(abs(italy.results1 - matrix(rep(italy.resultados*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif1))
italy.dif11<-as.data.frame(abs(italy.results1 - matrix(rep(italy.resultados.otros*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif11))

#.......................................
# 2.Criterio de filtrado para el voto mayoritario de un grupo político 
#.......................................
italy.input2 <- dat.italy[which(apply(dat.italy[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results
(italy.results2<-results(italy.input2,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont2 <- dHont(italy.results2, italy.escanos,italy.resultados.DHont))

italy.dif2<-as.data.frame(abs(italy.results2 - matrix(rep(italy.resultados*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif2))
italy.dif21<-as.data.frame(abs(italy.results2 - matrix(rep(italy.resultados.otros*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif21))

#.......................................
# 3.Criterio de filtrado del voto en blanco inusual 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.italy[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.italy[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
italy.input3 <- dat.italy[which(dat.italy[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
(italy.results3<-results(italy.input3,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont3 <- dHont(italy.results3, italy.escanos,italy.resultados.DHont))

italy.dif3<-as.data.frame(abs(italy.results3 - matrix(rep(italy.resultados*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif3))
italy.dif31<-as.data.frame(abs(italy.results3 - matrix(rep(italy.resultados.otros*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif31))
#.......................................
# Filtro 2 y 3
#.......................................

italy.input4 <- dat.italy[which(apply(dat.italy[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.italy[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

(italy.results4<-results(italy.input4,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont4 <- dHont(italy.results4, italy.escanos,italy.resultados.DHont))

italy.dif4<-as.data.frame(abs(italy.results4 - matrix(rep(italy.resultados*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif4))
italy.dif41<-as.data.frame(abs(italy.results4 - matrix(rep(italy.resultados.otros*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif41))


#.......................................
# Filtering criterion together
#.......................................
italy.input5 <- dat.italy[which(apply(dat.italy[,voting.inputs],1,sum) < quantile(apply(dat.italy[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.italy[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.italy[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 
#Results
(italy.results5<-results(italy.input5,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont5 <- dHont(italy.results5, italy.escanos,italy.resultados.DHont))

italy.dif5<-as.data.frame(abs(italy.results5 - matrix(rep(italy.resultados*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif5))
italy.dif51<-as.data.frame(abs(italy.results5 - matrix(rep(italy.resultados.otros*100, each = nrow(italy.results1)), ncol = ncol(italy.results1), byrow = FALSE)))
mean(rowSums(italy.dif51))


#.......................................
# Final Results
#.......................................

italy.input <- dat.italy[which(apply(dat.italy[,voting.inputs],1,sum)<quantile(apply(dat.italy[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)),]
(italy.results<-results(italy.input,italy.subpopulation,italy.total,italy.resultados.otros,voting.inputs,control.inputs = control.inputs))
colnames(italy.results)[7]<-"Otros"
(italy.Dhont <- dHont2(italy.results, italy.escanos,italy.resultados.DHont))

EE<-c(26.9 ,20.8 ,15.6 ,9 ,8.6, 4.4,14.7)
Cassandra<-c(26.5, 20.5, 15.5, 8.5, 8.5, 4.5 ,16) 
EE.escanos<-c(23,17,14,7,6,4,5)
Cassandra.escanos<-c(23,17,13,7,7,4,5)

italy.results<-data.frame(rbind(italy.results[-7,],EE,Cassandra))
italy.Dhont<-data.frame(rbind(italy.Dhont[-7,],EE.escanos,Cassandra.escanos))

italy.resultados.otros<- matrix(rep(italy.resultados.otros*100, each = nrow(italy.results)), ncol = ncol(italy.results), byrow = FALSE)
italy.dif<-as.data.frame(abs(italy.results-italy.resultados.otros))

italy.mae.porcentaje<-round(rowMeans(italy.dif),2)
italy.mape.porcentaje<- round(rowMeans(italy.dif/italy.resultados.otros)*100,2)

italy.resultados.DHont<-matrix(rep(italy.resultados.DHont, each = nrow(italy.Dhont)), ncol = ncol(italy.Dhont), byrow = FALSE)
italy.dif.dhont<-as.data.frame(abs(italy.Dhont -italy.resultados.DHont ))
italy.mae.escanos<-round(rowMeans(italy.dif.dhont),2)
italy.mape.escanos<- round(rowMeans(italy.dif.dhont/italy.resultados.DHont)*100,2)



#.......................................
#Distribucion de media por partido
#.......................................

library(tidyr)

library(RColorBrewer)

votos.spain <- dat.spain[, 3:9]
prop.spain  <- data.frame(t(apply(votos.spain, 1, function(x) x / sum(x))))
means.spain <- colMeans(prop.spain, na.rm = TRUE)

final.means.spain <- c(
  means.spain[7],
  sort(means.spain[1:6], decreasing = TRUE)
)

spain_plot <- data.frame(
  Category = names(final.means.spain),
  Value    = unname(final.means.spain),
  Country  = "Spain"
)

votos.france<-(dat.france[,3:9])
prop.france <- data.frame(t(apply(votos.france, 1, function(x) x / sum(x))))
means.france <- colMeans(prop.france, na.rm = TRUE)
final.means.france <- c(
  means.france[7],
  sort(means.france[1:6], decreasing = TRUE)
)
france_plot <- data.frame(
  Category = names(final.means.france),
  Value    = unname(final.means.france),
  Country  = "France"
)

votos.italy<-(dat.italy[,3:9])
prop.italy <- data.frame(t(apply(votos.italy, 1, function(x) x / sum(x))))
means.italy <- colMeans(prop.italy, na.rm = TRUE)
final.means.italy <- c(
  means.italy[7],
  sort(means.italy[1:6], decreasing = TRUE)

)
italy_plot <- data.frame(
  Category = names(final.means.italy),
  Value    = unname(final.means.italy),
  Country  = "Italy"
)

final.means <- rbind(spain_plot, france_plot, italy_plot)
final.means$Category <- factor(final.means$Category, levels = unique(final.means$Category))


seq_palette <- c(brewer.pal(6, "Blues"),brewer.pal(6, "Greens"),brewer.pal(6, "Purples")) # 6 colores de la paleta "Blues"

color_map <- setNames(
  c(seq_palette, "grey"),
  c(cats_no_blanco, "Blanco")
)


ggplot(final.means, aes(x = Country, y = Value, fill = Category)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = color_map) +  # aquí aplicamos la paleta
  labs(title = "Distribución de votos por país",
       x = "País", y = "Proporción") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Centra el título
  )

