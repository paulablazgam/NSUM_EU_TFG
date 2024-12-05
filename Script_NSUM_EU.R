#..........
#R Packages
#..........

library(networkscaleup) # https://cran.r-project.org/web/packages/networkscaleup/vignettes/FittingNetworkScaleup.html
#library(NSUM)

library(readr)
library(digest)
library(readxl)
library(data.table)

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
compute_dhont_seats <- function(votes, seats) {
  # Create a vector to store the number of seats each party receives
  party_seats <- rep(0, length(votes))
  # Loop to allocate seats
  for (i in 1:seats) {
    # Calculate the allocation ratio for each party
    allocation_ratios <- votes / (party_seats + 1)
    
    # Find the party with the highest allocation ratio
    party_with_highest_ratio <- which.max(allocation_ratios)
    
    # Allocate a seat to the party with the highest ratio
    party_seats[party_with_highest_ratio] <- party_seats[party_with_highest_ratio] + 1
  }
  
  # Return the vector of seats allocated to each party
  return(party_seats)
}


dHont <- function(results, seats) {
  # Crear una lista para almacenar los resultados
  seat_distributions <- list()
  
  # Iterar sobre las filas de la tabla de resultados
  for (method in rownames(results)) {
    # Calcular la distribución de escaños para cada fila (método)
    seat_distributions[[method]] <- compute_dhont_seats(results[method, 1:6], seats)
  }
  
  # Convertir la lista en un data frame organizado
  seat_table <- do.call(rbind, seat_distributions)
  colnames(seat_table) <- colnames(results)[1:6]  # Partidos
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
spain.resultados.DHont<-c(22,20,6,3,2,3)
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

(spain.Dhont1 <- dHont(spain.results1, spain.escanos))

#.......................................
# 2. Criterio de filtrado para el voto mayoritario de un grupo político 
#.......................................
spain.input2 <- dat.spain[which(apply(dat.spain[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results
(spain.results2<-results(spain.input2,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))

(spain.Dhont2 <- dHont(spain.results2, spain.escanos))


#.......................................
# 3. Criterio de filtrado del voto en blanco inusual 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.spain[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.spain[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
spain.input3 <- dat.spain[which(dat.spain[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
(spain.results3<-results(spain.input3,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))
(spain.Dhont3 <- dHont(spain.results3, spain.escanos))


#.......................................
# Filtro 2 y 3
#.......................................

spain.input4 <- dat.spain[which(apply(dat.spain[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.spain[,voting.inputs[5]]<=threshold.net.size.vote.blank),] 
dim(spain.input4)

sum(rowSums(spain.input4[,voting.inputs]))
mean(rowSums(spain.input4[,voting.inputs]))

(spain.results4<-results(spain.input4,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))

(spain.Dhont4 <- dHont(spain.results4, spain.escanos))

dif<-as.data.frame(abs(spain.results4 - matrix(rep(spain.resultados*100, each = nrow(spain.results4)), ncol = ncol(spain.results4), byrow = FALSE)))

rowSums(dif)

#.......................................
# Filtering criterion together
#.......................................
spain.input5 <- dat.spain[which(apply(dat.spain[,voting.inputs],1,sum) < quantile(apply(dat.spain[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.spain[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.spain[,voting.inputs[5]]<=threshold.net.size.vote.blank),] 
#Results
(spain.results5<-results(spain.input5,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs))
(spain.Dhont5 <- dHont(spain.results5, spain.escanos))

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
france.resultados.DHont<-c(13,30,5,6,9,13)
france.escanos <- 81

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


france.subpopulation <- c(11771902 ,17258866,23414389,3500000,2300000,199089)  
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
(france.Dhont1 <- dHont(france.results1, france.escanos))

#.......................................
# 2.Criterio de filtrado para el voto mayoritario de un grupo político 
#.......................................
france.input2 <- dat.france[which(apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results

(france.results2<-results(france.input2,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs))
(france.Dhont2 <- dHont(france.results2, france.escanos))


#.......................................
# 3.Criterio de filtrado del voto en blanco inusual 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.france[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.france[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
france.input3 <- dat.france[which(dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
(france.results3<- results(france.input3,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs))
(france.Dhont3 <- dHont(france.results3, france.escanos))

#.......................................
# Filtro 2 y 3
#.......................................

france.input4 <- dat.france[which(apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

dim(france.input4)

sum(rowSums(france.input4[,voting.inputs]))
mean(rowSums(france.input4[,voting.inputs]))

(france.results4<-results(france.input4,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs))

(france.Dhont4 <- dHont(france.results4, france.escanos))

dif<-as.data.frame(abs(france.results4 - matrix(rep(france.resultados*100, each = nrow(france.results4)), ncol = ncol(france.results4), byrow = FALSE)))

rowSums(dif)

#.......................................
# Filtering criterion together
#.......................................
france.input42 <- dat.france[which(apply(dat.france[,voting.inputs],1,sum) < quantile(apply(dat.france[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 
#Results
(france.results42<- results(france.input42,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs))
(france.Dhont42 <- dHont(france.results42, france.escanos))
dif2<-as.data.frame(abs(france.results42 - matrix(rep(france.resultados*100, each = nrow(france.results4)), ncol = ncol(france.results4), byrow = FALSE)))

rowSums(dif2)

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
italy.resultados.DHont<-c(24,21,8,8,8,0)
italy.escanos <- 76


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

#italy.subpopulation<- c(18-34,35-54,+55,autonomos,desempleados,medicos)  
#autonomos    #https://www.istat.it/it/files/2024/03/GENNAIO-2024-CS-Occupati-e-disoccupati.pdf
#medicos      # https://www.statista.com/statistics/462075/physicians-employment-in-italy/
#desempleados #https://tradingeconomics.com/italy/unemployment-rate
#poblacion    #https://demo.istat.it/app/?i=POS&l=it


italy.subpopulation <- c(10388946,16182560,23489719,5045000,1588000,250813)  
italy.total <- 50061225 
names(italy.subpopulation) <- control.inputs

#.......................................
# Raw Data
#.......................................

results(dat.france,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs)
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
(italy.Dhont1 <- dHont(italy.results1, italy.escanos))

#.......................................
# 2.Criterio de filtrado para el voto mayoritario de un grupo político 
#.......................................
italy.input2 <- dat.italy[which(apply(dat.italy[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results
(italy.results2<-results(italy.input2,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont2 <- dHont(italy.results2, italy.escanos))


#.......................................
# 3.Criterio de filtrado del voto en blanco inusual 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.italy[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.italy[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
italy.input3 <- dat.italy[which(dat.italy[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
(italy.results3<-results(italy.input3,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont3 <- dHont(italy.results3, italy.escanos))

#.......................................
# Filtro 2 y 3
#.......................................

italy.input4 <- dat.italy[which(apply(dat.italy[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.italy[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

dim(italy.input4)

sum(rowSums(italy.input4[,voting.inputs]))
mean(rowSums(italy.input4[,voting.inputs]))

(italy.results4<-results(italy.input4,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont4 <- dHont(italy.results4, italy.escanos))

dif<-as.data.frame(abs(italy.results4 - matrix(rep(italy.resultados*100, each = nrow(italy.results4)), ncol = ncol(italy.results4), byrow = FALSE)))

rowSums(dif)

#.......................................
# Filtering criterion together
#.......................................
italy.input42 <- dat.italy[which(apply(dat.italy[,voting.inputs],1,sum) < quantile(apply(dat.italy[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.italy[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.italy[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 
#Results
(italy.results42<-results(italy.input42,italy.subpopulation,italy.total,italy.resultados,voting.inputs,control.inputs = control.inputs))
(italy.Dhont42 <- dHont(italy.results42, italy.escanos))

dif2<-as.data.frame(abs(italy.results42 - matrix(rep(italy.resultados*100, each = nrow(italy.results4)), ncol = ncol(italy.results4), byrow = FALSE)))

rowSums(dif2)


