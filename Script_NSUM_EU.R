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
  MoS.degrees <- total*rowMeans(dat.nsum[,control.inputs[1:3]]/subpopulation.sizes[1:3])
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
  Summary <- data.frame(Naive = naive.results, Naive2 = naive2.results,MoS = MoS.results,RoS = RoS.results,MLE=mle.results,PIMLE=pimle.results,Reales=resultados.reales)
  return(t(Summary))
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

#############################################################################################################################################
#SPAIN
#############################################################################################################################################
#Import dataset into R (code used for importing the data)
#.......................................

Pollfish_Survey_Elecciones_Europeas_es <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/UNIVERSIDAD/TFG/Data/Pollfish_Survey_Elecciones_europeas_Espana_389886097_es.xlsx")
df.spain <- as.data.frame(Pollfish_Survey_Elecciones_Europeas_es[,-3]) # The column with the permission is removed
dim(df.spain)
colnames(df.spain) <- c("Gender","Year.Birth","PP","PSOE","Vox","Sumar","UP","Ahora.Republicas","Blanco","NoSabe","edad.18.34","edad.35.54","edad.mas.54","autonomo","desempleado","medico","Education","Employment.Status","Income")
spain.resultados <- c(0.3421,0.3019,0.0963,0.0467,0.033,0.0491,0.007)

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
#Criterion for retaining the respondents with a network under a specific threshold
#.......................................

threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 
spain.input1<- dat.spain[which(apply(dat.spain[,voting.inputs],1,sum)<quantile(apply(dat.spain[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)),]

#Results
results(spain.input1,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs = control.inputs)

#.......................................
# Filtering criterion for a political group majoritary voting 
#.......................................
spain.input2 <- dat.spain[which(apply(dat.spain[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results
results(spain.input2,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs)

#.......................................
# Filtering criterion of unusual blank vote 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.spain[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.spain[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
spain.input3 <- dat.spain[which(dat.spain[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
results(spain.input3,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs)

#.......................................
# Filtering criterion together
#.......................................
spain.input4 <- dat.spain[which(apply(dat.spain[,voting.inputs],1,sum) < quantile(apply(dat.spain[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.spain[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.95 & dat.spain[,voting.inputs[5]]<=threshold.net.size.vote.blank),] 
#Results
results(spain.input4,spain.subpopulation,spain.total,spain.resultados,voting.inputs,control.inputs)



#############################################################################################################################################
#FRANCE
#############################################################################################################################################
#Import dataset into R (code used for importing the data)
#.......................................

Pollfish_Survey_Elecciones_Europeas_fr <- read_excel("/Users/paulablazquez/Library/Mobile Documents/com~apple~CloudDocs/UNIVERSIDAD/TFG/Data/Pollfish_Survey_Elections_au_Parlement_europeen_de_2024_389899575_fr.xlsx")
df.france <- as.data.frame(Pollfish_Survey_Elecciones_Europeas_fr[,-3]) # The column with the permission is removed
dim(df.france)
colnames(df.france) <- c("Gender","Year.Birth","BdE","La France revient","LE","LR","LFI","RlE","Blanco","NoSabe","edad.18.34","edad.35.54","edad.mas.54","autonomo","desempleado","medico","Education","Employment.Status","Income")
france.resultados <- c(0.1460,0.3137,0.055,0.0725,0.0989,0.1383,0.0137)

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
#Identification of outlying observations
#.......................................

# Encuestas que solo respondan su votacion

#.......................................
#Criterion for retaining the respondents with a network under a specific threshold
#.......................................

threshold.net.size.vote.intention <- 0.80 #We can study the thresholds: 0.80,0.90,0.95 
france.input1<- dat.france[which(apply(dat.france[,voting.inputs],1,sum)<quantile(apply(dat.france[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)),]

#Results
results(france.input1,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs)

#.......................................
# Filtering criterion for a political group majoritary voting 
#.......................................
france.input2 <- dat.france[which(apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95),] 

#Results
results(france.input2,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs)

#.......................................
# Filtering criterion of unusual blank vote 
#.......................................
outliers.vote.blank.flag <- is_mad_outlier(dat.france[,voting.inputs[7]],5) #We can study the thresholds: 2, 3 and 5 (the defauls value is 5) 
threshold.net.size.vote.blank <- max(dat.france[!outliers.vote.blank.flag,voting.inputs[7]]) # Calculate it from the MAD
france.input3 <- dat.france[which(dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 

#Results
results(france.input3,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs)

#.......................................
# Filtering criterion together
#.......................................
france.input4 <- dat.france[which(apply(dat.france[,voting.inputs],1,sum) < quantile(apply(dat.france[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat.france[,voting.inputs[1:6]],1,function(x) max(prop.table(x)))<=0.95 & dat.france[,voting.inputs[7]]<=threshold.net.size.vote.blank),] 
#Results
results(france.input4,france.subpopulation,france.total,france.resultados,voting.inputs,control.inputs = control.inputs)

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

