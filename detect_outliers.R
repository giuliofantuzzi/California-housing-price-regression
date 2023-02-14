detect_outliers<- function(v){
    quantiles= quantile(v, names=F)
    W= quantiles[4]-quantiles[2]
    min= quantiles[2]- 1.5*W
    max= quantiles[4]+ 1.5*W
    return(v< min | v > max)
}

data<- read.csv("data/housing.csv", header=T)
data$total_bedrooms[which(is.na(data$total_bedrooms))]<- mean(data$total_bedrooms, na.rm=TRUE)

sum(detect_outliers(data$longitude))
sum(detect_outliers(data$latitude))
sum(detect_outliers(data$housing_median_age))

#nuove variabili
data$rooms_per_household<- data$total_rooms / data$households
data$bedrooms__totroom<- data$total_bedrooms / data$total_rooms
data$pop_per_household<- data$population / data$households
data<- data[,-which(colnames(data)=="population")]
data<- data[,-which(colnames(data)=="households")]
data<- data[,-which(colnames(data)=="total_rooms")]
data<- data[,-which(colnames(data)=="total_bedrooms")]


sum(detect_outliers(data$median_income))#681
sum(detect_outliers(data$rooms_per_household))#511
sum(detect_outliers(data$rooms_per_household))#657
sum(detect_outliers(data$pop_per_household)) #711

index.rm<- detect_outliers(data$median_income) | detect_outliers(data$rooms_per_household) |detect_outliers(data$rooms_per_household) | detect_outliers(data$pop_per_household)

data<- data[-index.rm,]


#Ora proviamo a costruire il modello e vediamo se migliora
#fit=lm(log(median_house_value) ~ log(housing_median_age) + 
           #log(median_income) + log(rooms_per_household) + log(bedrooms__totroom) + 
           #log(pop_per_household)+ocean_proximity, data = data)

#summary(fit)

#togliamo anche le Y outliers
data<- data[-detect_outliers(data$median_house_value),]
#fit1=lm(log(median_house_value) ~ log(housing_median_age) + 
           #log(median_income) + log(rooms_per_household) + log(bedrooms__totroom) + 
           #log(pop_per_household)+ocean_proximity, data = data)
#summary(fit1)


#vediamo se con train test cambia qualcosa
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
train  <- data[sample, ]
test   <- data[!sample, ]

#Ora costruisco il modello sul train
fit=lm(log(median_house_value) ~ latitude+longitude+log(housing_median_age) + 
           log(median_income) + log(bedrooms__totroom) + log(rooms_per_household) + 
           log(pop_per_household), data = train)
summary(fit) #0.6684

fit1= lm(log(median_house_value) ~ log(housing_median_age) + 
             log(median_income) + log(bedrooms__totroom) + 
             log(pop_per_household)+ocean_proximity, data = train)
summary(fit1) #0.6687

#Analisi dei residui
#NB: ricordo che approx i res studentizzati sono normale standard
hist(rstandard(fit), breaks=100, freq = F)
curve(dnorm(x,mean=0,sd=1),add=T,col="red") #qua non sembra male
qqnorm(rstandard(fit))
qqline(rstandard(fit)) #qui invece si vedono problemi alle code

shapiro.test(rstandard(fit)) #per verifica normalità...ma ho troppi dati per questo test
#un'alternativa è il test di andersen
install.packages("nortest")
library(nortest)
ad.test(rstandard(fit)) #rifiuterebbe normalità
#verifica omoschedasticità?

MSQ_fit= sqrt(mean((fitted(fit)-log(train$median_house_value))^2))
MSQ_fit1= sqrt(mean((fitted(fit1)-log(train$median_house_value))^2))

#Per avere un'idea con l'unità di misura
MAE_fit=mean(abs(exp(fitted(fit))-train$median_house_value)) #--->errore di 46823 dollari
MAE_fit1=mean(abs(exp(fitted(fit1))-train$median_house_value))#--->errore di 48002 dollari
#NB: qui in realtà c'è ancora un errorino...gli NA iniziali dovevo sostituirli con la media del TRAIN, non di tutto (sennò è barare)
         