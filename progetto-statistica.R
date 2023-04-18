setwd("C:/Users/giovanni/Documents/cartella/University/3 anno/STATISTICA APPLICATA/R projects")
dataset=read.csv("Dataset_IZ_gruppo5.csv",header=TRUE)
ds=dataset #nome alternativo per abbreviare dataset


###### ANALISI PRELIMINARE - STATISTICA DESCRITTIVA ######

#Cardinalità del campione
nrow(dataset)

#Grafici delle singole variabili del dataset
par(mfrow=c(2,3))
plot(dataset$x1_CPU)
plot(dataset$x2_HD)
plot(dataset$x3_proc)
plot(dataset$x4_aging)
plot(dataset$x5_audio)
plot(dataset$x6_RAM)
plot(dataset$y_prestazSWcalc)

#media e varianza delle variabile indipendenti
mean(dataset$x1_CPU)
mean(dataset$x2_HD)
mean(dataset$x3_proc)
mean(dataset$x4_aging)
mean(dataset$x5_audio)
mean(dataset$x6_RAM)
var(dataset$x1_CPU) #oppure sd(dataset$x1_CPU)
var(dataset$x2_HD)
var(dataset$x3_proc)
var(dataset$x4_aging)
var(dataset$x5_audio)
var(dataset$x6_RAM)
#I dati sono stati normalizzati in modo da avere media nulla e varianza 1. Si potrebbe scrivere
#che i dati sono normalizzati facendo (x-mean(c))/sd(x)

#Verifica grafica della normalità delle variabili
par(mfrow=c(3,2))
qqnorm(dataset$x1_CPU,main = "Normal Q-Q Plot della variabile X1")
abline(0,1)
qqnorm(dataset$x2_HD,main = "Normal Q-Q Plot della variabile X2")
abline(0,1)
qqnorm(dataset$x3_proc,main = "Normal Q-Q Plot della variabile X3")
abline(0,1)
qqnorm(dataset$x4_aging,main = "Normal Q-Q Plot della variabile X4")
abline(0,1)
qqnorm(dataset$x5_audio,main = "Normal Q-Q Plot della variabile X5")
abline(0,1)
qqnorm(dataset$x6_RAM,main = "Normal Q-Q Plot della variabile X6")
abline(0,1)
qqnorm(scale(dataset$y_prestazSWcalc) ,main = "Normal Q-Q Plot della variabile Y" )
abline(0,1)

#Boxplot e istogrammi delle variabili dipendenti del dataset
par(mfrow=c(2,3))
boxplot(dataset$x1_CPU)
boxplot(dataset$x2_HD)
boxplot(dataset$x3_proc)
boxplot(dataset$x4_aging)
boxplot(dataset$x5_audio)
boxplot(dataset$x6_RAM)
summary(dataset)
par(mfrow=c(2,3))
hist(dataset$x1_CPU,freq=F)
hist(dataset$x2_HD,freq=F)
hist(dataset$x3_proc,freq=F)
hist(dataset$x4_aging,freq=F)
hist(dataset$x5_audio,freq=F)
hist(dataset$x6_RAM,freq=F)

#Informazioni sulla variabile dipendente Y
par(mfrow=c(1,1))
mean(dataset$y_prestazSWcalc)
var(dataset$y_prestazSWcalc)
boxplot(dataset$y_prestazSWcalc)
summary(dataset$y_prestazSWcalc)
hist(dataset$y_prestazSWcalc,freq=F)


###### ANALISI PRELIMINARE - ANALISI DI CORRELAZIONE ###### 

#install.packages('car')
library(car)

#Scatterplot di ogni variabile: si è preferito usare il comando "scatterplot()" al comando "plot()"
scatterplot(y_prestazSWcalc ~ x1_CPU, data=dataset)
cor(dataset$x1_CPU,dataset$y_prestazSWcalc)

scatterplot(y_prestazSWcalc ~ x2_HD, data=dataset)
cor(dataset$x2_HD,dataset$y_prestazSWcalc)

scatterplot(y_prestazSWcalc ~ x3_proc, data=dataset)
cor(dataset$x3_proc,dataset$y_prestazSWcalc)

scatterplot(y_prestazSWcalc ~ x4_aging, data=dataset)
cor(dataset$x4_aging,dataset$y_prestazSWcalc)

scatterplot(y_prestazSWcalc ~ x5_audio, data=dataset)
cor(dataset$x5_audio,dataset$y_prestazSWcalc)

scatterplot(y_prestazSWcalc ~ x6_RAM, data=dataset)
cor(dataset$x6_RAM,dataset$y_prestazSWcalc)

#Modelli di regressione semplici e stima dei parametri della retta di regressione
lin_fit1=lm(ds$y_prestazSWcalc~ds$x1_CPU)
coeff = coefficients(lin_fit1)
plot(dataset$x1_CPU,dataset$y_prestazSWcalc)
abline(lin_fit1, col = "darkgreen")

lin_fit2=lm(ds$y_prestazSWcalc~ds$x2_HD)
coeff = coefficients(lin_fit2)
plot(dataset$x2_HD,dataset$y_prestazSWcalc)
abline(lin_fit2, col = "darkgreen")

lin_fit3=lm(ds$y_prestazSWcalc~ds$x3_proc)
coeff = coefficients(lin_fit3)
plot(dataset$x3_proc,dataset$y_prestazSWcalc)
abline(lin_fit3, col = "darkgreen")

lin_fit3_2=lm(ds$y_prestazSWcalc~I(ds$x3_proc^2)) #Modello con regressore al quadrato
coeff = coefficients(lin_fit3_2)
plot(dataset$x3_proc,dataset$y_prestazSWcalc)

lin_fit4=lm(ds$y_prestazSWcalc~ds$x4_aging)
coeff = coefficients(lin_fit4)
plot(dataset$x4_aging,dataset$y_prestazSWcalc)
abline(lin_fit4, col = "darkgreen")

lin_fit5=lm(ds$y_prestazSWcalc~ds$x5_audio)
coeff = coefficients(lin_fit5)
plot(dataset$x5_audio,dataset$y_prestazSWcalc)
abline(lin_fit5, col = "darkgreen")

lin_fit6=lm(ds$y_prestazSWcalc~ds$x6_RAM)
coeff = coefficients(lin_fit6)
plot(dataset$x6_RAM,dataset$y_prestazSWcalc)
abline(lin_fit6, col = "darkgreen")

#Rappresentazione grafica complessiva riguardo la correlazione fra tutte le variabili del dataset
#install.packages('corrplot')
library(corrplot)
corrplot(cor(dataset))


###### MODELLO LINEARE - 1 ######

#install.packages('matlib')
library(matlib)

#Realizzazione della Design Matrix
X=cbind(1,dataset$x1_CPU,dataset$x2_HD,dataset$x3_proc,dataset$x4_aging,dataset$x5_audio,dataset$x6_RAM)
y=as.matrix(dataset$y_prestazSWcalc)

#Vettore colonna delle stime dei parametri beta
beta_hat=inv(t(X) %*% X) %*% t(X) %*% y
print(beta_hat)

#Metodo alternativo per la stima dei parametri
lin_fit=lm(y_prestazSWcalc ~ x1_CPU + x2_HD + x3_proc + x4_aging + x5_audio + x6_RAM, data=dataset)
coef(lin_fit)

# Coefficiente di determinazione
RSS=(t(y-X%*%beta_hat)) %*% (y-X%*%beta_hat) #RSS di beta_hat
y_m=mean(y)
TSS=(sum((y-y_m)^2))
R_quadro=1-(RSS/TSS)

#Analisi dei residui - Test di specificazione

#Calcolo dei residui
residui=residuals(lin_fit)

#Test t per verificare linearità tra regressori e risposta
t.test(residui)
plot(lin_fit) #il primo grafico è utile per la verifica grafica

#Test di Shapiro-Wilk: verifichiamo la normalità della distribuzione degli errori
shapiro=shapiro.test(residui) 
print(shapiro)
qqnorm(scale(residui)) #verifica grafica. I residui vanno scalati a mu=0 e sd=1 per confrontarli con normale std
abline(0,1)

#memorizziamo la formula del modello in un oggetto per facilità di manipolazione
#install.packages('lmtest')
library(lmtest)
modello=formula(lin_fit) 

#test di Breusch-Pagan per verificare l'omoschedasticità
testbp=bptest(modello,data=dataset) 
print(testbp)

#Verfica grafica dell'omoschedacità
plot(fitted(lin_fit),lin_fit$residuals, ylab="Residui", xlab="Fitted", main="Residui vs Fitted")
abline(h=0)

#Test di Durbin-Watson per l'indipendenza degli errori
dw=dwtest(modello,data=dataset) 
print(dw)

#Diagnostica - possibili outlier
standard_res=rstandard(lin_fit) #calcola i residui standardizzati
y_hat = predict(lin_fit,dataset) #stime di y
plot(y_hat,standard_res, ylab="Residui standardizzati", xlab="Stime di yi", main="Possibili Outliers",ylim = c(-5,5))
abline(3,0,col="blue")
abline(-3,0,col="blue") 

#Intervalli di confidenza sui coefficienti
n=nrow(dataset) #numero di campioni
p=6
alpha=0.05 #livello di rischio
t_score=qt(p=alpha/2,df=(n-1),lower.tail=F) #quantile
bounds=matrix(ncol=2,nrow=p+1) #matrice che conterrà i limiti
colnames(bounds)=c("lower","upper")
rownames(bounds)=c("beta_0","beta_1","beta_2","beta_3","beta_4","beta_5","beta_6")
v=c()
sigma_2_hat=RSS/(n-p-1) #stima della varianza
sigma_hat=sqrt(sigma_2_hat) #stima della deviazione standard
inv_Xt_X=inv(t(X)%*%X) #(X^T*X)^-1
for(i in 1:(p+1)){
  v[i]=inv_Xt_X[i,i]
  bounds[i,1]=beta_hat[i]-sigma_hat*sqrt(v[i])*t_score
  bounds[i,2]=beta_hat[i]+sigma_hat*sqrt(v[i])*t_score
}
print(bounds)
confint(lin_fit,level=0.95)
confint(lin_fit,level=0.99)

# Test d'ipotesi sui coefficienti
n=nrow(dataset) #numero di campioni
p=6
alpha=0.05 #rischio di prima specie
t_score=qt(p=alpha/2,df=(n-1),lower.tail=F) #quantile
bounds=matrix(ncol=2,nrow=p+1) #matrice che conterrà i limiti di I_AC
v=c()
sigma_2_hat=RSS/(n-p-1) #stima della varianza
sigma_hat=sqrt(sigma_2_hat) #stima della deviazione standard
inv_Xt_X=inv(t(X)%*%X) #(X^T*X)^-1
for(i in 1:(p+1)){
  v[i]=inv_Xt_X[i,i]
  bounds[i,1]=-sigma_hat*sqrt(v[i])*t_score
  bounds[i,2]=sigma_hat*sqrt(v[i])*t_score
}
for(i in 1:(p+1)){
  if(beta_hat[i]<bounds[i,1] || beta_hat[i]>bounds[i,2]){
    print(paste("beta_", i-1, ": H_0 rifiutata",sep=""))
  } else {
    print(paste("beta_", i-1, ": H_0 accettata",sep=""))
  }
}
summary(lin_fit)

#Ricaviamo il modello semplificato
lin_fit_2=lm(y_prestazSWcalc ~ x1_CPU + x3_proc + x4_aging + x6_RAM, data=dataset)
summary(lin_fit_2)

# Analisi del modello semplificato
plot(lin_fit_2) #verifica linearità
t.test(lin_fit_2$residuals)
shapiro.test(lin_fit_2$residuals) #verifica normalità
qqnorm(scale(lin_fit_2$residuals))
abline(0,1)
bptest(formula(lin_fit_2),data=dataset) #verifica omoschedasticità
plot(fitted(lin_fit_2),lin_fit_2$residuals, ylab="Residui", xlab="Fitted", main="Residui vs Fitted")
abline(h=0)
dwtest(formula(lin_fit_2),data=dataset) #verifica indipendenza residui
plot(fitted(lin_fit_2),rstandard(lin_fit_2), ylab="Residui standardizzati", xlab="Stime di yi", main="Possibili Outliers",ylim = c(-5,5)) #verifica outiler
abline(3,0,col="blue")
abline(-3,0,col="blue")


######  MODELLO CON REGRESSORI X^2 - 2 ######

#Stepwise Forward
poly_fit=vector(mode = "list", length = 6)
base_model=lin_fit_2
poly_fit[[1]]=update(base_model,.~.+I(x1_CPU^2))
poly_fit[[2]]=update(base_model,.~.+I(x2_HD^2))
poly_fit[[3]]=update(base_model,.~.+I(x3_proc^2))
poly_fit[[4]]=update(base_model,.~.+I(x4_aging^2))
poly_fit[[5]]=update(base_model,.~.+I(x5_audio^2))
poly_fit[[6]]=update(base_model,.~.+I(x6_RAM^2))
t.values=c()
for(i in 1:6){
  t.values[i]=summary(poly_fit[[i]])[["coefficients"]][6,"t value"]
}
which.max(abs(t.values)) #criterio del minimo BIC
summary(poly_fit[[3]]) #modello accettabile

#Verifichiamo qual è il prossimo regressore da aggiungere
base_model=poly_fit[[3]]
poly_fit=vector(mode = "list", length = 5)
poly_fit[[1]]=update(base_model,.~.+I(x1_CPU^2))
poly_fit[[2]]=update(base_model,.~.+I(x2_HD^2))
poly_fit[[3]]=update(base_model,.~.+I(x4_aging^2))
poly_fit[[4]]=update(base_model,.~.+I(x5_audio^2))
poly_fit[[5]]=update(base_model,.~.+I(x6_RAM^2))
t.values=c()
for(i in 1:5){
  t.values[i]=summary(poly_fit[[i]])[["coefficients"]][7,"t value"]
}
which.max(abs(t.values)) #criterio del minimo BIC
summary(poly_fit[[4]]) #Questo modello non accettabile
#Fine della Stepwise Forward

#Il modello accettato è 
poly_fit=base_model

#Intervalli di confidenza del modello
confint(poly_fit,level=0.95)


#Analisi del modello
plot(poly_fit) #verifica linearità
t.test(poly_fit$residuals)
shapiro.test(poly_fit$residuals) #verifica normalità
qqnorm(scale(poly_fit$residuals))
abline(0,1)
bptest(formula(poly_fit),data=dataset) #verifica omoschedasticità
plot(fitted(poly_fit),poly_fit$residuals, ylab="Residui", xlab="Fitted", main="Residui vs Fitted")
abline(h=0)
dwtest(formula(poly_fit),data=dataset) #verifica indipendenza residui
plot(fitted(poly_fit),rstandard(poly_fit), ylab="Residui standardizzati", xlab="Stime di yi", main="Possibili Outliers",ylim = c(-5,5)) #verifica outiler
abline(3,0,col="blue")
abline(-3,0,col="blue")


###### MODELLO CON TERMINI DI INTERAZIONE - 3 ######

#install.packages('leaps')
library(leaps)

#Approccio Best Subsect Selection: consideriamo i regressori lineari, quadratici e incrociati
bss_1=regsubsets(y_prestazSWcalc ~ x1_CPU + x2_HD + x3_proc + x4_aging + x5_audio + x6_RAM
               + I(x1_CPU^2) + I(x2_HD^2) + I(x3_proc^2) + I(x4_aging^2) + I(x5_audio^2) + I(x6_RAM^2)
               + (.)^2
               , data = ds, nvmax = 10)
sum_bss_1=summary(bss_1)
print(sum_bss_1)
which.min(sum_bss_1$bic)

#memorizzo in bss_fit_1 il modello con il BIC minimo
bss_fit_1=lm(y_prestazSWcalc ~ x1_CPU + x3_proc + x4_aging + x6_RAM + I(x3_proc^2) + x2_HD:x4_aging,data=ds)
sum_bss_fit_1=summary(bss_fit_1)
print(sum_bss_fit_1)

#Intervalli di confidenza del modello
confint(bss_fit_1,level=0.95)

# Analisi del modello
plot(bss_fit_1) #verifica linearità
t.test(bss_fit_1$residuals)
shapiro.test(bss_fit_1$residuals) #verifica normalità
qqnorm(scale(bss_fit_1$residuals))
abline(0,1)
bptest(formula(bss_fit_1),data=dataset) #verifica omoschedasticità
plot(fitted(bss_fit_1),bss_fit_1$residuals, ylab="Residui", xlab="Fitted", main="Residui vs Fitted")
abline(h=0)
dwtest(formula(bss_fit_1),data=dataset) #verifica indipendenza residui
plot(fitted(bss_fit_1),rstandard(bss_fit_1), ylab="Residui standardizzati", xlab="Stime di yi", main="Possibili Outliers",ylim = c(-5,5)) #verifica outiler
abline(3,0,col="blue")
abline(-3,0,col="blue")


##### MODELLO CON ESPONENZIALI - 4 #####

#Approccio Best Subsect Selection: consideriamo i regressori lineari, quadratici, incrociati, cubici e esponenziali
bss_2=regsubsets(y_prestazSWcalc ~ x1_CPU + x2_HD + x3_proc + x4_aging + x5_audio + x6_RAM
                 + I(x1_CPU^2) + I(x2_HD^2) + I(x3_proc^2) + I(x4_aging^2) + I(x5_audio^2) + I(x6_RAM^2)
                 + I(x1_CPU^3) + I(x2_HD^3) + I(x3_proc^3) + I(x4_aging^3) + I(x5_audio^3) + I(x6_RAM^3)
                 + (.)^2
                 + I(exp(-x4_aging)) + I(exp(-x1_CPU)) + I(exp(-x2_HD)) + I(exp(-x3_proc)) + I(exp(-x5_audio)) + I(exp(-x6_RAM))
                 + I(exp(x4_aging)) + I(exp(x1_CPU)) + I(exp(x2_HD)) + I(exp(x3_proc)) + I(exp(x5_audio)) + I(exp(x6_RAM))
                 , data = ds, nvmax = 10)
sum_bss_2=summary(bss_2)
print(sum_bss_2)
which.min(sum_bss_2$bic)

#memorizzo in bss_fit_2 il modello con il BIC minimo
bss_fit_2=lm(y_prestazSWcalc ~ x1_CPU + x4_aging + x6_RAM + I(x3_proc^2) + I(exp(x3_proc)) + x2_HD:x4_aging,data=ds)
sum_bss_fit_2=summary(bss_fit_2)
print(sum_bss_fit_2)

#Intervalli di confidenza del modello
confint(bss_fit_2,level=0.95)

# Analisi del modello
plot(bss_fit_2) #verifica linearità
t.test(bss_fit_2$residuals)
shapiro.test(bss_fit_2$residuals) #verifica normalità
qqnorm(scale(bss_fit_2$residuals))
abline(0,1)
bptest(formula(bss_fit_2),data=dataset) #verifica omoschedasticità
plot(fitted(bss_fit_2),bss_fit_2$residuals, ylab="Residui", xlab="Fitted", main="Residui vs Fitted")
abline(h=0)
dwtest(formula(bss_fit_2),data=dataset) #verifica indipendenza residui
plot(fitted(bss_fit_2),rstandard(bss_fit_2), ylab="Residui standardizzati", xlab="Stime di yi", main="Possibili Outliers",ylim = c(-5,5)) #verifica outiler
abline(3,0,col="blue")
abline(-3,0,col="blue")




##### Considerazione Conclusiva  #####
bss=regsubsets(y_prestazSWcalc ~ x1_CPU + x2_HD + x3_proc + x4_aging + x5_audio + x6_RAM
                 + I(x1_CPU^2) + I(x2_HD^2) + I(x3_proc^2) + I(x4_aging^2) + I(x5_audio^2) + I(x6_RAM^2)
                 + I(exp(-x4_aging)) + I(exp(-x1_CPU)) + I(exp(-x2_HD)) + I(exp(-x3_proc)) + I(exp(-x5_audio)) + I(exp(-x6_RAM))
                 + I(exp(x4_aging)) + I(exp(x1_CPU)) + I(exp(x2_HD)) + I(exp(x3_proc)) + I(exp(x5_audio)) + I(exp(x6_RAM))
                 + (.)^2
                 + x1_CPU:x4_aging:x6_RAM
                 + x4_aging:x5_audio:x6_RAM 
                 + x1_CPU:x3_proc + x2_HD:x4_aging
               , data = ds, nvmax = 10,really.big = T)

summary(bss)
