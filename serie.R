chemin=choose.files()
data=read.csv(chemin)

is.ts(data)
smp = ts(data = data[,2],start =c(1985,1),end = c(2018,1),frequency = 12)
ts.plot(smp)



dcp = decompose(smp)
plot(dcp)

saisonnalite=decompose(smp,type="additive")$seasonal
plot(saisonnalite)


lag.plot(rev(smp),15,layout=c(4,4),do.lines=FALSE,diag.col="red",col.main="blue")
frequency(smp)

tendance=decompose(smp,type="additive")$trend 
plot(tendance)


require(tseries)
require(caschrono)

adf.test(smp) #p-value<0.05 d=0 => on rejete H0 (series est stationnaire)


smpDiff=diff(smp,12) #D=1
plot.ts(smpDiff)
plot(decompose(smpDiff))

par(mfrow=c(1,2))
acf(smpDiff) #q=3 ; Q = 2
pacf(smpDiff,lag.max = 66) #p=3 ; P = 5



#choisir le meilleur model


# Définir les paramètres
p <- 3
q <- 3
P <- 5
Q <- 2

# Créer une vecteur vide pour stocker les AIC valeurs 
aic_values <- numeric()

for (i in 0:p) {
  for (k in 0:q) {
    for (l in 0:P) {
      for (n in 0:Q) {
        # Adapter le modèle SARIMA avec tryCatch
        model <- tryCatch(
          arima(smp, order = c(i, 0, k), seasonal = list(order = c(l, 1, n), period = 12), method = "ML", include.mean = FALSE),
          error = function(e) NULL
        )
        
        # Vérifier si le modèle a convergé avec succès
        if (!is.null(model)) {
          # Stocker l'AIC dans le vecteur
          aic_values <- c(aic_values, AIC(model))
          
          # Afficher les paramètres et l'AIC pour chaque modèle
          print(paste("AIC pour les paramètres", i, 0, k, l, 1, n, ":", AIC(model)))
        } else {
          # Afficher un message si le modèle n'a pas convergé
          print(paste("Le modèle n'a pas convergé pour les paramètres", i, 0, k, l, 1, n))
        }
      }
    }
  }
}


#Trouver le minimum
best_model_index <- which.min(aic_values)
best_model_aic <- min(aic_values)


print(paste("Le meilleur modèle est le modèle avec les paramètres", best_model_index, "et AIC =", best_model_aic))



model = arima(smp, order = c(2, 0, 0), seasonal = list(order = c(2, 1, 2), period = 12), method = "ML", include.mean = F)
model$coef
model$aic

t_stat(model)


if (!require(lmtest)) {
  install.packages("lmtest")
  library(lmtest)
}
 


shapiro.test(residuals(model))

acf(residuals(model))

pacf(residuals(model))


Box.test.2(residuals(model),nlag=10,type = "Box-Pierce")



library(forecast) 
plot(forecast(model,h=100,level = 95))
forecast(model)



