# Coursework

View(customers.red)
attach(customers.red)

#####################

# statistiche descrittive della variabile "pricepromo" per genere:
tapply(pricepromo, gender, summary)
tapply(pricepromo, gender, sd)
# statistiche descrittive della variabile "pricepromo" per stato coniugale:
tapply(pricepromo, married, summary)
tapply(pricepromo, married, sd)
# statistiche descrittive della variabile "pricepromo" stratificata per genere e stato coniugale:
des_price <- tapply(pricepromo,list(gender,married), summary)
des_price[c(1,2,3,4)] # 1 = femmina non sposata; 2 = maschio non sposato; 3 = femmina sposata; 4 = maschio sposato;
tapply(pricepromo,list(gender,married),sd)

# statistiche descrittive della variabile "retailpromo" per genere:
tapply(retailpromo, gender, summary)
tapply(retailpromo, gender, sd)
# statistiche descrittive della variabile "retailpromo" per stato coniugale:
tapply(retailpromo, married, summary)
tapply(retailpromo, married, sd)
# statistiche descrittive della variabile "retailpromo" stratificata per genere e stato coniugale:
des_retail <- tapply(retailpromo,list(gender,married), summary)
des_retail[c(1,2,3,4)] # 1 = femmina non sposata; 2 = maschio non sposato; 3 = femmina sposata; 4 = maschio sposato; 
tapply(retailpromo,list(gender,married),sd)

# statistiche descrittive della variabile "catalogpromo" per genere:
tapply(catalogpromo, gender, summary)
tapply(catalogpromo, gender, sd)
# statistiche descrittive della variabile "catalogpromo" per stato coniugale:
tapply(catalogpromo, married, summary)
tapply(catalogpromo, married, sd)
# statistiche descrittive della variabile "catalogpromo" stratificata per genere e stato coniugale:
des_catalog <- tapply(catalogpromo,list(gender,married), summary)
des_catalog[c(1,2,3,4)] # 1 = femmina non sposata; 2 = maschio non sposato; 3 = femmina sposata; 4 = maschio sposato; 
tapply(catalogpromo,list(gender,married),sd)

# statistiche descrittive della variabile "item" per genere:
tapply(item, gender, summary)
tapply(item, gender, sd)
# statistiche descrittive della variabile "item" per stato coniugale:
tapply(item, married, summary)
tapply(item, married, sd)
# statistiche descrittive della variabile "item" stratificata per genere e stato coniugale:
des_item <- tapply(item,list(gender,married), summary)
des_item[c(1,2,3,4)] # 1 = femmina non sposata; 2 = maschio non sposato; 3 = femmina sposata; 4 = maschio sposato; 
tapply(item,list(gender,married),sd)

# statistiche descrittive della variabile "income" per genere:
tapply(income, gender, summary)
tapply(income, gender, sd)
# statistiche descrittive della variabile "income" per stato coniugale:
tapply(income, married, summary)
tapply(income, married, sd)
# statistiche descrittive della variabile "income" stratificata per genere e stato coniugale:
des_income <- tapply(income,list(gender,married), summary)
des_income[c(1,2,3,4)] # 1 = femmina non sposata; 2 = maschio non sposato; 3 = femmina sposata; 4 = maschio sposato; 
tapply(income,list(gender,married),sd)

####################

# Mostrare graficamente l'andamento temporale del numero degli oggetti acquistati.
  
gm <- (paste(gender, married, sep = ""))  
  
library(ggplot2)

ggplot(customers.red, aes(x=monthnumb, y=item, fill=gm)) + 
  geom_bar(stat="identity") +
  labs(subtitle="Andamento temporale del numero di oggetti acquistati",
       y="Numero di oggetti acquistati", x="Tempo (2 anni)",
       title="Grafico a barre",
       caption = "Fonte: customers.red")

###################

# Identificare potenziali relazioni tra il numero degli oggetti acquistati e "pricepromo", "retailpromo", "catalogpromo".
  
cor.test(pricepromo, item)
cor.test(retailpromo, item)
cor.test(catalogpromo, item)

library(ggcorrplot)

dat <- customers.red[, c(3:6)]
corr <- round(cor(dat), 2)
ggcorrplot(corr, 
           type = "lower", 
           lab = T, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogramma",
           ggtheme=theme_bw)

###################

# Dopo aver reso binaria la variabile "item", creando una nuova variabile che assuma valore 1 se
# compro e 0 altrimenti, identificare potenziali relazioni tra il comprare/non comprare e "pricepromo",
# "retailpromo", "catalogpromo".  

new_item <- item
new_item[which(new_item>1)]<-1

cor.test(pricepromo, new_item)
cor.test(retailpromo, new_item)
cor.test(catalogpromo, new_item)

customers.red <- cbind(customers.red, new_item)

# library(ggcorrplot)
dat2 <- customers.red[, c(3:5, 10)]
corr2 <- round(cor(dat2), 2)
ggcorrplot(corr2, 
           type = "lower", 
           lab = T, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogramma",
           ggtheme=theme_bw)

####################

# Scrivere una funzione per generare i dati dal seguente modello di regressione
# . y = 0.5 + 2.5 * x + errore, con X~N(0;0.5) ed errore ~N(0;0.25).
  
generatore <- function()
{
  #set.seed() 
  x <- rnorm(100, 0, 0.5)
  e = rnorm(100, 0, 0.25)
  y = 0.5 + 2.5 * x + e
  plot(x, y)
  data <- data.frame(x, y)
  return(head(data))  
}

generatore()

#######################

# Scrivere una funzione che abbia come argomenti la numerosità campionaria, i parametri della retta di regressione
# e il numero di campioni da simulare (fissato a 1000) e che ritorni come output la distorsione e l'errore quadratico
# medio delle stime.
    
library(Metrics)  
  
bias_mse <- function(b0, b1, campioni=1000)
{
  #set.seed()
  x = rnorm(campioni)
  e = rnorm(campioni)
  y = b0 + b1 * x + e
  actual <- y
  mod <- lm(y~x)
  predicted <- predict(mod)
  bias <- bias(actual, predicted) # distorsione
  mse <- mean((actual-predicted)^2) # errore quadratico medio
  var_residui <- var(mod$residuals) # varianza dei residui
  return(cbind(bias, mse, var_residui))
}

bias_mse(0.5, 2.5)
