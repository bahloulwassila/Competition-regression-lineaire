##### CODE COMPETITION


# library
library(tidyverse)
library(olsrr)
library(car)


# rpertoire
#setwd("C:/Users/wassi/OneDrive - Universite de Montreal/semestre4/STT2400/devoirs/Competition/price.csv") # ? changer.

# importation du fichier
donnee <- read.csv(file = "C:/Users/wassi/OneDrive - Universite de Montreal/semestre4/STT2400/devoirs/Competition/price.csv", header = T)

# Analyse du fichier
str(donnee)
summary(donnee)

# Traitement des donn?es manquantes
na_by_col <- apply(donnee, 2, function(x) sum(is.na(x)))  # Pour voir les na par colonnes.
na_by_col
var_select <- names(na_by_col[na_by_col<0.2*nrow(donnee)]) # on choisit les variables 
# avec au plus 20% de NA
# les variables exclues sont : 
names(na_by_col[na_by_col>=0.2*nrow(donnee)])

donnee_propre <- donnee[,var_select] # nouvelle base avec les variables s?lectionn?es.

####### Imputation des valeurs manquantes -------

# Les variables num?riques sont imput?es par la moyenne et
# les vvariables qualitatives par leur mode.

# Fonction pour calculer le mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# L'imputation
for (col in names(donnee_propre)) {
  if (is.numeric(donnee_propre[,col])) {
    # Remplacer NA par la moyenne pour les colonnes num?riques
    mean_value <- mean(donnee_propre[,col], na.rm = TRUE)
    donnee_propre[,col][is.na(donnee_propre[,col])] <- mean_value
  } else if (is.character(donnee_propre[[col]])) {
    # Remplacer NA par le mode pour les colonnes de caract?res
    mode_value <- get_mode(donnee_propre[,col][!is.na(donnee_propre[,col])])
    donnee_propre[,col][is.na(donnee_propre[,col])] <- mode_value
  }
}

# On v?rifie s'il y a encore des NA 
apply(donnee_propre, 2, function(x) sum(is.na(x)))
# Plus de donn?es manquantes

###### Division en train-test  --------------
##donne propre à transformer en facteurs
donnee_propre <- data.frame(lapply(donnee_propre, function(x) if(is.character(x)) as.factor(x) else x))
set.seed(12)
num_select <- sample( (1:nrow(donnee_propre)), 0.8*nrow(donnee_propre), replace = F)
data_train <- donnee_propre[num_select,]
data_test <- donnee_propre[-num_select,]

hist(data_train$SalePrice, main = "Distribution of SalePrice", xlab = "SalePrice", breaks = 30, col = "lightblue")

###### Analyse de la variable d?pendante  ----------
plot(data_train$SalePrice, xlab="maison", ylab="prix des maisons", pch =19)
# Il semble y avoir deux ou trois valeurs atypiques.

qqnorm(data_train$SalePrice) # Les donn?es ne sont pas normales
qqline(data_train$SalePrice)


# La m?thode BoxCOX indiquent qu'une transformation avec le log est indiqu?.
boxCox(lm(data_train$SalePrice~1))

lambda_opt <- boxcox_trans$x[which.max(boxcox_trans$y)]
data_train$BoxCox_SalePrice <- (data_train$SalePrice^lambda_opt - 1) / lambda_opt

qqnorm(log(data_train$SalePrice)) # transformation log rendent les donn?es du prix normales
qqline(log(data_train$SalePrice))

data_train$logSalePrice <- log(data_train$SalePrice) # ajout ? la base de donn?es
data_test$logSalePrice <- log(data_test$SalePrice)
hist(data_train$BoxCox_SalePrice, main = "Distribution of Box-Cox Transformed SalePrice", xlab = "BoxCox_SalePrice", breaks = 30, col = "lightgreen")

###### Analyse statistiques
# Corr?lation entre les variables quantitatives et la variables d?pendantes.
numeric_columns <- data_train[sapply(data_train, is.numeric)]
cor(numeric_columns)["logSalePrice",]
# La variable "OverallQual" est la plus correl? ? la variable d?pendante.

character_columns <- data_train[sapply(data_train, is.factor)]
for(i in 1:dim(character_columns)[2]) {
  print(names(character_columns)[i])
  print(table(character_columns[,i]))
}
table(donnee_propre$Utilities)

# Choix de mod?le
full_model <- lm(logSalePrice~., data=data_train[,!(names(data_train) %in% c("Id", "SalePrice", "Utilities"))])
summary(full_model)

#stepwise_model <- ols_step_both_p(full_model)
#summary(stepwise_model$model)
#AIC
aic_select <- ols_step_backward_aic(full_model)
summary(aic_select$model)
aic_select$predictors
print(aic_select)


donnee_soumission <- read.csv(file = "C:/Users/wassi/OneDrive - Universite de Montreal/semestre4/STT2400/devoirs/Competition/base_price_predict.csv", header = T)

# Analyse du fichier
str(donnee_soumission)
summary(donnee_soumission)

# Traitement des donn?es manquantes
na_by_col <- apply(donnee_soumission, 2, function(x) sum(is.na(x)))  # Pour voir les na par colonnes.
na_by_col
var_select <- names(na_by_col[na_by_col<0.2*nrow(donnee_soumission)]) # on choisit les variables 
# avec au plus 20% de NA
# les variables exclues sont : 
names(na_by_col[na_by_col>=0.2*nrow(donnee_soumission)])

donnee_soumission <- donnee_soumission[,var_select] # nouvelle base avec les variables s?lectionn?es.

####### Imputation des valeurs manquantes -------

# Les variables num?riques sont imput?es par la moyenne et
# les vvariables qualitatives par leur mode.

# Fonction pour calculer le mode
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# L'imputation
for (col in names(donnee_soumission)) {
  if (is.numeric(donnee_soumission[,col])) {
    # Remplacer NA par la moyenne pour les colonnes num?riques
    mean_value <- mean(donnee_soumission[,col], na.rm = TRUE)
    donnee_soumission[,col][is.na(donnee_soumission[,col])] <- mean_value
  } else if (is.character(donnee_soumission[[col]])) {
    # Remplacer NA par le mode pour les colonnes de caract?res
    mode_value <- get_mode(donnee_soumission[,col][!is.na(donnee_soumission[,col])])
    donnee_soumission[,col][is.na(donnee_soumission[,col])] <- mode_value
  }
}

# On v?rifie s'il y a encore des NA 
apply(donnee_soumission, 2, function(x) sum(is.na(x)))
# Plus de donn?es manquantes
donnee_soumission <- data.frame(lapply(donnee_soumission, function(x) if(is.character(x)) as.factor(x) else x))
predictions <- predict(aic_select$model, newdata = donnee_soumission[,names(aic_select$model$coefficients)[-1]])







#step_select <- ols_step_backward_p(full_model, p_val = 0.05)
#summary(step_select$model)
#step_select$predictors
#print(step_select$predictors)

data_model_select <- data.frame(data_train[, c("logSalePrice", aic_select$predictors[-2])])
final_model <- lm(logSalePrice~., data=data_model_select)

#Residus partiels
crPlots(final_model)

#Predictions
predictions <- predict(final_model, newdata = data_test[,c("logSalePrice", step_select$predictors[-2])])

