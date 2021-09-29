library(readxl)
autoencoded = read_excel("C:/Users/matth/OneDrive/Store Sales Folder/autoencoded.xlsx")
View(autoencoded)

#Backwards elimination for gender
FitAll = lm(Gender ~ ., data = autoencoded)
summary(FitAll)
step(FitAll, direction = 'backward')
fitsome = lm(Gender ~ Branch + `Unit price` + `Tax 5%` + Time, 
             data = autoencoded)
#AIC=-1392.9




#Backwards elimination for Rating
FitAll = lm(Rating ~ ., data = autoencoded)
summary(FitAll)
step(FitAll, direction = 'backward')
fitsome=lm(formula = Rating ~ City + `Unit price` + Quantity + `Tax 5%`, 
   data = autoencoded)
#AIC=1084.34



#CITY
FitAll = lm(City ~ ., data = autoencoded)
summary(FitAll)
step(FitAll, direction = 'backward')
fitsome=lm(formula = City ~ Branch + Time + Rating, data = autoencoded)
#AIC=-693.88



#CUSTOMER TYPE
FitAll = lm('Customer type' ~ ., data = autoencoded)
summary(FitAll)
step(FitAll, direction = 'backward')
fitsome=lm(formula = City ~ Branch + Time + Rating, data = autoencoded)
#AIC=-58473.22



#COGS
FitAll = lm(cogs ~ ., data = autoencoded)
summary(FitAll)
step(FitAll, direction = 'backward')
fitsome=lm(formula = cogs ~ `Invoice ID` + Branch + City + `Customer type` + 
             `Unit price` + `Tax 5%`, data = autoencoded)
#AIC=-58473.22



#PAYMENT
FitAll = lm(Payment ~ ., data = autoencoded)
summary(FitAll)
step(FitAll, direction = 'backward')
fitsome=lm(formula = Payment ~ Branch + Time, data = autoencoded)
#AIC=-371.32



