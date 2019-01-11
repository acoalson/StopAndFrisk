download.file('http://math.hmc.edu/m35f/2010_sqf_m35.csv', '2010_sqf_m35.csv')
download.file('http://math.hmc.edu/m35f/2015_sqf_m35.csv', '2015_sqf_m35.csv')

sqf2010 = read.csv('2010_sqf_m35.csv')
sqf2015 = read.csv('2015_sqf_m35.csv')

sqf2015 = sqf2015[!sqf2015$perstop=="**" & !sqf2015$perstop==" ",]
sqf2015$perstop = as.numeric(as.character(sqf2015$perstop))


#Mosaic plot to visualize number of arrests made per race from the data
mosaicplot(table(sqf2015$arstmade, sqf2015$race),
           xlab="Arrest Made", ylab="Races",
           main="Number of Arrests Made per Race")


#move white on top so when taking the binomial regression we use
#WHITE as the baseline
sqf2015$race <- relevel(sqf2015$race, "WHITE")


#Takes a binomial regression that looks at how race effects
#the arrests made using WHITE arrest made as a baseline
mod <- glm(arstmade ~ race, data=sqf2015, family="binomial")
summary(mod)


#after fitting the model with regression
data <- data.frame(race=as.factor(c("WHITE","BLACK","BLACK-HISPANIC","WHITE-HISPANIC","OTHER","UNKNOWN")))
#predictive probability of the above races getting arrested
predict(mod, newdata = data , type = "response")




