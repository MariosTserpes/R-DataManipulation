      #### Tserpes Marios ####
###Research Methods in Data Science###
       ### Assignment 1 #######
      
#install package
install.packages("MASS")
library(MASS)      

      
#Quick look about data set
?Boston
View(Boston)
str(Boston)


####################
####QUESTION 1.i####
####################

#Objects that contain number of rows and columns of Boston's dataset
number.of.rows <- nrow(Boston)
number.of.columns <- ncol(Boston)

sprintf('The number of rows is %d. The number of columns is %s.',
        number.of.rows, number.of.columns)


####################
####QUESTION 1.ii####
####################
pairs(Boston, main = 'Pairwise comparisons between variables')

#In this step I create each scatter plot differently to make the relationship more visible.
#scatter plot for rad and crim rate
plot(Boston$rad, Boston$crim, xlab = 'rad', ylab = 'crim', 
    col = 'blue', main = 'scatter plot for rad and crim')

#scatter plot for tax and crim rate
plot(Boston$tax, Boston$crim, xlab = 'tax', ylab = 'crim', 
     col = 'blue', main = 'scatter plot for tax and crim')

#scatter plot for crim and tax, reducing xlimits and ylimits
plot(Boston$tax, Boston$crim, xlab = 'tax', ylab = 'crim', 
     col = 'blue', main = 'scatter plot for tax and crim',
     ylim = c(0, 10), xlim = c(min(Boston$tax), 450))


#scatter plot for lstat and crim rate
plot(Boston$lstat, Boston$crim, xlab = 'lstat', ylab = 'crim', 
     col = 'blue', main = 'scatter plot for lstat and crim')
abline(lm(Boston$crim~Boston$lstat))


#scatter plot for black and crim rate
plot(Boston$black, Boston$crim, xlab = 'black', ylab = 'crim', 
     col = 'black', main = 'scatter plot for black and crim')

#scatter plot for medv and crim rate
plot(Boston$medv, Boston$crim, xlab = 'medv', ylab = 'crim', 
     col = 'black', main = 'scatter plot for medv and crim')
abline(lm(Boston$crim~Boston$medv), col = 'red')

#scatter plot for zn and crim rate
plot(Boston$zn, Boston$crim, xlab = 'zn', ylab = 'crim', 
     col = 'red', main = 'scatter plot for zn and crim')

 
#scatter plot for indus and crim rate
plot(Boston$indus, Boston$crim, xlab = 'indus', ylab = 'crim', 
     col = 'red', main = 'scatter plot for indus')


     
#scatter plot for chas and crim rate.!!!DOES NOT MAKE SENSE DUE TO THE FACT THAT IS CATEGORICAL
plot(Boston$chas, Boston$crim, xlab = 'chas', ylab = 'crim', 
     col = 'red', main = 'scatter plot for chas and crim')

#scatter plot for nox and crim rate
plot(Boston$nox, Boston$crim, xlab = 'nox', ylab = 'crim', 
     col = 'red', main = 'scatter plot for nox and crim')
abline(lm(Boston$crim~Boston$nox))


#scatter plot for rm and crim rate
plot(Boston$rm, Boston$crim, xlab = 'rm', ylab = 'crim', 
     col = 'red', main = 'scatter plot for rm and crim')
abline(lm(Boston$crim~Boston$rm))


#scatter plot for age and crim rate
plot(Boston$age, Boston$crim, xlab = 'age', ylab = 'crim', 
     col = 'red', main = 'scatter plot for age and crim')
abline(lm(Boston$crim~Boston$age))

#scatter plot for dis and crim rate
plot(Boston$dis, Boston$crim, xlab = 'dis', ylab = 'crim', 
     col = 'red', main = 'scatter plot for dis and crim')
abline(lm(Boston$crim~Boston$dis))

#In this plot(dis and crime rate) i have reduce xlimits in order to be more visible the non-linear relationship
plot(Boston$dis, Boston$crim, xlab = 'dis', ylab = 'crim', 
     col = 'red', main = 'scatter plot for dis and crim reducing xlimits',
     xlim = c(min(Boston$dis), 4.5))

#scatter plot for ptratio and crim rate
plot(Boston$ptratio, Boston$crim, xlab = 'ptratio', ylab = 'crim', 
     col = 'red', main = 'scatter plot for ptratio and crim')



####################
####QUESTION 1.iii####
####################

hist(Boston$crim, breaks = 7, main = 'per capita crime rate by town',
     xlab = 'crim', col = 'blue', labels = TRUE)
# +3 standard deviations from the mean
mean(Boston$crim) + (sd(Boston$crim) *3)


hist(Boston$tax, breaks = 10, main = 'full-value-property-tax rate',
     xlab = 'crim', col = 'red', labels = TRUE)
# +3 standard deviations from the mean
mean(Boston$tax) + (sd(Boston$tax) *3)

hist(Boston$ptratio, breaks = 10, main = 'pupil-teacher ratio by town',
     xlab = 'crim', col = 'white', labels = TRUE)
# +3 standard deviations from the mean
mean(Boston$ptratio) + (sd(Boston$ptratio) * 3)


####################
####QUESTION 1.iv####
####################

#a subset that contains data about suburbs bordering with Charles' river
suburbs.near.river <- subset(Boston, Boston$chas == 1) #or !=0
#an object tha contains the number of suburbs bordering with the river
num.rows.near.river <- nrow(suburbs.near.river)

#percentage of suburbs near to Charles'river
percent.subs.near.river <- round(num.rows.near.river/nrow(Boston)*100,1)

sprintf('Number of suburbs bordering Charles is %s.Percentaglly, is %f percent',
        num.rows.near.river, percent.subs.near.river)


#if we wouldlike to visualize a barplot as i have mentioned in report
#Boston$chas <- as.factor(Boston$chas)
#levels(Boston$chas) <- c('Not in Charles', 'In charles')

#plot(Boston$chas, main = 'Number of suburbs bordering Charles or not,
#col = 'blue', xlab = 'chas)



####################
####QUESTION 1.v####
####################


median.value <- median(Boston$ptratio)
sprintf('The median ratio of pupils to teachers is : %s. ',
        median.value)

#or alternatively 
#median.value2 <- summary(Boston$ptratio)[3]

####################
####QUESTION 1.vi####
####################

#in order to examine the minimum value: medv = 5.00
summary(Boston$medv)

#a subset that contains suburbs with the minimum medv
suburbs.min.medv <- subset(Boston, Boston$medv == min(Boston$medv))
suburbs.min.medv



summary(suburbs.min.medv)
summary(Boston)

####################
####QUESTION 1.vii####
####################

#a subset that contains suburbs with more than 8 rooms
more.than.8.rm <- subset(Boston, Boston$rm > 8)
num.of.subs.8.rm <- nrow(more.than.8.rm)
sprintf("The number of suburbs with more than 8 rooms per dwelling is : %s .",
       num.of.subs.8.rm)

print(more.than.8.rm)

summary(more.than.8.rm)
summary(Boston)


#hist$age for  subset
hist(more.than.8.rm$age, breaks = 10,
     col = 'red', labels = TRUE)


#plotline in order to visualize the difference in lower status of the population (percent).
plot(more.than.8.rm$lstat, type = 'b', col = 'red', lwd = 5, 
     main = 'lstat of subset with rooms greater than 8')
plot(Boston$lstat, type = "b", col = "blue", main = 'overall population') 


#about crime rate in subset and overall population
plot(more.than.8.rm$crim, col = 'red', type = 'b',
     main = 'crime rate of subset with rooms greater than 8', labels = TRUE)

plot(Boston$crim, col = 'blue', type = 'l',
     main = 'overall population')


#about median value of homes in subset and overall population
hist(more.than.8.rm$medv, col = 'red', labels = TRUE,
     main = 'medv of homes of subset with rooms greater than 8')
hist(Boston$medv, col = 'blue', labels = TRUE,
     main = 'overall population')




