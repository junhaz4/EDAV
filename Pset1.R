### 1. Fast Food
library(ggplot2)
library(tidyverse)
library(openintro)
library(agridat)
library(boot)
library(ggridges)
library(gridExtra)
fastfood <- openintro::fastfood
View(fastfood)
ggplot(fastfood,aes(restaurant,calories))+geom_boxplot()+coord_flip()
### median, outliers. range, half values fall in some range
ggplot(fastfood,aes(calories))+geom_histogram( color = "blue", fill = "lightblue")+
  facet_wrap(~ restaurant)
#mode,mean,multimodality and other features of distributions,no original data
new_data <- filter(fastfood,
       xor(str_detect(fastfood$item,'Crispy'),str_detect(fastfood$item,'Grilled'))==TRUE)
new_data$cooktype <-
  ifelse(str_detect(new_data$item,'Crispy')==TRUE,'Crispy','Grilled')
ggplot(new_data,aes(x=calories,fill=cooktype))+geom_density(alpha=.4)

new_data2 <- filter(fastfood,
                   xor(str_detect(toupper(fastfood$item),'CRISPY'),
                       str_detect(toupper(fastfood$item),'GRILLED'))==TRUE)
new_data2$cooktype <-
  ifelse(str_detect(toupper(new_data2$item),'CRISPY')==TRUE,'CRISPY','GRILLED')
View(new_data2)
new_data3 <- filter(new_data2,new_data2$item,-starts_with('Chargrilled'))
View(new_data3)
ggplot(new_data3,aes(x=calories,fill=cooktype))+geom_density(alpha=.4)

### 2. Temporal Lobes
mtl <- openintro::mtl
View(mtl)
hist(mtl$asubic,breaks="FD")
### default S3 method
nclass.Sturges(mtl$asubic)
ceiling(log(length(mtl$asubic),base=2))+1
hist(mtl$asubic,breaks="Sturges")
pretty(mtl$asubic)
### the default set for bin number in ggplot is 30
ggplot(mtl,aes(asubic))+geom_histogram( color = "blue", fill = "lightblue")
###asubic represent the thickness of the subiculum subregion.small bidwidth means more details
### help analyze the effects of asubic and help visualize how the difference in asubic affect
### sedentary behavior. Big binwidth too general, not good for analysis
ggplot(mtl,aes(age))+geom_histogram(breaks = seq(45, 80, 5),right = FALSE,
                                    color = "blue", fill = "lightblue")+
  scale_x_continuous(breaks = seq(45, 80, 5))+
  scale_y_continuous(breaks = seq(0, 10, 2))
ggplot(mtl,aes(age))+geom_histogram(breaks = seq(45, 80, 5),right = TRUE,
                                    color = "blue", fill = "lightblue")+ 
  scale_x_continuous(breaks = seq(45, 80, 5))+
  scale_y_continuous(breaks = seq(0, 10, 2))
par(mfrow=c(1,2))
h1 <- hist(mtl$age,right = FALSE, col = "lightblue")
h2 <- hist(mtl$age,right = TRUE, col = "lightblue")


hist(mtl$age,right = FALSE, col = "lightblue",breaks = seq(45.5,75.5,5),axes=FALSE)
axis(1,at=seq(45.5,75.5,5))
axis(2,at=seq(0,10,2))
#adjust the bounday of each bin so that they contain the same data

### 3. Soybeans
ggplot(australia.soybean, aes(sample=yield)) + geom_qq(distribution = qnorm) + 
  geom_qq_line(col = "blue")+
  facet_wrap(~ loc, nrow = 1)
### L closed to be normal
ggplot(australia.soybean,aes(yield))+
  geom_histogram(aes(y=..density..),color = "black", fill = "white")+
  geom_density(col='blue')+
  stat_function(fun = dnorm, 
                args = list(mean = mean(australia.soybean$yield), 
                            sd = sd(australia.soybean$yield)),col='red')+
  facet_wrap(~ loc)

Brookstead <- filter(australia.soybean,loc=='Brookstead')
Lawes <- filter(australia.soybean,loc=='Lawes')
Nambour <- filter(australia.soybean,loc=='Nambour')
RedlandBay <- filter(australia.soybean,loc=='RedlandBay')

g1 <- ggplot(Brookstead,aes(yield))+
  geom_histogram(aes(y=..density..),color = "black", fill = "white")+
  geom_density(col='blue')+
  stat_function(fun = dnorm, 
                args = list(mean = mean(Brookstead$yield), 
                            sd = sd(Brookstead$yield)),col='red')+
  ggtitle("Brookstead")
g2 <- ggplot(Lawes,aes(yield))+
  geom_histogram(aes(y=..density..),color = "black", fill = "white")+
  geom_density(col='blue')+
  stat_function(fun = dnorm, 
                args = list(mean = mean(Lawes$yield), 
                            sd = sd(Lawes$yield)),col='red')+
  ggtitle("Lawes")
g3 <- ggplot(Nambour,aes(yield))+
  geom_histogram(aes(y=..density..),color = "black", fill = "white")+
  geom_density(col='blue')+
  stat_function(fun = dnorm, 
                args = list(mean = mean(Nambour$yield), 
                            sd = sd(Nambour$yield)),col='red')+
  ggtitle("Nambour")
g4 <- ggplot(RedlandBay,aes(yield))+
  geom_histogram(aes(y=..density..),color = "black", fill = "white")+
  geom_density(col='blue')+
  stat_function(fun = dnorm, 
                args = list(mean = mean(RedlandBay$yield), 
                            sd = sd(RedlandBay$yield)),col='red')+
  ggtitle("RedlandBay")
grid.arrange(g1,g2,g3,g4,nrow=2)
B <- shapiro.test(Brookstead$yield)
L <- shapiro.test(Lawes$yield)
N <- shapiro.test(Nambour$yield)
R <- shapiro.test(RedlandBay$yield)
B
L
N
R
### compare p-value with alpha=0.05. if p big, then normal, else not normal

### 4. Doctors
smoke <- filter(breslow,smoke==1)
nsmoke <- filter(breslow,smoke==0)
g1 <- ggplot(smoke,aes(age,y))+geom_col(color = "blue", fill = "lightblue")+
  ggtitle("for smoke")
g2 <- ggplot(nsmoke,aes(age,y))+geom_col(color = "blue", fill = "lightblue")+
  scale_y_continuous(limits=c(0, 200))+
  ggtitle("for not smoke")
grid.arrange(g1,g2,nrow=1)

### 5. Loans
str(loans_full_schema$loan_amount)
summary(loans_full_schema$loan_amount)
ggplot(loans_full_schema,aes(loan_amount))+geom_boxplot()
ggplot(loans_full_schema,aes(loan_amount))+geom_histogram(color = "blue", fill = "lightblue")
ggplot(loans_full_schema,aes(x=loan_amount,y=loan_purpose))+
  geom_boxplot()
ggplot(loans_full_schema,aes(x=loan_amount,y=loan_purpose))+
  geom_density_ridges() +
  theme_ridges() 
##boxplot better,displays the range and distribution of data and outliers
##provide some indication of the data’s symmetry and skew-ness
#d) Short Answer: The three methods used in a,b and c almost give the same results. In part(a), we used the qqplot with theoretical normal lines, from the graph we conclude that location Lawes appears to be closest the a normal distribution. In part(b), we plotted density histograms with density curves and theoretical normal curves and the graph shows that Lawes and Brookstead appear to be normally distributed. In part(c), we performed the Shapiro-Wilk tests and the test results show that Lawes and Brookstead are normally distributed. 
##ridgeline plots overlap can obscure patterns,