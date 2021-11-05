knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(ggplot2)
library(tidyverse)
library(openintro)
library(agridat)
library(boot)
library(ggridges)
library(gridExtra)
library(broom)
#(1)
ames <-openintro::ames
ggplot(ames,aes(fct_infreq(Roof.Style)))+geom_bar(color='blue',fill='lightblue')
ggplot(ames,aes(Mo.Sold))+geom_bar(color='blue',fill='lightblue')+
  scale_x_continuous(breaks = seq(1, 12, 1))
fc <- c("Ex","Fa","Gd","Po","TA")
df1 <- ames[,sapply(sapply(ames,levels),identical,fc)]
df2 <- ames[,(str_detect(sapply(ames,levels), fc))==TRUE]
df2 <- df2[,2:11]
names(df2)
df1 %>%
  rownames_to_column("observation") %>%
  pivot_longer(cols=!observation,names_to = "type",values_to = "level")
df3 <- df2 %>%
  rownames_to_column("observation") %>%
  pivot_longer(cols=!observation,names_to = "type",values_to = "level")

ggplot(df3,aes(level))+geom_bar(color='blue',fill='lightblue')+
  facet_wrap(~type,ncol = 5)

#(2)
dog <- filter(seattlepets,species=="Dog")
cat <- filter(seattlepets,species=="Cat")
dogcat <- filter(seattlepets, species %in% c("Dog","Cat"))
dogname <-as.data.frame(sort(table(dog$animal_name),decreasing = TRUE))
colnames(dogname) <- c("name","count")
catname <- as.data.frame(sort(table(cat$animal_name),decreasing = TRUE))
colnames(catname) <- c("name","count")
dogcatname <-as.data.frame(sort(table(dogcat$animal_name),decreasing=TRUE))
colnames(dogcatname) <- c("name","count")
#a
ggplot(dogname[1:30,],aes(count,name))+geom_point()
ggplot(catname[1:30,],aes(count,name))+geom_point()
#b
dc <- inner_join(dogname,catname, by=c("name"="name"))
dcc <- dc %>%
  mutate(proportion=count.x/(count.x+count.y)) %>%
  arrange(desc(proportion))
ggplot(dcc[1:30,],aes(proportion,fct_reorder(name,proportion)))+geom_point()
#c
ddc <- dc %>%
  mutate(total=count.x+count.y) %>%
  arrange(desc(total))
ggplot(ddc[1:30,])+
  geom_point(aes(total,fct_reorder(name,total),color='total')) +
  geom_point(aes(count.x,name,color='dog')) +
  geom_point(aes(count.y,name,color='cat'))
#d
ggplot(dc,aes(count.x,count.y))+geom_point(col='blue',fill='lightblue')
dvsc <- mutate(ddc,ratio=count.x/count.y)
dvsc
ggplot(filter(dvsc,total>50),aes(ratio))+geom_boxplot()
#e

#(3)
ggplot(ames,aes(price,area))+geom_point(color='blue',alpha=.5,size=2)
ggplot(ames,aes(price,area))+geom_point()+geom_density_2d()
#bins=30 by default
ggplot(ames,aes(price,area))+geom_hex()
ggplot(ames,aes(price,area))+geom_bin_2d()
ggplot(ames,aes(price,area))+geom_bin2d()

#(4)
#a
ames_new <- ames
neigh_sort <- ames_new %>%
  group_by(Neighborhood) %>%
  do(model=lm(price~area, data= .)) %>%
  mutate(coeff=coef(model)[2]) %>%
  arrange(coeff)
neigh_sort
ames_new$Neighborhood <- factor(ames_new$Neighborhood,levels = neigh_sort$Neighborhood)
ggplot(ames_new,aes(area,price))+
  geom_point(color='blue',alpha=.5)+
  geom_smooth(method = "lm", col = "red")+
  facet_wrap(~Neighborhood,ncol = 4)
#c
A=lm(price~area,data=ames)
summary(A)[['r.squared']]
cor(ames$price,ames$area)^2
ames2 <- ames
rs_sort <- ames2 %>%
  group_by(Neighborhood) %>%
  do(model=lm(price~area, data= .)) %>%
  mutate(coeff=summary(model)[['r.squared']]) %>%
  arrange(coeff)
rs_sort
ames2$Neighborhood <- factor(ames2$Neighborhood,levels=rs_sort$Neighborhood)
ggplot(ames2,aes(area,price))+
  geom_point(color='blue',alpha=.5)+
  geom_smooth(method = "lm", col = "red")+
  facet_wrap(~Neighborhood,ncol = 4)
