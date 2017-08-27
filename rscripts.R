library(readr)
library(dplyr)
library(ggplot2)

gapminder <- read.csv(file="c:/Users/xuepan/SDC_workshop_20170826/intro-r-20170825/datasets/gapminder_backup.txt", header = TRUE, sep = '\t')
summary(gapminder)
str(gapminder)
head(gapminder)
gapminder[gapminder$gdpPercap == max(gapminder$gdpPercap),]
min(gapminder$lifeExp)
gapminder[gapminder$lifeExp == min(gapminder$lifeExp),]
nrow(gapminder[gapminder$lifeExp < 50, ])
asia <- gapminder[gapminder$continent == "Asia",]
length(unique(asia$country))
gapminder$gdp <- gapminder$gdpPercap * gapminder$pop
gapminder$RelationToMeanPop <- NA
for(i in 1:nrow(gapminder)){
  if (gapminder$[i, 'pop'] > mean(gapminder$pop)){
    gapminder$RelationToMeanPop[i] = "Above"
  }else if(gapminder$pop[i] <- mean(gapminder$pop)){
    gapminder$RelationToMeanPop[i] = "Below"
  } else if(gapminder$RelationToMeanPop == mean(gapminder$pop)){
    gapminder$RelationToMeanPop[i] = "At"
  } else next
}
table(gapminder$RelationToMeanPop)
ggplot(gapminder, aes(x=year,y=lifeExp, color=continent)) +
  geom_smooth(method = 'lm')

ggplot(gapminder, aes(x=continent, y=gdpPercap, fill=continent)) + 
  geom_boxplot()