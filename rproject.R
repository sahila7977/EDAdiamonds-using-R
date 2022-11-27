#load the dataset
diamonds <- read.csv("C://Users//Asus//Desktop//Itvedant lectures//R//diamonds.csv")
#summarise
summary(diamonds)
#preview the dataset
head(diamonds)
print(nrow(diamonds))
library(ggplot2)
library(dplyr)
#removing diamonds with length,width,depth of 0
df <-diamonds %>% filter(x > 0, y > 0, z > 0)
print(nrow(df))
#checking for outliers
df %>% 
  ggplot(aes(x, factor(1))) +
  geom_boxplot()
df %>% 
  ggplot(aes(y, factor(1))) +
  geom_boxplot()
df %>% 
  ggplot(aes(z, factor(1))) +
  geom_boxplot()
df %>% 
  ggplot(aes(carat, factor(1))) +
  geom_boxplot()
#removing outliers
df <- df %>% 
  filter(x<10, y < 20, z < 10, carat < 2.5)
df %>% 
  ggplot(aes(carat)) +
  geom_histogram(binwidth = 0.01)
df %>%
  ggplot(aes(cut)) +
  geom_bar()
df %>%
  ggplot(aes(color)) +
  geom_bar()
df %>%
  ggplot(aes(clarity)) +
  geom_bar()
summary(diamonds)
print(nrow(df))
#so 53940-53775=165 rows were removed
ggplot(data = df, mapping = aes(x = carat, y =price)) + 
  geom_point() 
ggplot(data = df, mapping = aes(x = carat, y =price)) + 
  geom_point() + geom_smooth()
df %>% 
  ggplot(aes(carat, price, color = cut)) +
  geom_smooth()
df %>% 
  ggplot(aes(carat, price, color = clarity)) +
  geom_smooth()
df %>% 
  ggplot(aes(carat, price, color = color)) +
  geom_smooth()
x=df %>% 
  group_by(cut) %>% 
  summarize(Mean = mean(price))  %>%   
  ungroup()
x
a<-ggplot(data=x, aes(x=cut, y=Mean)) +
  geom_bar(stat="identity")
a
y=df %>% 
  group_by(color) %>% 
  summarize(Mean = mean(price))  %>%   
  ungroup()
y
b<-ggplot(data=y, aes(x=color, y=Mean)) +
  geom_bar(stat="identity")
b
z=df %>% 
  group_by(clarity) %>% 
  summarize(Mean = mean(price))  %>%   
  ungroup()
z
c<-ggplot(data=z, aes(x=clarity, y=Mean)) +
  geom_bar(stat="identity")
c
