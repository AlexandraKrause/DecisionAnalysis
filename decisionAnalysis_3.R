#tasks####

#Create a scatter plot, barchart and boxplot (as above)
#Vary the sample and run the same analysis and plots
#Save your most interesting figure and share it with us


#Seminar 3####
library(ggplot2)
#for top:
library(dplyr)
?diamonds
names(diamonds)
head(diamonds)
str(diamonds)


# Scatter plot####
#I (Inhibit Interpretation/ Conversion of Objects) 
dsmall<-top_n(diamonds,n=50)
ggplot(data=dsmall, 
       aes(x = color, 
           y = price,
           color = color,
           alpha= I(0.1)))+
  geom_point()+
  ggtitle("Price of diamonds in relation to their color")+
  labs(x="Color of diamonds",
       y="Price of diamonds")

# Boxplot####
ggplot(data = diamonds,
       aes(x = color,
           y = price,
           alpha = I (0.1),
           color = I ("blue"))) +
  geom_boxplot()+
  ggtitle("Price of diamonds in relation to their color")+
  labs(x="Color of diamonds",
       y="Price of diamonds")

# Jitter plot (Boxplot)
ggplot(data = diamonds,
       aes(x = color,
           y = price,
           alpha = I (0.1),
           color = I ("blue"))) +
  geom_boxplot() +
  geom_jitter() +
  labs(title = "Relation of color and price")+
  ggtitle("Price of diamonds in relation to their color")+
  labs(x="Color of diamonds",
       y="Price of diamonds")

# Barplot####

ggplot(data = diamonds,
       aes(x = price,
           alpha = I (0.1),
           color = I ("blue"))) +
  geom_bar() +
  ggtitle("Price of diamonds in relation to their color")+
  labs(x="Price",
       y= "Count")



#Sample change to Iris####
?iris
names(iris)
head(iris)
str(iris)

# Scatter plot####
#I (Inhibit Interpretation/ Conversion of Objects) 
ismall<-top_n(iris,n=50)
ggplot(data=ismall, 
       aes(x = Petal.Length, 
           y = Sepal.Length,
           size = Petal.Length,
           alpha= I(0.1)))+
  geom_point()+
  ggtitle("Petal lenght in relation to  sepal lenght")+
  labs(x="Petal lenght",
       y="Sepal lenght")

# Boxplot
ggplot(data = ismall,
       aes(x = Petal.Length, 
           y = Sepal.Length,
           alpha = I (0.1),
           color = I ("blue"))) +
  geom_boxplot()+
  ggtitle("Petal lenght in relation to Sepal lenght")+
  labs(x="Petal lenght",
       y="Sepal lenght")

# Barplot####

ggplot(data = ismall,
       aes(x = Petal.Length, 
           alpha = I (0.1),
           color = I ("blue")))+
  geom_bar() +
  ggtitle("Distribution of Petal lenghts")+
  labs(x="Petal Lenght",
       y= "Count")