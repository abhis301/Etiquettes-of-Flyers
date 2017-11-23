install.packages("vcd")
install.packages("BHH2")
install.packages("lmPerm")
library(lmPerm)
library(plotrix)
library(BHH2)
library(grid)
library(vcd)
library(tidyr)
library(ggplot2)
library(dplyr)
#loading the package for using inference function
load(url('http://s3.amazonaws.com/assets.datacamp.com/course/dasi/inference.Rdata'))

setwd("C:/Users/Abhishek Singh/Desktop/538/fly")
getwd()
df <- read.csv("data_flying_etiquette.csv")
df$RespondentID <- as.numeric(df$RespondentID)
str(df)
df %>%
group_by(Gender, Age) %>%
  tally(sort = T) %>% filter(n>6)
#converting likely and less likely inclination
levels(df$Recline_obligation) <- sub("No, the person on the flight has no obligation to the person behind them",
                                     "likely", levels(df$Recline_obligation))
levels(df$Recline_obligation) <- sub("Yes, they should not recline their chair if the person behind them asks them not to",
                             "less_likely", levels(df$Recline_obligation))
## defining Tall and not tall category
# for men the average height is 5'10" and for female average height is 5'5"
df$Gender_Height[df$Gender == "Male" & df$Height_in_inches >= 71] <- "Tall Male"
df$Gender_Height[df$Gender == "Male" & df$Height_in_inches < 71] <- "Not Tall Male"
df$Gender_Height[df$Gender == "Female" & df$Height_in_inches >= 66] <- "Tall Female"
df$Gender_Height[df$Gender == "Female" & df$Height_in_inches < 66] <- "Not Tall Female"
df$Gender_Height <- as.factor(df$Gender_Height)
df_height <- df[,c(1:4,6,8,29,10,16)]
names(df_height)
summary(df_height)

# box plot for outlier detection:
boxplot(Height_in_inches~Recline_freq, data = df_height, main="Height VS Recline freq.", 
        xlab="Recline Freq",ylab="Height")

#mosiacplot
mosaicplot(Recline_freq~Gender_Height, data = df_height)
#comparing mens and womens hight distribution:
ggplot(df_height, aes(Height_in_inches)) + geom_histogram(binwidth = .5,fill = "Blue") +
  facet_wrap(~ df$Gender) +
  labs(title= "          Distribution of height by Gender",x = "Height", y= "Number of person")

ggplot(df_height, aes(Height_in_inches)) + geom_histogram(binwidth = .5,fill="Brown") + 
  facet_wrap(~ df$Recline_obligation)+
labs(title= "          Distribution of height by likely to recline",x = "Height", y= "Number of person")



#Average height by likely to recline
df_height %>%
  group_by( Recline_obligation) %>%
  summarize(avgHeight = mean(Height_in_inches)) 

mosaic(Recline_obligation~Gender_Height,data=df_height , gp_labels=(gpar(fontsize=14)))
#Tall males slightly more often than tall females appear to always or usually recline their seats.
attach(df_height)
###Avg total height by gender
df_height %>%
  group_by(Gender) %>%
  summarise(avg_height = mean(Height_in_inches))
  
##Avg height by likely to recline within each  gender
df_height %>%
  group_by(Gender,Recline_obligation)%>%
  summarise(avg_height = mean(Height_in_inches))

##std error of average height
df_height %>%
  group_by(Gender)%>%
  summarise(std.error(Height_in_inches))

##std error of average height among each gender and reline_obligation
df_height %>%
  group_by(Recline_obligation)%>%
  summarise(std.error(Height_in_inches))


################## INFERENCE ######################

a<-inference(y = df_height$Height_in_inches[df_height$Gender == "Male"], 
             x = df_height$Recline_obligation[df_height$Gender=="Male"],est = "mean",
          type = "ht", null = 0, alternative = "twosided", method = "theoretical")
b<-inference(y = df_height$Height_in_inches[df_height$Gender == "Female"], 
             x = df_height$Recline_obligation[df_height$Gender=="Female"],est = "mean",
             type = "ht", null = 0, alternative = "twosided", method = "theoretical")


attach(df_height)
df_height %>%
  group_by(Gender, Recline_obligation) %>%
  summarize(avgHeight = mean(df_height$Height_in_inches))
