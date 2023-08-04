
#install.packages("patchwork")
library("rmarkdown")
library("flexdashboard")
library("ggplot2")
#library(gridExtra)
#library(patchwork)
library(plotly)

dat<- read.csv("C:/Users/User/Desktop/STQD_ data visual/individual project/Sleep_health_and_lifestyle_dataset.csv", sep=",", header=TRUE)
head(dat,3)
sum(is.na(dat))  # no NA values
#unique(dat["parental.level.of.education"])
#unique(dat["race.ethnicity"])
#unique(dat["test.preparation.course"])
#unique(dat["SleepTime"])
sapply(dat, class) # see data type for each column
# Subset the data for male
male_data <- subset(dat, Gender == "Male")

# Subset the data for female
female_data <- subset(dat, Gender == "Female")



#plot 1 : to see the occupation distribution

ggplot(dat, aes(x="", fill=factor(Occupation)))+
  geom_bar(width=1)+coord_polar(theta="y")
  #geom_text(aes(label = ..count..), stat = "count", position = "stack")


#plot 2: duration of sleep vs sleep quality

ggplotly(ggplot(dat, aes(x=Sleep.Duration, y=Quality.of.Sleep, group=Occupation, fill=Occupation))+
  geom_point(shape=21, aes(size=Quality.of.Sleep))+
  #labs(colours="Occupation")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# we can see that the longer the sleep duration, the better the sleep quality
# we can further see that the occupation with sleep quality value > 8 are teacher, engineer, lawyer, manager


# plot 3: quality of sleep vs activity level

ggplotly(ggplot(dat, aes(x = Quality.of.Sleep, y =Physical.Activity.Level,colour=Occupation, group = Occupation)) +
  geom_point() +
  labs(x ="sleep quality", y = "activity level", colours="Occupation") +
  theme_minimal())

# from the plot, overall we see that sleep quality improves activity level. 
# this is because as sleep quality increase, the activity level increase as well


# plot 4: stress level for each occupation


ggplot(dat, aes(x = Occupation, y = Stress.Level, fill = Occupation)) +
  geom_bar(stat = "identity") +
  labs(x = "Occupation", y = "Stress Level") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

# next, we would like to observe the stress level for each occupation
# this is to further understand the relationship of stress level for each occupation
# as we can see, the occupation with highest stress level is doctor, followed by nurse. 
# in contrast, the lowest stress level belongs to managers


# plot 5: stress level and heart rate 


ggplotly(ggplot(dat, aes(fill = Occupation, x = Stress.Level, y = Heart.Rate)) +
  geom_point() +
  labs(fill = "Occupation", x = "stress level", y = "Heart Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# from here, we can see that as stress level increase, the heart rate increases
# however there are some outliers for lower stress level where the heart rate is high
# there might be lifestyle or workstyle that would lead to high heart rate, which include 
# low physical activity level will also affect heart rate



# plot 6: daily steps vs heart rate

ggplotly(ggplot(dat, aes(fill = Occupation, x = Daily.Steps, y= Heart.Rate)) +
  geom_point() +
  labs(y = "heart rate", x = "Daily Steps") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))


# in conjunction to the previous plot, we can see that the daily steps have
# negative relationship towards heart rate. this is because we can observe that
# as daily step increase, the heart rate decreases. 
# we can also see that doctor has the lowest heart rate as they have highest daily steps


# plot 7: BMI category distribution
ggplot(dat, aes(x = BMI.Category, fill = BMI.Category)) +
  geom_bar() +
  labs(x = "BMI Category", y = "Frequency", fill = "BMI Category") +
  theme_minimal()

# next, we will evaluate the BMI relationship with other variables for each occupation
# this is because we wish to understand the relationship of BMI affecting or affected by 
# the occupation, stress level, blood pressure and others
# but first, we will see the general distribution of BMI category, and we observe
# that majority of the dataset participant belongs to normal, and followed by overweight.

# plot 8: BMI category vs blood pressure
ggplotly(ggplot(dat, aes(fill = Occupation, y = Blood.Pressure, x= BMI.Category)) +
           geom_point() +
           labs(y = "blood pressure", x = "BMI category") +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# from the plot, we can see the distribution of each occupation on the BMI and blood pressure
# this is to see that occupation that belongs to obese and overweight will usually have high blood pressure
# this can come hand in hand with the stress level from the previous plots
# as we can see, on average, the doctor has highest blood pressure and stress level (from previous plot)
# and this can further infer that doctors have unhealthy lifestyle which results them to being obese generally


#plot 9: BMI vs sleep duration
ggplotly(ggplot(dat, aes(fill = Occupation, y = Sleep.Duration, x= BMI.Category)) +
           geom_point() +
           labs(y = " sleep duration", x = "BMI Category") +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)))


# we can observe that for normal and normal weight, the sleep duration is mostly around 7 hours and above
# whereas, for overweight, the sleep duration is mostly below 7 hours
# therefore, we can assume that sleep duration has a positive relationship with BMI 
# lesser sleep will negatively affect BMI, and more sleep will result to better BMI


# plot 10: sleep disorder vs BMI 
ggplot(dat, aes(x = Sleep.Disorder, fill = BMI.Category)) +
  geom_bar() +
  labs(x = "Sleep Disorder", y = "Count", fill = "BMI Category") +
  theme_minimal()


#lastly, we wish to understand relationship between sleep disorder affecting BMI category
# we see that when there is no sleep disorder, the BMI mostly will be normal
# and there is an equal distribution when there is a presence of sleep disorder, which leads to overweight
# therefore, we can conclude that sleeping quality, including sleeping duration and absence of sleep disorder will have a positive impact on BMI


### conclusion
#In conclusion, we can infer that the doctor occupation are most stressful occupation as there is a high reading on stress level and blood pressure but have good sleep quality.
#Then we move into a more in-depth study of how sleep and stress affects BMI, and we found that high blood pressure, poor sleep duration and presence of sleep disorder leads to poor BMI category.
#Thus, we can conclude that any occupation have a mixed distribution of BMI reading. But the doctors and nurses have better lifestyle as they have better sleep quality and better health heart rate.

rmarkdown::run("C:/Users/User/Desktop/STQD_ data visual/individual project/indi_proj_hilman_p121535.Rmd")





