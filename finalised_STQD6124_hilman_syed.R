# ////////////////////////// DATA CLEANING ////////////////////
dat_dat<- read.csv('C:/Users/User/Desktop/STQD_ data visual/group project/Life_Expectancy_Data.csv')
life_dat <- dat_dat
Life<- dat_dat
head(life_dat)
str(life_dat)
library(tidyverse)
library(mice)

#to visualise the NA values using MICE
is.na(life_dat)
mice::md.pattern(life_dat)   # too messy, need to check each variable one by one

sum(is.na(life_dat$Life.expectancy))
sum(is.na(life_dat$Adult.Mortality))   
sum(is.na(life_dat$infant.deaths))   # no NA
sum(is.na(life_dat$Alcohol))
sum(is.na(life_dat$percentage.expenditure))  # no NA
sum(is.na(life_dat$Hepatitis.B))       
sum(is.na(life_dat$Measles))    # no NA
sum(is.na(life_dat$BMI))
sum(is.na(life_dat$under.five.deaths))    # no NA
sum(is.na(life_dat$Polio))               
sum(is.na(life_dat$Total.expenditure))
sum(is.na(life_dat$Diphtheria))    
sum(is.na(life_dat$HIV.AIDS))     # no NA
sum(is.na(life_dat$GDP))
sum(is.na(life_dat$Population))
sum(is.na(life_dat$thinness..1.19.years))
sum(is.na(life_dat$thinness.5.9.years))
sum(is.na(life_dat$Income.composition.of.resources))
sum(is.na(life_dat$Schooling))


# fill missing values of with median

imp_med2 <- life_dat %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))
str(imp_med2)
head(imp_med2,10)
sum(is.na(imp_med2))
sum(is.na(life_dat))  # to compare with original data for sum of NA values
str(life_dat)
mice::md.pattern(imp_med2)

life_data<- imp_med2

# looking at developing countries

devping_life_data<- life_data%>%
  filter(life_data$Status=="Developing")%>%
  select(Country,Status,Life.expectancy, Population,GDP,Total.expenditure)

head(devping_life_data)

# looking at developed countries

devped_life_data<- life_data%>%
  filter(life_data$Status=="Developed")%>%
  select(Country,Status,Life.expectancy, Population,GDP,Total.expenditure)

head(devped_life_data)







# //////// hilman part //////////////


# ///////////////////////// GRAPH 1: STATIC VIOLIN PLOT /////////////////////////////
# violin plot to see the overall population distribution of developing and developed countries

set_plot_dimensions(20,10)
mfrow=c(2,2)

# developing country
ggplot(devping_life_data ,aes(x= Status,y = GDP, fill= Status)) + 
  geom_violin() +  
  geom_boxplot(width=0.1, color="blue", alpha=0.2) + 
  stat_summary(fun.y = median, geom = "point", shape = 23, size = 3, fill = "green")+
  ggtitle("GDP per country Status")

# developed country
ggplot(devped_life_data ,aes(x= Status,y=GDP, fill= Status)) + 
  geom_violin() +  
  geom_boxplot(width=0.1, color="blue", alpha=0.2) + 
  stat_summary(fun.y = median, geom = "point", shape = 23, size = 3, fill = "green")+
  ggtitle("GDP per country Status")

summary(devping_life_data$GDP)
summary(devped_life_data$GDP)
# from here, we can see that the average GDP in developing country is around 3,887.74 USD, 
# while the developed country is 19,517.58 USD
# it is approx 5 times for GDP in developed country against developing country



# /////////////////  GRAPH 2: INTERACTIVE SCATTER PLOT //////////////////////

#install.packages("webshot")
library(htmlwidgets)
library(ggplot2)
library(plotly)
library(htmltools)

# //// developed country ////
# manipulating data to create developed country dataframe

# include other relevant variables into dataset (year, population)
devped_life_data_1<- life_data%>%
  filter(life_data$Status=="Developed", life_data$Year==2015)%>%
  select(Country,Year,Status,Life.expectancy, Population,GDP,Total.expenditure)
head(devped_life_data_1,20)

# to find the average of GDP for all 15 years for each country
averages_devped <- devped_life_data_1 %>%
  group_by(Country) %>%
  summarize(Average_Life_Expectancy = mean(Life.expectancy),
            Average_Population = mean(Population),
            Average_GDP = mean(GDP),
            Average_Total_Expenditure = mean(Total.expenditure),
            .groups = "keep")
devped_life_data_1_average<- data.frame(averages_devped)
head(devped_life_data_1_average)



# //// developing country ////
# manipulating data to create developing country dataframe

# include other relevant variables into dataset (year, population)
devping_life_data_1<- life_data%>%
  filter(life_data$Status=="Developing", life_data$Year==2015)%>%
  select(Country,Year,Status,Life.expectancy, Population,GDP,Total.expenditure)
head(devping_life_data_1,20)

# to find the average of GDP for all 15 years for each country
averages_devping <- devping_life_data_1 %>%
  group_by(Country) %>%
  summarize(Average_Life_Expectancy = mean(Life.expectancy),
            Average_Population = mean(Population),
            Average_GDP = mean(GDP),
            Average_Total_Expenditure = mean(Total.expenditure),
            .groups = "keep")
devping_life_data_1_average<- data.frame(averages_devping)
head(devping_life_data_1_average)



# Plotly Scatter Plot : developed country

plot_1 <- ggplot(devped_life_data_1, aes(x = Population,y = GDP, text = Country)) +
  geom_point(shape=21, fill="darkblue", color="darkblue") +
  scale_size_area()+
  ggtitle("Year 2015: GDP vs. Population") +
  xlab("GDP") +
  ylab("Population")

ggplotly(plot_1)

#plot <- ggplot(devped_life_data_1_average, aes(x = Average_Population,y = Average_GDP, text = Country)) +
#  geom_point(shape=21, fill="darkblue", color="darkblue") +
#  scale_size_area()+
#  ggtitle("Scatter Plot: GDP vs. Population") +
#  xlab("GDP") +
#  ylab("Population")

#ggplotly(plot)


# Plotly Scatter Plot : developing country

plot_2 <- ggplot(devping_life_data_1, aes(x = Population,y = GDP, text = Country)) +
  geom_point(shape=21, fill="darkred", color="darkred") +
  scale_size_area()+
  ggtitle("Year 2015: GDP vs. Population") +
  xlab("GDP") +
  ylab("Population")

ggplotly(plot_2)


# Save the plotly object as an HTML file
plotly_obj <- ggplotly(plot_2)
htmlFile <- "plot_2.html"
htmlwidgets::saveWidget(as_widget(plotly_obj), htmlFile, selfcontained = TRUE)

plotly_obj_1 <- ggplotly(plot_1)
htmlFile1 <- "plot_1.html"
htmlwidgets::saveWidget(as_widget(plotly_obj_1), htmlFile1, selfcontained = TRUE)

#plot <- ggplot(devping_life_data_1_average, aes(x = Average_Population,y = Average_GDP, text = Country)) +
#  geom_point(shape=21, fill="darkred", color="darkred") +
#  scale_size_area()+
#  ggtitle("Scatter Plot: GDP vs. Population") +
#  xlab("GDP") +
#  ylab("Population")

ggplotly(plot)



# /////////////////  GRAPH 3: ANIMATION //////////////////////

library("gganimate")
library("gapminder")
library("ggplot2")
library("gifski")


# for developed country
devped_life_data_1_1<- life_data%>%
  filter(life_data$Status=="Developed")%>%
  select(Country,Year,Status,Life.expectancy, Population,GDP,Total.expenditure)
head(devped_life_data_1,20)

plot_1<- ggplot(devped_life_data_1_1, aes(x = GDP,y = Total.expenditure, color = Country, size = Population)) +
  geom_point() +
  guides(colour=F, size=F)+
  ggtitle("GDP vs. Total health expenditure for Year: {frame_time}") +
  ylab("Total health expenditure") +
  xlab("GDP")+theme_bw()+
  transition_time(Year)+
  theme(plot.background = element_rect(fill = "white", size = 8),
        panel.background = element_rect(fill = "white", size = 8),
        plot.margin = margin(0, 0.5, 0, 0, "cm"), 
        legend.position="bottom",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.key.size = unit(0.1, "cm"),
        legend.margin = margin(0, 0, 0, 0, "cm"))+
  guides(color = guide_legend(direction = "vertical", title.position = "top", title.hjust = 0.5),
         size = guide_legend(direction = "vertical", title.position = "top", title.hjust = 0.5))

plot_1

# for developing country
devping_life_data_1_1<- life_data%>%
  filter(life_data$Status=="Developing")%>%
  select(Country,Year,Status,Life.expectancy, Population,GDP,Total.expenditure)
head(devping_life_data_1_1,20)

plot_2 <- ggplot(devping_life_data_1_1, aes(x = GDP,y = Total.expenditure, color = Country, size = Population)) +
  geom_point() +
  guides(colour=F, size=F)+
  ggtitle("GDP vs. Total health expenditure for Year: {frame_time}") +
  ylab("Total health expenditure") +
  xlab("GDP")+theme_bw()+
  transition_time(Year)+
  theme(plot.background = element_rect(fill = "white", size = 8),
        panel.background = element_rect(fill = "white", size = 8),
        plot.margin = margin(0, 0.5, 0, 0, "cm"), 
        legend.position="bottom",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.key.size = unit(0.1, "cm"),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.1, "cm"),
        legend.margin = margin(0, 0, 0, 0, "cm"))+
  guides(color = guide_legend(direction = "vertical", title.position = "top", title.hjust = 0),
         size = guide_legend(direction = "vertical", title.position = "top", title.hjust = 0))

plot_2



# convert into gif
anim <- animate(plot_2, nframes = 100, fps = 10)
anim_save("developed_country.gif", plot_1)
anim_save("developing_country.gif", anim)


# /////////////////////////////////////////////////// //////////////////////////////
# /////////////////////////////////////////////////////////////////////////////////
# /////////////// PART 2 :SYED //////////////////////////////////////////////

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(plotly)
library(ggplot2)
library(ggthemes)
library(gganimate)
dat_dat<- read.csv(file.choose())  # SAME DATASET (LIFE EXPECTANCY)
life_dat <- dat_dat
Life<- dat_dat
Life$Total.expenditure
head(life_dat)
str(life_dat)

Life_Developing<-filter(Life,Status=="Developing")
Life_Developed<-filter(Life,Status=="Developed")
sum(is.na(Life$Status))
sum(is.na(Life$Life.expectancy))
sum(is.na(Life$Alcohol))
sum(is.na(Life$Hepatitis.B))       
sum(is.na(Life$Measles))    # no NA
sum(is.na(Life$BMI))
sum(is.na(Life$Polio))               
sum(is.na(Life$Diphtheria))    
sum(is.na(Life$HIV.AIDS)) # no NA
sum(is.na(Life$percentage.expenditure)) 

#select column for first our main objective
Data_Developed<-subset(Life_Developed,select=c(Country,Year,Status,Population,Life.expectancy,Hepatitis.B,Measles,Polio,Diphtheria,HIV.AIDS,
                                               Adult.Mortality,infant.deaths,under.five.deaths,BMI,Alcohol,Total.expenditure))
Data_Developing<-subset(Life_Developing,select=c(Country,Year,Status,Population,Life.expectancy,Hepatitis.B,Measles,Polio,Diphtheria,HIV.AIDS,
                                                 Adult.Mortality,infant.deaths,under.five.deaths,BMI,Alcohol,Total.expenditure))
theme_set(theme_minimal())
#static plot
ggplot(Life ,aes(x= Status,y=Total.expenditure, fill= Status)) + 
  geom_violin() +   geom_boxplot(width=0.1, color="blue", alpha=0.2) +
  ggtitle("Total.expenditure in Developed Country") + stat_summary(fun.y = median, geom = "point", shape = 23, size = 3, fill = "red")
# Create a grid layout with 1 row and 2 columns
grid_layout <- rbind(c(1, 2))
# Arrange the plots using grid.arrange()
grid.arrange(static1, static2, layout_matrix = grid_layout)

#mutiple regression interactive plot
ggplot1<-ggplot(Data_Developed ,aes(y=infant.deaths,x=Hepatitis.B)) + geom_point(shape=21,fill="darkgreen",color="green") +geom_smooth(method="lm",se=FALSE)+
  xlab("Hepatitis.B")+ylab("Infant Death") 
ggplot2<-ggplot(Data_Developing ,aes(y=infant.deaths,x=Hepatitis.B)) + geom_point(shape=21,fill="darkblue",color="blue") +geom_smooth(method="lm",se=FALSE)+
  xlab("Hepatitis.B")+ylab("Infant Death") + ggtitle("Infant Death in Hepatitis.B in Developing and Developed Country ")
Interactive1<-ggplotly(ggplot1)
Interactive2<-ggplotly(ggplot2)
# Arrange the plots using subplot()
subplot(Interactive1, Interactive2, nrows = 2, shareX = TRUE, shareY = TRUE)

# Save the plotly object as an HTML file
htmlFile <- "plot_2.html"
htmlwidgets::saveWidget(as_widget(A), htmlFile, selfcontained = TRUE)
#time series plot

# Plot
ggplot(Life,aes(y=Life.expectancy,x=,size=Population,colour=Country)) + 
  geom_point(alpha=0.7)  +guides(colour=F,size=F)+
  labs(x="GDP per capital",y="Life expentancy") +theme_bw() +transition_time(year) +shadow_mark(alpha=0.2,size=0.5)

# gif plot bubble plot
ggplot(Data_Developed, aes(y = infant.deaths,x = Life.expectancy, color = Country, size = Population)) +
  geom_point() +
  guides(colour=F, size=F)+
  ggtitle("infant death vs. Life expentancy in Developed Country for Year: {frame_time}") +
  ylab("Infant death") +
  xlab("Life expentancy")+theme_bw()+
  transition_time(Year)

ggplot(Data_Developing, aes(y = infant.deaths,x = Life.expectancy, color = Country, size = Population)) +
  geom_point() +
  guides(colour=F, size=F)+
  ggtitle("infant death vs. Life expentancy in Developing Country for Year: {frame_time}") +
  ylab("Infant Death") +
  xlab("Life expentancy")+theme_bw()+transition_time(Year) 




