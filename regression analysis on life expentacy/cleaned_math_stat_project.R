# ////////////////////////// DATA CLEANING ////////////////////
life_dat<- read.csv(file.choose())
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


# imputing NA data using median value
# (personal note: mice cannot be used due to linear regression in mice, 
# this results in a X matrix that cannot be inverted and will result in error)
# thats why we decided to use median for imputation as it is more representable than mean imputation


# fill missing values of with median

imp_med2 <- life_dat %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm=TRUE))))
str(imp_med2)
head(imp_med2,10)
sum(is.na(imp_med2))
sum(is.na(life_dat))  # to compare with original data for sum of NA values
str(life_dat)
mice::md.pattern(imp_med2)


# ///////////// EDA ////////////////////
life_data<- imp_med2

# objective 1: to analyse the relationship between economic factors against life expectancy in developed and developing countries

# defining economic factors in the dataset
str(life_data)
# dependent variable = life expectancy
# independent variable = total expenditure (govt on healthcare), GDP, income composition, percentage expenditure on heatlh (based on GDP)


## ---------- descriptive analysis using graph and histogram --------------------


# looking at overall countries

ggplot(life_data, aes(x=Life.expectancy)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of Life.expectancy in Country developed")

summary(life_data$Life.expectancy) 
# we can observe that the life expectancy for the whole population is skewed to the left
# with mean of the population are 69.23 years which are slightly lesser than the median of 72 years

hist(life_data$Life.expectancy)       #relatively normal, slightly skewed to left


hist(life_data$percentage.expenditure)      #skewed to right
hist(life_data$GDP)                      #skewed to right
hist(life_data$Total.expenditure)     # relatively normal, slightly skewed to right


# looking at developing countries

devping_life_data<- life_data%>%
  filter(life_data$Status=="Developing")%>%
  select(Country,Status,Life.expectancy, percentage.expenditure,GDP,Total.expenditure)

head(devping_life_data)

ggplot(devping_life_data, aes(x=Life.expectancy)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of Life.expectancy in Country developed")


hist(devping_life_data$Life.expectancy)
summary(devping_life_data$Life.expectancy)
# we can observe that the life expectancy for the developing countries is skewed to the left
# with mean of the developing countries are 67.23 years which are slightly lesser than the median of 69.05 years




ggplot(devping_life_data, aes(x=percentage.expenditure)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of percentage.expenditure in Country developed")

hist(devping_life_data$percentage.expenditure)   #skewed to right
summary(devping_life_data$percentage.expenditure)
# we can observe that the percentage expenditure for the developing countries is skewed to the right
# with mean of the developing countries are 323.5% which are significantly more than the median of 48%
# this indicates that the average of population in developing countries has significantly spent more than the country's GDP than the median of the population that spent nlt 48% of the country's GDP on health



ggplot(devping_life_data, aes(x=GDP)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of GDP in Country developed")

hist(devping_life_data$GDP)                       #skewed to right
summary(devping_life_data$GDP)
# we can observe that the GDP per capita for the developing counties is skewed to the right
# with GDP mean of the developing countries are 3887 USD per capita which are significantly more than the median of 1766 USD per capita
# this indicates that the average of population in developing countries has better GDP per capita then the median developing population

ggplot(devping_life_data, aes(x=Total.expenditure)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of Total.expenditure in Country developed")

hist(devping_life_data$Total.expenditure)
summary(devping_life_data$Total.expenditure)
# we can observe that the total government expenditure (%) on healthcare for the developing counties is skewed to the right
# with total expnditure mean of the developing countries are 5.6% which are similar to median at 5.6%
# this shows that the mean and the median of developing countries are spending on healthcare at an equal % expenditure between the developing countries






#transforming data

hist(log(devping_life_data$percentage.expenditure))   #more normally distributed
hist(log(devping_life_data$GDP))                      #more normally distributed


# looking at developed countries

devped_life_data<- life_data%>%
  filter(life_data$Status=="Developed")%>%
  select(Country,Status,Life.expectancy, percentage.expenditure,GDP,Total.expenditure)

head(devped_life_data)

ggplot(devped_life_data, aes(x=Life.expectancy)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of Life.expectancy in Country developed")

hist(devped_life_data$Life.expectancy)
summary(devped_life_data$Life.expectancy)

ggplot(devped_life_data, aes(x=percentage.expenditure)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of Life.expectancy in Country developed")

hist(devped_life_data$percentage.expenditure)   #skewed to right
summary(devped_life_data$percentage.expenditure)


ggplot(devped_life_data, aes(x=GDP)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of Life.expectancy in Country developed")

hist(devped_life_data$GDP)                       #skewed to right
summary(devped_life_data$GDP)


ggplot(devped_life_data, aes(x=Total.expenditure)) +
  geom_density(alpha=.3, fill="blue", color="blue", linewidth=1.5)+
  geom_vline(aes(xintercept=mean(Life.expectancy)), linewidth=1)+
  ggtitle("Distribution density of Life.expectancy in Country developed")
hist(devped_life_data$Total.expenditure)
summary(devped_life_data$Total.expenditure)


# ------- looking at summary (eg: mean, median, std dev, etc... ) for overall, developed and developing countries----------

# 1) overall population (including both developing and developed countries)
summary(life_data$Life.expectancy)
# in overall, the mean life expectancy for population is 69.23 years with median of 72.10 years. 

summary(life_data$percentage.expenditure)
# in overall, the mean percentage of expenditure on health is 738% over GDP. 
# it shows that the population expenditure on health is 738% against the countries' GDP

summary(life_data$Total.expenditure)
# in overall, the mean total expenditure

summary(life_data$GDP)





# 2) developing countries
summary(devping_life_data)


# 3) developed countries
summary(devped_life_data)



# ///////////////////////////// HYPOTHESIS TESTING 7 INFERENTIAL STATISTICAL ANALYSIS /////////////////////////////

# inferential statistic analysis (multivariate analysis)

# we do significant relationship hypothesis for HA
# HA: to test whether the x variables has significant relationship with life expectancy
# we will do two tailed test because we are doing a non-directional hypothesis
# we dont have a specific directional testing, whether we want to see a positive or negative relationship,
# but we wanna see is there a significant relationship between these variables against dependent variable



# definition: economic factor = percentage expenditure + GDP + total expenditure
# H0: there is no correlation between economic factor with life expectancy               (Pearson R correlation == 0)
# HA: there is a significant relationship between economic factor and life expectancy    (Pearson R correlation != 0)



## USING PEARSON CORRELATION TEST to study the correlation between variables



# 1) cor.test : percentage expenditure & life expectancy (for both developed & developing countries)
cor.test(devping_life_data$percentage.expenditure, devping_life_data$Life.expectancy)  #developing countries
cor.test(devped_life_data$percentage.expenditure, devped_life_data$Life.expectancy)    #developed countries

# ------------------ explanation -----------------------

# for developing countries, it is found that the pearson correlation test has a value of 0.3435609, 
# which indicates that there is a positive correlation between percentage expenditure on healthcare over GDP against life expectancy in developing countries
# plus, since p-value < 2.2e-16 (< 0.05), this shows that we have enough evidence to reject the null hypothesis, and that
# there is a significant positive correlation between percentage expenditure on healthcare over GDP against life expectancy in developing countries

# for developed countries, it is found that the pearson correlation test has a value of 0.3503151, 
# which indicates that there is a positive correlation between percentage expenditure on healthcare over GDP against life expectancy in developed countries
# plus, since p-value = 3.146e-16 (< 0.05), this shows that we have enough evidence to reject the null hypothesis, and that
# there is a significant positive correlation between percentage expenditure on healthcare over GDP against life expectancy in developed countries

# -----------------------------------------------------------------------------



# 2) cor.test : GDP & life expectancy 
cor.test(devping_life_data$GDP, devping_life_data$Life.expectancy)  #developing countries
cor.test(devped_life_data$GDP, devped_life_data$Life.expectancy)    #developed countries

# ------------------ explanation -----------------------

# for developing countries, it is found that the pearson correlation test has a value of 0.3596207, 
# which indicates that there is a positive correlation between a countries GDP against life expectancy in developing countries
# plus, since p-value < 2.2e-16 (< 0.05), this shows that we have enough evidence to reject the null hypothesis, and that
# there is a significant positive correlation between GDP against life expectancy in developing countries

# for developed countries, it is found that the pearson correlation test has a value of 0.3684408, 
# which indicates that there is a positive correlation between GDP against life expectancy in developed countries
# plus, since p-value < 2.2e-16 (< 0.05), this shows that we have enough evidence to reject the null hypothesis, and that
# there is a significant positive correlation between  GDP against life expectancy in developed countries

# -----------------------------------------------------------------------------



# 3) cor.test : total government expenditure on healthcare & life expectancy 
cor.test(devping_life_data$Total.expenditure, devping_life_data$Life.expectancy)  #developing countries
cor.test(devped_life_data$Total.expenditure, devped_life_data$Life.expectancy)    #developed countries

# ------------------ explanation -----------------------

# for developing countries, it is found that the pearson correlation test has a value of 0.09225557, 
# which indicates that there is a positive correlation between government total expenditure on healthcare against life expectancy in developing countries 
# plus, since p-value = 5.33e-06 (< 0.05), this shows that we have enough evidence to reject the null hypothesis, and that
# there is a significant positive correlation between GDP against life expectancy in developing countries

# for developed countries, it is found that the pearson correlation test has a value of 0.05392431, 
# which indicates that there is a positive correlation between government total expenditure on healthcare against life expectancy in developed countries
# plus, since p-value < 2.2e-16 (< 0.05), this shows that we have enough evidence to reject the null hypothesis, and that
# there is a significant positive correlation between  GDP against life expectancy in developed countries


# it is to note that the positive correlation is relatively weak as the correlation value is near zero

# -----------------------------------------------------------------------------


# ///////////////////////////// MULTIPLE LINEAR REGRESSION ANALYSIS /////////////////////////////

# we believe that multiple economical variables will have an effect on life expectancy for both developing and developed countries
# therefore, we are performing multiple linear regression of the economical variables for both developing and developed countires


# multi linear regression plot for developing countries
par(mfrow=c(2,2))
fit <- lm(Life.expectancy~percentage.expenditure+GDP+Total.expenditure , data=devping_life_data)
#plot(fit)
summary (fit)


# multi linear regression plot for developed countries
fit2 <- lm(Life.expectancy~percentage.expenditure+GDP+Total.expenditure , data=devped_life_data)
#plot(fit2)
summary (fit2)
fit3<- lm(Life.expectancy~GDP , data=devped_life_data)
summary (fit3)

# ----------------------------------------------------------------------------------------------

#since linear regression are based on having normal data, the skewed data will be transformed


# ------------ MLR for developing countries ------------------

#creating new dataset with transformed data
#trans_devping<- devping_life_data

#transforming data

#trans_devping$percentage.expenditure <- log(devping_life_data$percentage.expenditure)  #more normally distributed
#trans_devping$GDP<- log(devping_life_data$GDP)                     #more normally distributed

#the data is relatively more normally distributed
#hist(trans_devping$Life.expectancy)
#hist(trans_devping$percentage.expenditure)
#hist(trans_devping$GDP)
#hist(trans_devping$Total.expenditure)


# developing countries new dataframe for MLR
#MLR_devping<- trans_devping%>%
#  select(Life.expectancy, percentage.expenditure,GDP,Total.expenditure)

# ---------------------------------------------------------------------------------------------


#install.packages("GGally")
#library(GGally)
#ggpairs(MLR_devping)

#plot(Life.expectancy~percentage.expenditure+GDP+Total.expenditure , data=MLR_devping)




