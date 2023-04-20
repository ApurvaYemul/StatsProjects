##loading required libraries
library(readr)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(plotrix)
library(plotly)
summer_data <- read_csv("D:/Devi/Projects/TY projects/R project/summer data.csv")
summer_data
## Making a subset of data; we are considering the data from the year 1948 on wards 
SS = summer_data%>%filter(Year >=1948)
SS

##Table displaying number of medals given each year
Table_1 = SS%>%group_by(year = Year)%>%summarise(Total_sport = length(unique(Sport)),medal = n())
Table_1
par(bg = "Pink")
plot(Table_1$year,Table_1$medal ,main="Sports over years",xlab = "Year",ylab = "Total medals", 
     type="o",col="Black")

##Functions that returns the year and number of medals won when the country code is entered
gold = function(x) {
  x = readline(prompt = "Enter Name of the country")
  SS%>%filter(Medal=="Gold",Country==x)%>%group_by(Country,Year)%>%summarise(Medal=n(),.groups = "keep")
}
gold()
IND

silver = function(x) {
  x = readline(prompt = "Enter Name of the country")
  SS%>%filter(Medal=="Silver",Country==x)%>%group_by(Country,Year)%>%summarise(Medal=n(),.groups = "keep")
}
silver()
IND

bronze = function(x) {
  x = readline(prompt = "Enter Name of the country")
  SS%>%filter(Medal=="Bronze",Country==x)%>%group_by(Country,Year)%>%summarise(Medal=n(),.groups = "keep")
}
bronze()
IND

##function that returns the medals won by men and women when country code is entered
gender_wise = function(x) {
  x = readline(prompt = "Enter Name of the country: ")
  SS%>%filter(Country==x)%>%group_by(Country,Gender,Year)%>%summarise(Medal=n(), .groups = "keep")%>%spread(Gender,Medal)
}
gender_wise()
IND

##pivot table that displays medals won each year gender wise
A = SS%>%filter(Medal =="Gold")%>%group_by(Gender,Year)%>%summarise(Medal = n(),
                      .groups = "keep")%>%spread(Gender,Medal)
View(A)
B = SS%>%filter(Medal == "Silver")%>%group_by(Gender,Year)%>%summarise(Medal = n(), 
                      .groups = "keep")%>%spread(Gender,Medal)
View(B)
C = SS%>%filter(Medal == "Bronze")%>%group_by(Gender,Year)%>%summarise(Medal = n(),
                      .groups = "keep")%>%spread(Gender,Medal)
View(C)
D = merge(A,B,by = "Year")
E = merge(D,C, by = "Year")
colnames(E) = c('Year','Men_G','Women_G','Men_S','Women_S','Men_B','Women_B')
E
Table_Final = t(E)
Table_Final = Table_Final[-1,]
colnames(Table_Final) = c(1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,
                          1992,1996,2000,2004,2008,2012)
Table_Final
par(bg = "Grey")
barplot(Table_Final, main="Medal distribution for each olympic year", beside=TRUE, xlab = "Year",
        ylab = "Medals",col = c("Dark Green", "Green", "Dark blue","Blue","Red","Orange"))
        
## pie chart displaying number of medals won by each country all these years
Z = SS%>%filter(Medal == "Gold")%>%group_by(Country)%>%summarise(Medal = n())
Y = Z[with(Z,order(Medal, decreasing = TRUE)),]
p_tableG = head(Y,20)
p_tableG
pie3D(p_tableG$Medal,labels = p_tableG$Country, explode = 0.1, 
      main = "Pie chart displaying total gold medals won by top 20 countries from 1948-2012")
X = SS%>%filter(Medal == "Silver")%>%group_by(Country)%>%summarise(Medal = n())
W =X[with(X,order(Medal, decreasing = TRUE)),]
W
p_tableS = head(W,20)
p_tableS
pie3D(p_tableS$Medal,labels = p_tableS$Country, explode = 0.1, 
      main = "Pie chart displaying total silver medals won by top 20 countries from 1948-2012")
V = SS%>%filter(Medal == "Bronze")%>%group_by(Country)%>%summarise(Medal = n())
U = V[with(V,order(Medal, decreasing = TRUE)),]
p_tableB = head(U,20)
p_tableB
pie3D(p_tableB$Medal,labels = p_tableB$Country, explode = 0.1, 
      main = "Pie chart displaying total bronze medals won by top 20 countries from 1948-2012")

##for correlation and regression analysis
P = SS%>%group_by(Country)%>%summarise(Medal = n())
p_table = P[with(P,order(Medal, decreasing = TRUE)),]
p_table
Gdp_data = read_csv("D:/Devi/Projects/TY projects/R project/GDP data.csv")
Gdp_table = Gdp_data%>%select(c(Code,Population,`GDP per Capita`))
R_table = merge(p_table,Gdp_table,by.x = "Country", by.y = "Code", all = F)
R_table
cor.test(R_table$Medal,R_table$`GDP per Capita`)
cor.test(R_table$Medal,R_table$Population)
ggplot(R_table,aes(x= Population, y = Medal))+geom_point()
summary(lm(R_table$Medal~R_table$Population))
plot(lm(R_table$Medal~R_table$Population))

##Table that displays sport and its disciplines with number of countries participated
SD_table1 = SS%>%group_by(sport = Sport)%>%summarise(Total_disciplines = unique(Discipline),.groups = "keep" )
SD_table1

Q=SS%>%group_by(Discipline)%>%summarise(countries_participated = length(unique(Country)))
merge(SD_table1,Q,by.x = "Total_disciplines", by.y = "Discipline")


##function that returns the summary of medals won when country code is entered
medal_sum = function(x) {
  x = readline(prompt = "Enter Country Code: ")
  sum=SS%>%filter(Country==x)%>%group_by(Country,Year)%>%summarise(Medal = n(),
                                        .groups = "keep")%>%spread(Country,Medal)
  summary(sum)
}
medal_sum()
IND

##general conclusions 
##which country won most medals
H = SS%>%group_by(Country)%>%summarise(Medal = n())
H[which.max(H$Medal),]
##which countries have been participating since 1948 and which joined in later
C1 = SS%>%filter(Year == 1948)%>%summarise(Country = unique(Country))
C2 = SS%>%filter(Year == 2012)%>%summarise(Country = unique(Country))
View(intersect(C1,C2))
##From which year women events were included
summer_data%>%filter(Gender == "Women") ##Women started participating since 1900
##Men Women ratio in 1948 and in 2012
SS%>%filter(Year == 1948)%>%count(Gender)
SS%>%filter(Year == 2012)%>%count(Gender)
##Which discipline has maximum events
M3=SS%>%group_by(Discipline)%>%summarise(length(unique(Event)))
M4=SS%>%group_by(Discipline)%>%summarise(unique(Event))
M3[which.max(M3$`length(unique(Event))`),]
#Which city hosted Olympics in which year and for how many times
O = summer_data%>%group_by(Year,City)%>%summarise()
as.data.frame(table(O$City))


