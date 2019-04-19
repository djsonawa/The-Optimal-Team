
# Name : Digvijay Sonawane
# Class : IST 719


##################### Poster ####################################

install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
install.packages("forcats")
library(forcats)
install.packages("stargazer")
library(stargazer)
install.packages("dplyr")
library(dplyr)
fname <- file.choose()
df <- read.csv(fname, stringsAsFactors = FALSE, header = TRUE)


##################### Data Cleaning ###############################

df$position_cat <- as.factor(df$position_cat)
df$region <- as.factor(df$region)
df$club_id <- as.numeric(as.factor(df$club))
df$age_cat <- as.factor(df$age_cat)
df$big_club <- ifelse(df$club_id %in% c(1,5,10,11,12,17),1,0)
epl$club <- as.factor(epl$club)
epl$nationality <- as.factor(epl$nationality)

summary(df)

ageCat <- function(age){
  if (age < 22)return(1)
  else if( age < 25)return(2)
  else if( age < 28)return(3)
  else if( age < 30)return(4)
  else if( age < 32)return(5)
  else return(6)
}


df <- transform(df,age_category=cut(age,breaks=c(16,21,25,28,31,38),labels = c("17-21","22-25",
                                                                               "26-28","29-31","32-38")))

View(df)




############################# Exploratory analysis ###############################################


### Who are the most valuable players in the EPL?
temp <-arrange(df,desc(df$market_value))
temp1 <- head(temp)
View(temp1)

### Who are the most popular players?
temp <-arrange(df,desc(df$page_views))
temp1 <- head(temp)
View(temp1)

### Distribution of Market Value

ggplot(df,aes(market_value))+geom_histogram(binwidth = 2.5)

#Clearly not a normal distribution, but this was expected. Teams tend to have few elite players, and a large number of low + mid value players in their *squads*. An analysis of a team's 1st 15 would probably look more like a normal distribution, since we'd be excluding low value fringe / youth players.


#### Does it look different for the Top 6?

ggplot(df,aes(market_value))+geom_histogram(binwidth = 0.5) + facet_wrap(~big_club)

#Interesting. The top 6 seem to have a spread of players, whereas the others have a large majority of players worth under 10 million (transfermrkt's valuation, not mine).

### Distribution of popularity

ggplot(df,aes(page_views))+geom_histogram(binwidth = 50)

#Similar distribution to market value, except the 2 outliers at the end - Wayne Rooney and Paul Pogba. While Rooney is already the most well-known (popular is debatable) current English footballer, he also happened to break Sir Bobby Charlton's record of most goals for Manchester United. This, alongside the constant speculation over his United career, definitely led to his heightened page views. Paul Pogba, on the other hand, is a combination of intense scrutiny (of being the world's most expensive transfer), a return to Manchester United (can definitely see people looking him up for that), and also the fact that he is a very marketable, visible player.

#### Top 6 vs the rest

ggplot(df,aes(page_views))+geom_histogram(binwidth = 50)+facet_wrap(~big_club)

#Again, the top 6 clubs seem to have a spread of players popularity. Also, Wayne Rooney is at Everton now.

### FPL Valuation

ggplot(df,aes(fpl_value,market_value,color=age))+geom_jitter(alpha=0.5,size=2)

#There seems to be nice agreement between the FPL value and transfermrkt value, despite the fact that FPL valuation is decidedly shorter term, so age would be less of a factor. I was expecting to see more players in the bottom right - older players with low market value, but high FPL value, theoretically like Petr Cech and Yaya Toure. Maybe there's a better way of highlighting that.
df %>% #filter(!club_id %in% c(3,8,13)) %>%
  #filter(age < 35) %>%
  filter(fpl_points!=0) %>%
  mutate(val_ratio=market_value/fpl_value) %>% 
  group_by(age_category,position_cat) %>%
  summarize(val_ratio=mean(val_ratio)) %>%
  mutate(position_cat=fct_recode(position_cat,"Forward"="1","Midfield"="2","Defence"="3",
                                 "Goalkeeper"="4")) %>% 
  ggplot(aes(age_category,val_ratio,fill=position_cat))+geom_bar(stat="identity") + facet_wrap(~position_cat) +
  theme_hc() +ylab("Market Value / FPL Value") + xlab("Age") 


#This seems about right. If FPL valuation were equivalent to transfer market value, we'd see a constant ratio, across age groups. But the fact that the lowest FPL value is 4 million, very young and unproven players have a low ratio. Similarly, at the other end old players have very low market values, but they may still be valuable over the next season.  
#What's interesting is how the ratio for forwards falls off a cliff beyond 32, possibly implying very low market valuations for them. 

### Market Value with Age
ggplot(df, aes(age,market_value))+geom_jitter(alpha=0.25,size=3)

#The high value players are clustered around the age of 24-32, peaking at about 27. It's important to note that this is in no way a linear relationship, which is why I use age categories in the regression model that follows. An alternative would be to do a change-point regression, which means building 2 models, one where age < threshold, one where age >= threshold.


df1 <- filter(df,!club_id %in% c(3,8,13)) %>%
  filter(new_foreign == 0)

## Popularity as a proxy for Ability

ggplot(df1,aes(fpl_value,page_views))+geom_jitter()

#There seems to be a nice, linear relationship between FPL valuation and 
#popularity, with a few notable exceptions (Wayne Rooney, sigh). Wonderful! 
#This will help in the model below.

df %>% 
  filter(club_id %in% c(1,5,10,11,12,17)) %>%
  group_by(club,position_cat) %>%
  summarise(value=sum(market_value)) %>%
  ungroup() %>% 
  mutate(position_cat=fct_recode(position_cat,"Forward"="1","Midfield"="2","Defence"="3",
                                 "Goalkeeper"="4")) %>%
  ggplot(aes(club,value,fill=position_cat))+geom_bar(stat = "identity") +facet_wrap(~position_cat)+
  theme(axis.text.x = element_text(angle = 60,hjust=0.6,vjust=0.5))



################ Dataset with predicted Market Value ##################################

fname <- file.choose()
epl <- read.csv(fname, stringsAsFactors = FALSE, header = TRUE)
View(epl)

opt_team <- data.frame(epl[c(1,3,29,35,75,95,96,97,98,103,137,214,217,261,377,378,379,380,397,421,427), ])
View(opt_team)



options(scipen=999)  # turn-off scientific notation like 1e+48

par(mfrow = c(1,2))
# Scatterplot
gg <- ggplot(opt_team, aes(x=opt_team$final.mv, y=opt_team$fpl_points)) + 
  geom_point(aes(col=opt_team$club, size=opt_team$page_views)) + 
  labs(y="Popularity", 
       x="Market Value", 
       title="Scatterplot")

plot(gg)

chelsea <- data.frame(epl[epl$club == "Chelsea", ])

gg <- ggplot(chelsea, aes(x=chelsea$final.mv, y=chelsea$fpl_points)) + 
  geom_point(aes(col=chelsea$club, size=chelsea$page_views)) + 
  geom_smooth(method="loess", se=F) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)


install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)






#############################3 
install.packages("plotrix")
library(plotrix)

# Create data for the graph.
x <-  c(1639,1910,2038,3500)
lbl <-  c("Man city","Tot","Chelsea","optimal team")


# Plot the chart.
pie3D(x,labels = lbl,explode = 0.1, main = "Pie Chart of Countries ")


par(mfrow=c(2,2))
#################3 scatter plot ##################
par(mfrow = c(1,2))


epl[-c(462,463), ]

View(epl)

d <- epl
a <- epl$fpl_points
b <- epl$market_value

d$pc <- predict(prcomp(~a+b, d))[,1]
ggplot(epl, aes(epl$fpl_points, epl$market_value,color = epl$fpl_points)) +
  geom_point(shape = 16, size = 5,show.legend = FALSE,alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")

ggplot(epl, aes(epl$fpl_points, epl$final.mv,color = epl$fpl_points)) +
  geom_point(shape = 16, size = 5,show.legend = FALSE,alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e")



##########################lollipop #############################

#value1 <- c(519,553,460,564)
don <- data.frame(
  x= c("Optimal Team","Chelsea","Tottham","Man city"), 
  value1=c(519,553,460,564), 
  value2=c(588,425,378,409)
) %>%
  rowwise() %>% 
  mutate( mymean = mean(c(value1,value2) )) %>% 
  arrange(mymean) %>% 
  mutate(x=factor(x, x))


# With a bit more style
ggplot(don) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2), color="grey") +
  geom_point( aes(x=x, y=value1), color=rgb(0.2,0.7,0.1,0.8), size=3 ) +
  geom_point( aes(x=x, y=value2), color=rgb(0.7,0.2,0.1,0.8), size=3 ) +
  coord_flip()+
  theme(
    legend.position = "none",
    panel.border = element_blank()
  )

