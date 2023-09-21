library(grid)
require(stringr)
require(rvest)
require(tidyverse)
require(dplyr)
require(car)
require(patchwork)
library(gganimate)
library(gifski)
require(ggimage)
require(ggrepel)
require(png)
require(fmsb)
require(ggpubr)
require(plyr)
require(scales)
require(data.table)
require(readxl)
require(jpeg)
AJ_Theme <- function() {
  theme(panel.background = element_rect(fill = "azure1"),
        panel.grid.major.x = element_line(colour = "lightcyan3", linetype = 2, size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y =  element_line(colour = "lightcyan3", linetype = 2, size = 0.25),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(colour = "grey", fill=NA)
  )
}
Years <- c(seq(2010,2023,1))
UVA <- c(15,16,22,23,30,30,29,23,31,35,23,18,21,25) 
Duke <- c(35,32,27,30,26,35,25,28,29,32,25,13,32,27) 
UNC <- c(20,29,32,25,24,26,33,33,26,29,14,18,29,20) 
Kansas <- c(33,35,32,31,25,27,33,31,31,26,28,21,34,28) 
Kentucky <- c(35,29,28,21,29,38,27,32,26,30,25,9,26,22) 
Villanova <- c(25,21,13,20,29,33,35,32,36,26,24,18,30,17) 
made_tourney <- c(9,12,11,13,11,11)
average_seed <- c(3.22,2.17,3.82,1.62,3.27,3.64)
championships <- c(1,2,1,1,1,2)
UVA_teams <- c("UVA","Duke","UNC","Kanas","Kentucky","Villanova")

df1 <- data.frame(Years,UVA,Duke,UNC,Kansas,Kentucky,Villanova)
df1 <- df1 %>% 
  pivot_longer(
    cols = !Years, 
    names_to = c("Team"), 
    values_to = "count"
  )
uva_logo <- "/Users/aidandowd/desktop/UVA logo.png"
duke_logo <- "/Users/aidandowd/desktop/Duke logo.png"
unc_logo <- "/Users/aidandowd/desktop/UNC logo.png"
kansas_logo <- "/Users/aidandowd/desktop/Kansas logo.png"
kentucky_logo <- "/Users/aidandowd/desktop/Kentucky logo.png"
villanova_logo <- "/Users/aidandowd/desktop/Villanova logo.png"

plot_1 <- ggplot(data = subset(df1, Team != "UVA"), aes(x=Years, y=count,group=Team)) +
  geom_line(color = "navy") +
  geom_point(color = "navy") +
  geom_point(data = subset(df1,  Team == "UVA"),color = "orange")+
  geom_line(data = subset(df1,  Team == "UVA"),color = "orange") +
  geom_image(data = subset(df1,  Team == "UVA"),aes(image=uva_logo)) +
  geom_image(data = subset(df1,  Team == "Duke"),aes(image=duke_logo)) +
  geom_image(data = subset(df1,  Team == "UNC"),aes(image=unc_logo)) +
  geom_image(data = subset(df1,  Team == "Kansas"),aes(image=kansas_logo)) +
  geom_image(data = subset(df1,  Team == "Kentucky"),aes(image=kentucky_logo)) +
  geom_image(data = subset(df1,  Team == "Villanova"),aes(image=villanova_logo)) +
  ggtitle("Wins per Team")+
  AJ_Theme() +
  ylab("Wins") +
  transition_reveal(Years)

animate(plot_1, nframe=100)

#Plot 2 - radar?
wins <- c(15,35,25)
offensive_eff <- c(108,123.4,110.8)
defensive_eff <- c(97.2,89.5,94.5)
two_pt_pct <- c(45.8,52.5,50)
three_point_pct <- c(34.5,39.5,35)

max <- c(38,max(offensive_eff)+5,max(defensive_eff)+5,max(two_pt_pct)+5,max(three_point_pct)+5)
min <- c(5,min(offensive_eff)-5,min(defensive_eff)-5,min(two_pt_pct)-5,min(three_point_pct)-5)
df2p <- data.frame(wins,offensive_eff,defensive_eff,two_pt_pct,three_point_pct)
teams <- c("UVA 2010","UVA 2019","UVA 2023")
rownames(df2p) <- teams
df2x <- rbind(df2p,max)
df2x <- rbind(df2x,min)
df2x$index <- rownames(df2x)
df2x <- df2x[order(df2x$index),]
df2x$index <- NULL

areas <- c(rgb(255, 51, 51, alpha = 125,maxColorValue = 255),
           rgb(204, 204, 0, alpha = 75,maxColorValue = 255),
           rgb(102, 178, 255, alpha = 75,maxColorValue = 255))
radarchart(df2x, axistype = 2,
           cglty = 1,       
           cglcol = "gray", 
           pcol = c(rgb(255, 51, 51, alpha = 200,maxColorValue = 255),
                    rgb(204, 204, 0, alpha = 200,maxColorValue = 255),
                    rgb(102, 178, 255, alpha = 200,maxColorValue = 255)),
           plwd = 2,        
           plty = 1,        
           pfcol = areas,
           axislabcol = "darkgrey",
           title = "UVA Basketball Performance in the Bennett Era",
           vlabels = c("Wins", "Offensive Efficiency","Defensive Efficiecy","2pt%", "3pt%"),
           vlcex = .75,
           
)
legend("topright",
       legend = teams,
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 1)



#Plot 3
bart_stats <- read_excel("/Users/aidandowd/desktop/bartorrvik.xlsx")

plot3o <- ggplot(data = bart_stats, aes(x=off_eff, y=wins)) + 
  geom_point(aes(col=conference),size=2) + 
  geom_image(data = bart_stats[38,],aes(image=uva_logo),size=.075) +
  geom_smooth(method="lm", se=F)  + stat_cor(method = "pearson") +
  labs(y="Wins", 
       x="Offensive Efficiency", 
       title="Offensive Efficiency vs. Wins",
       subtitle = "Examining the correation between defensive efficieny and wins",
       caption = "Source: barttorvik") + theme_bw()

plot3d <- ggplot(bart_stats, aes(x=def_eff, y=wins)) + 
  geom_point(aes(col=conference),size=2) + 
  geom_smooth(method="lm", se=F)  + stat_cor(method = "pearson") +
  geom_image(data = bart_stats[38,],aes(image=uva_logo),size=.075) +
  labs(y="Wins", 
       x="Defensive Efficiency", 
       title="Defensive Efficiency vs. Wins",
       subtitle = "Examining the correation between defensive efficieny and wins",
       caption = "Source: barttorvik") + theme_bw() + theme(legend.position= "None")
plot3o + plot3d



#Plot 4 - good defense to limit shot making
# half court image
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- readJPEG("/Users/aidandowd/desktop/nba_court.jpeg", native=TRUE)
court <- rasterGrob(court, interpolate = TRUE)

shot_location <-  c(rep("corner 3 L",25), rep("corner 3 R",25),rep("middle left 3",25),rep("middle right 3",25), rep("straightaway 3",25),rep("baseline 2 L",25),rep("baseline 2 R",25),rep("elbow L",25), rep("elbow R",25), rep("layup",25),rep("free throw",25))
make_miss <- c(rep("miss",3),rep("make",2),rep("miss",4),rep("make",2),rep("miss",3),"make","make",rep("miss",2),"make","miss","make",rep("miss",2),rep("make",2), 
               "miss",rep("make",2),rep("miss",3),rep("make",5),rep("miss",3),"make",rep("miss",2),rep("make",3),rep("miss",2),rep("make",3), 
               "make",rep("miss",3),"make",rep("miss",4),rep("make",3),rep("miss",5),rep("make",2),rep("miss",4),"make","make",
               rep("miss",3),"make","make",rep("miss",3),rep("make",3),rep("miss",6),rep("make",3),rep("miss",4),"make",
               "make",rep("miss",3),"make","miss","make","miss","make","miss",rep("make",2),rep("miss",3),rep("make",2),"miss","make","miss","make",rep("miss",2),rep("make",2),
               rep("miss",3),"make","miss",rep("make",3),"miss","make","miss","make","miss","make",rep("miss",3),rep("make",3),rep("miss",2),rep("make",2),"miss",
               rep("miss",3),"make",rep("miss",3),rep("make",2),rep("miss",10),"make",rep("miss",2),"make","miss","make",
               rep("miss",5),rep("make",2),"miss","make",rep("miss",8),rep("make",2),rep("miss",3),rep("make",2),"miss",
               rep("make",2),rep("miss",3),"make",rep("miss",4),rep("make",2),"miss","make","miss",rep("make",3),rep("miss",2),"make",rep("miss",4),
               rep("make",4),"miss",rep("make",8),"miss",rep("make",4),rep("miss",2),rep("make",2),"miss","make","miss",
               rep("make",3),"miss",rep("make",2),"miss",rep("make",3),rep("miss",2),rep("make",5),"miss",rep("make",5),"miss","miss")
X_coord <- c(runif(25,min=-21,max=-19.75), runif(25,min=19.75,max=21), runif(25,min=-20,max=-16), runif(25,min=16,max=20),runif(25,min=-6,max=6),runif(25,min=-16,max=-11),runif(25,min=11,max=16),runif(25,min=-10,max=-4),runif(25,min=4,max=10),runif(25,min=-3,max=3),runif(25,min=-1,max=1))
Y_coord <- c(runif(50,min=0,max=6),runif(20,min=20,max=23),runif(30,min=16,max=20),runif(25,min=25,max=28),runif(50,min=2,max=8),runif(50,min=12,max=19),runif(25,min=2,max=6),rep(16,25))
my_shots <- data.frame(shot_location,X_coord,Y_coord,make_miss)
result = list()
for (x in 1:275){
  if (my_shots$make_miss[x] == "make"){
    result[x] <- "made"}
  else if (my_shots$make_miss[x] == "miss"){
    result[x] <- "missed"}}
my_shots$result <- result

means <- ddply(my_shots, .(shot_location), summarize, 
               attempts = 25,
               makes = sum(result == "made"),
               mean_loc_x = mean(X_coord),
               mean_loc_y = mean(Y_coord))
percentage <- label_percent()(means$makes/means$attempts)
means$percentage <- (percentage)

plot_4 <- ggplot(my_shots, aes(x=X_coord, y=Y_coord,colour = factor(make_miss))) + 
  annotation_custom(court, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point(size=2.5) +
  xlim(-23.25, 23.25) +
  ylim(0, 39) +
  scale_color_manual(values = c("green","red")) + theme_bw() + guides(color = guide_legend(title = "Shot Result")) + ggtitle("My Shooting Results")

plot_4
plot_4a <- ggplot(means, aes(x=mean_loc_x, y=mean_loc_y,colour = factor(shot_location))) + 
  annotation_custom(court, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point(size=2.5) +
  geom_label(label=percentage) +
  xlim(-25, 25) +
  ylim(0, 40) + theme(legend.position = "None") + theme_bw() + theme(legend.position = "None") +
  ylab("Y Distance") + xlab("X Distance") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plot_4a


# Plot_5
my_3s <- subset(means[c(3,4,9:11),])
my_2s <- subset(means[c(1,2,5,6,8),])
my_fts <- subset(means[8,])

htmluva <- read_html("https://www.espn.com/mens-college-basketball/team/stats/_/id/258/virginia-cavaliers")
uva_players <- html_nodes(htmluva, ".ResponsiveWrapper+ .remove_capitalize .Table__TD .AnchorLink") %>% html_text()
uva_3s <- html_nodes(htmluva, ".ResponsiveWrapper+ .remove_capitalize .Table__TD:nth-child(11) span") %>% html_text()
uva_total <- html_nodes(htmluva, ".ResponsiveWrapper+ .remove_capitalize .Table__TD:nth-child(9) span") %>% html_text()
uva_fts <- html_nodes(htmluva, ".ResponsiveWrapper+ .remove_capitalize .Table__TD:nth-child(10) span") %>% html_text()
fga <- html_nodes(htmluva, ".remove_capitalize+ .remove_capitalize .Table__TD:nth-child(3) span") %>% html_text()
fgm <- html_nodes(htmluva, ".remove_capitalize+ .remove_capitalize .Table__TD:nth-child(2) span") %>% html_text()
uva_shots <- data.frame(uva_3s,uva_fts,uva_total,fga,fgm)[-13,]
uva_shots$player <- uva_players
my_row <- c(uva_3s = 43.8,uva_fts = 68,uva_total = 45.8,fga = 275,fgm = 126,player = "Aidan Dowd")
df5 <-rbind(uva_shots,my_row)
df5a <- df5[(as.numeric(df5$fga) > 100),]
df5r = melt(as.data.table(df5a), id.vars="player")
df5r$value <- as.numeric(df5r$value)
bardf <- df5r[((df5r$variable != "fga")&(df5r$variable != "fgm"))]
bardf$UVA_or_Aidan = ifelse(bardf$player == "Aidan Dowd", "yes", "no" )

plot_5 <- ggplot(bardf, aes(x=variable,y=value,fill=player)) + geom_bar(stat="identity",position="dodge")+ AJ_Theme() +
  scale_y_continuous(breaks = seq(0,100,5), limits = c(0,100)) +
  ylab("Percentage") + xlab("Shot Type") + ggtitle("Shooting Percentages of UVA Players 2023 & Me")+ 
  scale_x_discrete(labels = c("3pt FG","Free Throw","Total"))
plot_5avg <- ggplot(bardf, aes(x=variable,y=value,fill=UVA_or_Aidan)) + geom_bar(stat="identity",position="dodge")+ AJ_Theme() +
  scale_y_continuous(breaks = seq(0,100,5), limits = c(0,100)) + 
  scale_fill_manual(name = "area", values=c("red","grey50")) + AJ_Theme() +
  ylab("Percentage") + xlab("Shot Type") + ggtitle("Average UVA Shooting Percentages vs. Mine") + scale_fill_discrete(labels=c('UVA Players 2023','Aidan Dowd')) + 
  scale_x_discrete(labels = c("3pt FG","Free Throw","Total"))
plot_5 + plot_5avg


# Plot 6

description <- c("FG %", "FG %", "3FG %", "3FG %", "2FG %", "2FG %", "FGM", "FGM", "FGA", "FGA", "FTM", "FTM", "FTA", "FTA")
value <- c(130, 44, 120, 187, 172, 33, 265, 11, 326, 12, 158, 16, 144,13)
O_or_D <- c("Offense", "Defense","Offense", "Defense","Offense", "Defense","Offense", "Defense","Offense", "Defense","Offense", "Defense","Offense", "Defense")
percentile = 100*(1-value/358)
df6 <- data.frame(description,percentile,O_or_D)

ggplot(df6, aes(x=description, y= as.numeric(percentile), fill=O_or_D)) + 
  geom_bar(position = "dodge", stat="identity") + AJ_Theme() + 
  scale_fill_manual(values = c("orange", "blue")) + scale_y_continuous(breaks = seq(0,100,10)) +
  xlab("Statistic") + ylab("Percentile") + 
  labs(title = "UVA Team Statistics", subtitle = "Data Displayed as Percentiles Calculated from National Ranking")






  