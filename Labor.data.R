#Average Hourly Earnings of All Employees: Total Private
#Jobs Added Chart (All Employees: Total Nonfarm Payrolls)
#US Unemployment Rate
#US Participation Rate

#Average Hourly Earnings of All Employees: Total Private
Avg_Hourly_Earnings <- getSymbols("CES0500000003",src="FRED", from = from, to = to, auto.assign=FALSE)
Avg_Hourly_Earnings$YoY_Change <- Avg_Hourly_Earnings$CES0500000003 - lag(Avg_Hourly_Earnings$CES0500000003,12)
colnames(Avg_Hourly_Earnings) <- c("Avg_Hourly_Earnings", "YoY_Change")

Avg_Hourly_Earnings_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = Avg_Hourly_Earnings['2012/'],
            aes(x = Index, y = YoY_Change)) +
  xlab("") + ylab("") +
  labs(title="Average Hourly Earnings of All Employees (Wage Growth)",
       subtitle = "YoY Change ($)",
       caption = "Frequency: Monthly") +
  scale_y_continuous(position = c("left"))+
  scale_x_date(date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=12,
      hjust = 0),
    plot.caption=element_text(
      family = "Palatino",
      color = "black",
      size=8,
      vjust = 4),
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=10,
      hjust = 0),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino",
      size=8),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size=8),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

Avg_Hourly_Earnings_last <- paste(
  format(last(index(Avg_Hourly_Earnings)),"%B %Y"),": $",
  as.numeric(last(Avg_Hourly_Earnings$Avg_Hourly_Earnings)),sep = "")

Avg_Hourly_Earnings_12moChng_USD <- paste("$",as.numeric(last(Avg_Hourly_Earnings$YoY_Change)),sep="")
Avg_Hourly_Earnings_12moChng_pct <- paste(
  round(
    (as.numeric(last(Avg_Hourly_Earnings$YoY_Change))/
       as.numeric(Avg_Hourly_Earnings$Avg_Hourly_Earnings[length(Avg_Hourly_Earnings$Avg_Hourly_Earnings)-12])
    )*100,
    1),"%", sep="")

#Jobs Added Chart (All Employees: Total Nonfarm Payrolls)
getSymbols("PAYEMS",src="FRED", from = from, to = to)
colnames(PAYEMS) <- c("All_Employees")
PAYEMS$MoM_Change <- diff(PAYEMS$All_Employees, lag = 1)

Job_Growth_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = PAYEMS['2016/'],
            aes(x = Index, y = MoM_Change)) +
  xlab("") + ylab("") +
  labs(title="Jobs Added (Total Nonfarm Payrolls)",
       subtitle = "MoM Change (000s)",
       caption = "Frequency: Monthly") +
  scale_y_continuous(position = c("left"))+
  # limits = c(-25,15),
  # breaks=seq(-25,15,by=5)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "2 months",
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    plot.caption=element_text(
      family = "Palatino",
      color = "black",
      size=8,
      vjust = 4),
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=10,
      hjust = 0),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino",
      size=8),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size=8),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

Job_Growth_last <- paste(
  format(last(index(PAYEMS)),"%B %Y"),": ",
  format(as.numeric(last(PAYEMS$MoM_Change))*1000, big.mark=","),sep = "")
Job_Growth_12moAvg <- format(round(mean(as.numeric(tail(PAYEMS$MoM_Change, n = 12))),1)*1000, big.mark = ",")


#US Unemployment Rate #####
getSymbols("UNRATE", src = "FRED", from = from, to = to)
Unemployment_US <- as.xts(na.exclude(UNRATE))
colnames(Unemployment_US) <- c("Rate")

Unemployment_US_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = Unemployment_US['2006/'],
            aes(x = Index, y = Rate)) +
  xlab("") + ylab("") +
  labs(title="Unemployment Rate",
       caption="Frequency: Monthly") +                         #Removed title for plotly
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(floor(min(Unemployment_US['2005/']))-1,
                                ceiling(max(Unemployment_US['2005/']))+1),
                     breaks=seq(floor(min(Unemployment_US['2005/']))-1,
                                ceiling(max(Unemployment_US['2005/']))+1,
                                by=2)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    plot.caption=element_text(
      family = "Palatino",
      color = "black",
      size=8,
      vjust = 4),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

Unemployment_US_last <-paste(
  format(index(Unemployment_US)[length(Unemployment_US)],"%B %Y"),": ",
  as.numeric(Unemployment_US$Rate[length(Unemployment_US)]),"%",sep="")
Unemployment_US_1.prior <-paste(
  format(index(Unemployment_US)[length(Unemployment_US)-1],"%B %Y"),": ",
  as.numeric(Unemployment_US$Rate[length(Unemployment_US)-1]),"%",sep="")
Unemployment_US_2.prior <-paste(
  format(index(Unemployment_US)[length(Unemployment_US)-2],"%B %Y"),": ",
  as.numeric(Unemployment_US$Rate[length(Unemployment_US)-2]),"%",sep="")

# Unemployment_US_plotly <- ggplotly(Unemployment_US_plot)
# Unemployment_US_plotly <- layout(Unemployment_US_plotly,
#        margin = list(l = 0, r = 0, t = 40, b = 0),
#        annotations = list(
#          yref="paper",
#          xref="paper",
#          y=1.3,
#          x= 0,
#          text="U.S. Unemployment Rate",
#          font=list(size=18, family = "Palatino"),
#          showarrow=F))

#US Participation Rate #####
getSymbols("CIVPART", src = "FRED", from = from, to = to)
ParticipationRate_US <- as.xts(na.exclude(CIVPART))
colnames(ParticipationRate_US) <- c("Rate")

ParticipationRate_US_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = ParticipationRate_US['2000/'],
            aes(x = Index, y = Rate)) +
  xlab("") + ylab("") +
  labs(title="Participation Rate",
       caption="Frequency: Monthly") +                         #Removed title for plotly
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(62,68),
                     breaks=seq(62,68,by=1)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 year",
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    plot.caption=element_text(
      family = "Palatino",
      color = "black",
      size=8,
      vjust = 4),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

ParticipationRate_US_last <-paste(
  format(index(ParticipationRate_US)[length(ParticipationRate_US)],"%B %Y"),": ",
  as.numeric(ParticipationRate_US$Rate[length(ParticipationRate_US)]),"%",sep="")
ParticipationRate_US_1.prior <-paste(
  format(index(ParticipationRate_US)[length(ParticipationRate_US)-1],"%B %Y"),": ",
  as.numeric(ParticipationRate_US$Rate[length(ParticipationRate_US)-1]),"%",sep="")
ParticipationRate_US_2.prior <-paste(
  format(index(ParticipationRate_US)[length(ParticipationRate_US)-2],"%B %Y"),": ",
  as.numeric(ParticipationRate_US$Rate[length(ParticipationRate_US)-2]),"%",sep="")

# ParticipationRate_US_plotly <- ggplotly(ParticipationRate_US_plot)
#
# ParticipationRate_US_plotly <- layout(ParticipationRate_US_plotly,
#        margin = list(l = 0, r = 0, t = 40, b = 0),
#        annotations = list(
#          yref="paper",
#          xref="paper",
#          y=1.3,
#          x= 0,
#          text= "U.S. Participation Rate",
#          font=list(size=18, family = "Palatino"),
#          showarrow=F))
