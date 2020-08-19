# Working #####

#*20.0 Yield/Vol Ratio #####
#avg 10yr yield / 60day price volatility
#10 Yr Note Futures, Continuous Contract #2 (TY2) - CHRIS/CME_TY2
#

#*51.0 SPX vs FFR #####
SP500 <- na.locf(getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = to, auto.assign = F))
colnames(SP500) <- c("Open","High","Low","Close","Volume","Adj")

SP500vsFFR_plot <-ggplot() +
  labs(title="S&P500 vs Fed Fund Rate") +
  scale_x_date(name = NULL, date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  geom_line(data = SP500['2015/'],
            aes(x = Index,
                y = Close,
                color = "S&P500")) +
  geom_line(data = FF10_data['2015/'],
            aes(x = Index,
                y = FedFunds*1100,
                color = "Fed Funds Rate (right axis)")) +   #transforming TurnoverRatio by opposite of axis transformation
  scale_y_continuous(name = "S&P500",
                     position = c("left"),
                     limits = c(1800,3000),
                     breaks=seq(1800, 3000, by=200),
                     sec.axis = sec_axis(~./1100,                     #transforming axis
                                         name = "Fed Funds Rate")) +
  geom_hline(yintercept = 0, color = "dark gray") +
  scale_color_manual(values=c( "red", "blue")) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title=element_blank(),
    legend.margin=margin(t=-.4, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=8,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 5, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=12,
      hjust = 0),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size = 10),
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

#*52.0 Consumer Sentiment vs GDP #####
RealGDP.Growth_US <- na.locf(getSymbols("A191RL1A225NBEA", src = "FRED", from = "2000-01-01", to = to, auto.assign = F))
#Percent Change from Preceding Period, Not Seasonally Adjusted
colnames(RealGDP.Growth_US) <- c("RealGDP.Growth_US")

ConsumerSentiment_US <- to.yearly(na.locf(getSymbols("UMCSENT", src = "FRED", from = "2000-01-01", to = to, auto.assign = F)))
ConsumerSentiment_US <- ConsumerSentiment_US[,4]
colnames(ConsumerSentiment_US) <- c("ConsumerSentiment_US")
ConsumerSentiment_US <- to.yearly(ConsumerSentiment_US)
ConsumerSentiment_US <- ConsumerSentiment_US[,4]

ggplot() +
  labs(title="US Consumer Sentiment vs Real GDP Growth") +
  scale_x_date(date_labels = "%Y",
               date_breaks = "1 year",
               expand = expand_scale(mult = c(0, .01))) +
  geom_line(data = ConsumerSentiment_US['2007/'],
            aes(x = Index,
                y = ConsumerSentiment_US.Close,
                color = "ConsumerSentiment_US (LHS)")) +
  geom_line(data = RealGDP.Growth_US['2007/'],
            aes(x = Index,
                y = RealGDP.Growth_US*35,
                color = "RealGDP.Growth_US (RHS)")) +   #transforming TurnoverRatio by opposite of axis transformation
  scale_y_continuous(name = "Consumer Sentiment",
                     position = c("left"),
                     limits = c(30,110),
                     breaks=seq(30, 110, by=20),
                     sec.axis = sec_axis(~./30,                     #transforming axis
                                         name = "Real GDP Growth")) +
  geom_hline(yintercept = 0, color = "dark gray") +
  scale_color_manual(values=c("red", "blue")) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title=element_blank(),
    legend.margin=margin(t=-.4, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=8,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 5, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=12,
      hjust = 0),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size = 10),
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

#*55.0 Ratio of Leading to Coincident Index
LeadingIndex.US <- na.locf(getSymbols("USSLIND", src = "FRED", from = from, to = to, auto.assign = F))
LeadingIndex.US <- LeadingIndex.US/100
LeadingIndex.US <- LeadingIndex.US+1
LeadingIndex.US <- LeadingIndex.US["2007/"]

for (i in 1:length(LeadingIndex.US)) {
  LeadingIndex.US$Index[i] <- 100*prod(as.numeric(LeadingIndex.US$USSLIND[1:i]))
}

CoincidentIndex.US <- na.locf(getSymbols("USPHCI", src = "FRED", from = from, to = to, auto.assign = F))
CoincidentIndex.US <- CoincidentIndex.US["2007/"]
plot(LeadingIndex.US$Index/CoincidentIndex.US)

