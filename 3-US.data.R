#US Yield Curve Movement
#US GDP
#Consumer Sentiment
#Private Non-Farm Business Sector: Labor Productivity
#Nonfarm Business Sector: Real Output Per Hour of All Persons
#Manufacturing Sector: Real Output
#Retail Trade and Food Services, excluding Auto
#U.S. Current Account Balance
#PPI Pipeline Transportation of Crude Oil
#Corporate Profits by Industry (BEA: Table 6.16D)
#2% Swing Bar Chart
#S&P 500 PE Ratio by Month
#5-Year Breakeven Inflation Rate
#DXY Index
#SP500 Index Breadth - 200dma
#Rolling Correlation: 10Yr vs CPI
#Rolling Correlation: 10Yr vs Jobless Claims
#Rolling Correlation: 10Yr-2Yr Spread vs Gold
#Chicago Fed National Financial Conditions
# 
# #US Yield Curve Movement #####
# US.1mo.chng.1wk <- as.numeric(US1Mo[yield.today])-as.numeric(US1Mo[DateAdj_7days])
# US.1mo.chng.1mo <- as.numeric(US1Mo[yield.today])-as.numeric(US1Mo[DateAdj_1mo])
# US.2Yr.chng.1wk <- as.numeric(US2Yr[yield.today])-as.numeric(US2Yr[DateAdj_7days])
# US.2Yr.chng.1mo <- as.numeric(US2Yr[yield.today])-as.numeric(US2Yr[DateAdj_1mo])
# US.10Yr.chng.1wk <- as.numeric(US10Yr[yield.today])-as.numeric(US10Yr[DateAdj_7days])
# US.10Yr.chng.1mo <- as.numeric(US10Yr[yield.today])-as.numeric(US10Yr[DateAdj_1mo])
# US.30Yr.chng.1wk <- as.numeric(US30Yr[yield.today])-as.numeric(US30Yr[DateAdj_7days])
# US.30Yr.chng.1mo <- as.numeric(US30Yr[yield.today])-as.numeric(US30Yr[DateAdj_1mo])
# 
# US.TreasuryRate.Move <-
#   data.frame(
#     Term = factor(c("1 Week Change","1 Month Change"),levels=c("1 Week Change","1 Month Change")),
#     "1-Month" = c(US.1mo.chng.1wk,US.1mo.chng.1mo),
#     "2-Year" = c(US.2Yr.chng.1wk,US.2Yr.chng.1mo),
#     "10-Year" = c(US.10Yr.chng.1wk,US.10Yr.chng.1mo),
#     "30-Year" = c(US.30Yr.chng.1wk,US.30Yr.chng.1mo),check.names=F)
# US.TreasuryRate.Move <- melt(US.TreasuryRate.Move, id.vars='Term')
# colnames(US.TreasuryRate.Move) <- c("Term","Rate","Change")

US.TreasuryRate.Move_plot <-
  ggplot(US.TreasuryRate.Move, aes(fill=Term, y=Change, x=Rate,
                                   order=-as.numeric(Rate)
                                   )) +
  geom_bar(position="dodge", stat="identity",width=.8) +
  geom_text(aes(label=round(Change,2)), position=position_dodge(width=0.8),
            vjust = ifelse(US.TreasuryRate.Move$Change>= 0, -.6, 1.2),
            family="Palatino",size=4)+
  scale_fill_manual(values=c("blue","red")) +
  geom_hline(yintercept = 0)+
  scale_y_continuous(position = c("left"),
                     limits = c(-.12,.06),
                     breaks=seq(-.12,.06,by=.02)) +
  xlab("") + ylab("") +
  labs(title="U.S. Treasury Rate Movement") +
  theme(
    legend.title = element_blank(),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    legend.text=element_text(
      family = "Palatino",
      color = "black",
      size=9),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=10))

#US GDP #####
GDP_US <- na.locf(getSymbols("A191RL1Q225SBEA", src = "FRED", from = from, to = to, auto.assign = F))
colnames(GDP_US) <- c("Change")

GDP_US_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession01,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0)+
  geom_line(data = GDP_US['2000/'],
            aes(x = Index, y = Change)) +
  xlab("") + ylab("") +
  labs(title="Real GDP",
       caption = "Frequency: Quarterly",
       subtitle="Percent Change from Preceding Period, SA Annual Rate") +
  scale_y_continuous(position = c("left"),
                     limits = c(-10,8),
                     breaks=seq(-10,8,by=2),
                     labels = function(x) paste0(x, "%")) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(index(GDP_US['2000/'])),
                            max(index(GDP_US)),
                            by="1 year"),
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "NULL",
    plot.margin = margin(t = 8, r = 5, b = 2, l = 3, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=14,
      hjust = 0),
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=10),
    plot.caption=element_text(
      family = "Palatino",
      color = "black",
      size=8,
      vjust = 4),
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

GDP_US_last <-paste(
  format(index(GDP_US)[length(GDP_US)],"%B %Y"),": ",
  round(as.numeric(GDP_US$Change[length(GDP_US)]),1),"%",sep="")
GDP_US_1.prior <-paste(
  format(index(GDP_US)[length(GDP_US)-1],"%B %Y"),": ",
  round(as.numeric(GDP_US$Change[length(GDP_US)-1]),1),"%",sep="")
GDP_US_2.prior <-paste(
  format(index(GDP_US)[length(GDP_US)-2],"%B %Y"),": ",
  round(as.numeric(GDP_US$Change[length(GDP_US)-2]),1),"%",sep="")


#Consumer Sentiment #####
ConsSentiment <- na.omit(getSymbols("UMCSENT", src = "FRED", from = from, to = to,auto.assign = F))
colnames(ConsSentiment) <- c("Close")

ConsSentiment_last <-paste(
  format(index(ConsSentiment)[length(ConsSentiment)],"%B"),": ",
  as.numeric(ConsSentiment$Close[length(ConsSentiment)]),sep="")
ConsSentiment_1mo.prior <-paste(
  format(index(ConsSentiment)[length(ConsSentiment)-1],"%B"),": ",
  as.numeric(ConsSentiment$Close[length(ConsSentiment)-1]),sep="")
ConsSentiment_2mo.prior <-paste(
  format(index(ConsSentiment)[length(ConsSentiment)-2],"%B"),": ",
  as.numeric(ConsSentiment$Close[length(ConsSentiment)-2]),sep="")

ConsSentiment_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = ConsSentiment['2010/'],
            aes(x = Index, y = Close)) +
  xlab("") + ylab("") +
  labs(title="Consumer Sentiment",
       caption = "Frequency: Monthly") +
  scale_y_continuous(position = c("left"),
                     limits = c(50,110),
                     breaks=seq(50, 110, by=10)) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c( "black")) +
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

#Nonfarm Business Sector: Real Output Per Hour of All Persons #####
getSymbols("PRS85006092", src = "FRED", from = from, to = to)
RealOutputPerHrPP_US <- as.xts(na.exclude(PRS85006092))
colnames(RealOutputPerHrPP_US) <- c("Output")

RealOutputPerHrPP_US_last <-paste(
  format(index(RealOutputPerHrPP_US)[length(RealOutputPerHrPP_US)],"%B %Y"),": ",
  as.numeric(RealOutputPerHrPP_US$Output[length(RealOutputPerHrPP_US)]),"%",sep="")
RealOutputPerHrPP_US_1mo.prior <-paste(
  format(index(RealOutputPerHrPP_US)[length(RealOutputPerHrPP_US)-1],"%B %Y"),": ",
  as.numeric(RealOutputPerHrPP_US$Output[length(RealOutputPerHrPP_US)-1]),"%",sep="")
RealOutputPerHrPP_US_2mo.prior <-paste(
  format(index(RealOutputPerHrPP_US)[length(RealOutputPerHrPP_US)-2],"%B %Y"),": ",
  as.numeric(RealOutputPerHrPP_US$Output[length(RealOutputPerHrPP_US)-2]),"%",sep="")

RealOutputPerHrPP_US_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = RealOutputPerHrPP_US['2006/'],
            aes(x = Index, y = Output)) +
  xlab("") + ylab("") +
  labs(title="Real Ouput Per Hour of All Persons",
       caption = "Frequency: Quarterly",
       subtitle = "Percent Change at Annual Rate, SA") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(-5,10),
                     breaks=seq(-5,10,by=2)) +
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
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=10,
      hjust = 0),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino"),
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


#Manufacturing Sector: Real Output #####
getSymbols("PRS30006042", src = "FRED", from = from, to = to)
RealOutputManufac_US <- as.xts(na.exclude(PRS30006042))
colnames(RealOutputManufac_US) <- c("Output")

RealOutputManufac_US_last <-paste(
  format(index(RealOutputManufac_US)[length(RealOutputManufac_US)],"%B %Y"),": ",
  as.numeric(RealOutputManufac_US$Output[length(RealOutputManufac_US)]),"%",sep="")
RealOutputManufac_US_1.prior <-paste(
  format(index(RealOutputManufac_US)[length(RealOutputManufac_US)-1],"%B %Y"),": ",
  as.numeric(RealOutputManufac_US$Output[length(RealOutputManufac_US)-1]),"%",sep="")
RealOutputManufac_US_2.prior <-paste(
  format(index(RealOutputManufac_US)[length(RealOutputManufac_US)-2],"%B %Y"),": ",
  as.numeric(RealOutputManufac_US$Output[length(RealOutputManufac_US)-2]),"%",sep="")

RealOutputManufac_US_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = RealOutputManufac_US['2006/'],
            aes(x = Index, y = Output)) +
  xlab("") + ylab("") +
  labs(title="Real Ouput of Manufacturing Sector",
       subtitle = "Percent Change at Annual Rate, SA",
       caption = "Frequency: Quarterly") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(-25,15),
                     breaks=seq(-25,15,by=5)) +
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

#Private Non-Farm Business Sector: Labor Productivity #####
LaborProductivity_US <-
  na.exclude(getSymbols("MPU4910063", src = "FRED", from = from, to = to, auto.assign = F))
colnames(LaborProductivity_US) <- c("Output")

LaborProductivity_US_last <-paste(
  format(index(LaborProductivity_US)[length(LaborProductivity_US)],"%Y"),": ",
  as.numeric(LaborProductivity_US$Output[length(LaborProductivity_US)]),"%",sep="")
LaborProductivity_US_1yr.prior <-paste(
  format(index(LaborProductivity_US)[length(LaborProductivity_US)-1],"%Y"),": ",
  as.numeric(LaborProductivity_US$Output[length(LaborProductivity_US)-1]),"%",sep="")
LaborProductivity_US_2yr.prior <-paste(
  format(index(LaborProductivity_US)[length(LaborProductivity_US)-2],"%Y"),": ",
  as.numeric(LaborProductivity_US$Output[length(LaborProductivity_US)-2]),"%",sep="")

LaborProductivity_US_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = LaborProductivity_US['2000/'],
            aes(x = Index, y = Output)) +
  xlab("") + ylab("") +
  labs(title="Labor Productivity",
       subtitle = "Percent Change from a Year Ago, NSA",
       caption = "Frequency: Annual") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(-2,5),
                     breaks=seq(-2,5,by=1)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 year",
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
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
      family = "Palatino"),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size=10),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#Retail Trade and Food Services, excluding Auto (Not Seasonally Adjusted) #####
getSymbols("RSFSXMVN", src = "FRED", from = from, to = to)
RetailSales_exAuto <- as.xts(na.exclude(RSFSXMVN))
colnames(RetailSales_exAuto) <- c("Sales")

RetailSales_exAuto$YoYChange <- diff(RetailSales_exAuto, 11)/RetailSales_exAuto
RetailSales_exAuto$YoYChange <- RetailSales_exAuto$YoYChange*100
tail(RetailSales_exAuto$YoYChange)

RetailSales_exAuto_last <-paste(
  format(index(RetailSales_exAuto$YoYChange)[length(RetailSales_exAuto$YoYChange)],"%B %Y"),": ",
  round(as.numeric(RetailSales_exAuto$YoYChange[length(RetailSales_exAuto$YoYChange)]),2),"%",sep="")
RetailSales_exAuto_prior.1 <-paste(
  format(index(RetailSales_exAuto$YoYChange)[length(RetailSales_exAuto$YoYChange)-12],"%B %Y"),": ",
  round(as.numeric(RetailSales_exAuto$YoYChange[length(RetailSales_exAuto$YoYChange)-12]),2),"%",sep="")
RetailSales_exAuto_prior.2 <-paste(
  format(index(RetailSales_exAuto$YoYChange)[length(RetailSales_exAuto$YoYChange)-24],"%B %Y"),": ",
  round(as.numeric(RetailSales_exAuto$YoYChange[length(RetailSales_exAuto$YoYChange)-24]),2),"%",sep="")

RetailSales_exAuto_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = RetailSales_exAuto['2008/'],
            aes(x = Index, y = YoYChange)) +
  xlab("") + ylab("") +
  labs(title="Retail Sales Ex Auto",
       subtitle="YoY Growth",
       caption = "Frequency: Monthly") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("black")) +
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#U.S. Current Account Balance #####
CurrenAccount_US <- na.omit(getSymbols("IEABC", src = "FRED", from = from, to = to,auto.assign = F))/1000
colnames(CurrenAccount_US) <- c("Balance")
CurrenAccount_US_date_last <- last(index(CurrenAccount_US))
CurrenAccount_US_QoQChng <- paste(round((as.numeric(last(CurrenAccount_US$Balance))/
                                           as.numeric(last(lag(CurrenAccount_US$Balance,1)))-1)*100,1),"%",sep="")
CurrenAccount_US_YoYChng <- paste(round((as.numeric(last(CurrenAccount_US$Balance))/
                                           as.numeric(last(lag(CurrenAccount_US$Balance,4)))-1)*100,1),"%",sep="")

CurrenAccount_US_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = CurrenAccount_US['2008/'],
            aes(x = Index, y = Balance)) +
  xlab("") + ylab("") +
  labs(title="Current Account Balance",
       subtitle = "Billions of Dollars",
       caption = "Frequency: Quarterly") +
  scale_y_continuous(position = c("left"),
                     limits = c(-130,-70),
                     breaks=seq(-130, -70, by=10)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c( "black")) +
  theme(
    legend.position = "none",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=10,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = -3, "pt"),
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
      family = "Palatino"),
    axis.title.y=element_text(
      color = "black",
      family = "Palatino",
      size=10),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#PPI Pipeline Transportation of Crude Oil #####
PPI_OilTrans <- na.omit(getSymbols("PCU486110486110312", src = "FRED", from = from, to = to,auto.assign = F))
colnames(PPI_OilTrans) <- c("Close")
PPI_OilTrans_date_last <- last(index(PPI_OilTrans))
PPI_OilTrans_last <- as.numeric(last(PPI_OilTrans$Close))

PPI_OilTrans_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = PPI_OilTrans['2014/'],
            aes(x = Index, y = Close)) +
  xlab("") + ylab("") +
  labs(title="PPI: Pipeline Transportation of Crude Oil",
       subtitle = "Index Jun 1986=100, NSA",
       caption = "Frequency: Monthly") +
  scale_y_continuous(position = c("left"),
                     limits = c(290,350),
                     breaks=seq(290, 350, by=10)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c( "black")) +
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#Corporate Profits by Industry (BEA: Table 6.16D) ########
beaKey <- "5DDE3FF4-484C-4E0A-91D7-EBEE891FDE9A"

CorpProfits <- list(
  'UserID' = beaKey ,
  'Method' = 'GetData',
  'datasetname' = 'NIPA',
  'TableName' = 'T61600D',
  'Frequency' = 'Q',
  'Year' = 'X',
  'ResultFormat' = 'json')
CorpProfits <- beaGet(CorpProfits)
CorpProfits <- CorpProfits[,-c(1:7)]
CorpProfits <- CorpProfits[c(1)]
CorpProfits <- t(CorpProfits)
CorpProfits <- as.data.frame(CorpProfits)
CorpProf_dates <- as.yearqtr(2001 + seq(0, nrow(CorpProfits)-1)/4)   #ERROR HERE - variable not getting created
CorpProfits$dates <- CorpProf_dates

colnames(CorpProfits) <- c("Corporate_Profits", "Dates")
CorpProfits$Corporate_Profits <- CorpProfits$Corporate_Profits/1000

recession08_2 <- data.frame(xmin=CorpProfits$Dates[28],     # 2007 Q4
                            xmax=CorpProfits$Dates[34],       # 2009 Q2
                            ymin=-Inf,
                            ymax=Inf)

CorpProfits_plot <- ggplot() +
  geom_rect(data = recession08_2,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_line(data = CorpProfits, aes(x = Dates, y = Corporate_Profits),
            color = "black", size = .5) +
  geom_point(data = CorpProfits, aes(x = Dates, y = Corporate_Profits),
             size = 1) +
  scale_y_continuous(position = c("left"),
                     breaks=seq(500,3000,by=250),
                     labels = function(x) format(x, big.mark = ",",
                                                 scientific = FALSE)) +
  xlab("") + ylab("") +
  labs(title = "Corporate Profits",
       subtitle = "Total income earned from current production by U.S. corporations ($Bn)",
       caption = "Frequency: Quarterly") +
  scale_x_yearqtr(limits = c(min(CorpProfits$Dates), max(CorpProfits$Dates)),
                  n = 20,
                  format = "%Y Q%q") +
  theme(
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

CorpProfits_date_last <- last(CorpProfits$Dates)

CorpProfits_1qtrChng <- round(last(diff(CorpProfits$Corporate_Profits,1)),1)
CorpProfits_1qtrChng_Pct <- paste(round(CorpProfits_1qtrChng/
                                          head(tail(CorpProfits$Corporate_Profits,2),1)*100,1), "%", sep="")

CorpProfits_1yrChng <- round(last(diff(CorpProfits$Corporate_Profits,4)),1)
CorpProfits_1yrChng_Pct <- paste(round(CorpProfits_1yrChng/
                                         head(tail(CorpProfits$Corporate_Profits,2),1)*100,1), "%", sep="")

#2% Swing Bar Chart#####################
from2 = "1988-01-01"
getSymbols("^GSPC", from = from2, to = Sys.Date())
SP500 <- as.xts(GSPC, "%Y")
colnames(SP500) <- c("Open", "High", "Low", "Close", "Vol", "Adj")
SP500$Abs_Chng <- abs(SP500$High/SP500$Low-1)
SP500$Abs_Chng_Count <- ifelse(SP500$Abs_Chng>0.02, 1, 0)
ep <- endpoints(SP500, on = "years")
Swing_2pct <- period.apply(SP500$Abs_Chng_Count/253, INDEX = ep, FUN = sum)
Swing_2pct <- xts(round(Swing_2pct$Abs_Chng_Count*100, 1))
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
CurrYear_Months <- elapsed_months(Sys.Date(), (as.Date(ceiling_date(Sys.Date(),"year") - days(365))))
CurrYear_TradeDays <- CurrYear_Months * 21
CurrYear_Swing <- round(sum(SP500$Abs_Chng_Count["2018"]) / CurrYear_TradeDays*100, 1)
Swing_2pct["2018-10-30"] <- CurrYear_Swing
colnames(Swing_2pct) <- c("pct")
Swing_2pct_Years <- .indexyear(Swing_2pct) + 1900
Swing_2pct <- data.frame(Swing_2pct, Swing_2pct_Years)
colnames(Swing_2pct) <- c("pct", "Year")

Swing_2pct_chart <- ggplot(Swing_2pct, aes(x = Year, y = as.numeric(pct))) +
  geom_bar(position = position_dodge(width = .2),
           stat="identity") +
  #geom_text(aes(label=pct), vjust = -.5, size = 3) +
  ggtitle("% Days With Intraday Trading Swings of at Least 2% in the S&P500") +
  ylab("") +
  xlab("") +
  scale_x_continuous(breaks=Swing_2pct$Year[
    seq(1, length(Swing_2pct$Year), by = 2)]) +
  theme(
    plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
    plot.title=element_text(
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

#S&P 500 PE Ratio by Month #####
Quandl.api_key("h8vqKTPis9Jf25z6woGz")
SP500.PE.Ratio <- Quandl("MULTPL/SP500_PE_RATIO_MONTH")
SP500.PE.Ratio <- xts(SP500.PE.Ratio[,2], order.by = SP500.PE.Ratio[,1])
colnames(SP500.PE.Ratio) <- c("SP500.PE.Ratio")

SP500.PE.Ratio_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = SP500.PE.Ratio['2010/'],
            aes(x = Index, y = SP500.PE.Ratio)) +
  xlab("") + ylab("") +
  labs(title="S&P 500 PE Ratio",
       caption = "Frequency: Monthly") +
  scale_y_continuous(position = c("left"),
                     limits = c(12,28),
                     breaks=seq(12,28,by=2)) +
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(index(SP500.PE.Ratio)),
                            max(index(SP500.PE.Ratio)),
                            by="6 months"),
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "NULL",
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
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

#5-Year Breakeven Inflation Rate #####
BE_Inflation_5yr <- na.exclude(quantmod::getSymbols("T5YIE", src = "FRED", from = from, to = to, auto.assign=FALSE))
colnames(BE_Inflation_5yr) <- c("Close")

BE_Inflation_5yr_date_last <- format(last(index(BE_Inflation_5yr)),"%B %Y")
BE_Inflation_5yr_date_last
BE_Inflation_5yr_last <- paste(round(as.numeric(last(BE_Inflation_5yr$Close)),3), "%", sep="")
BE_Inflation_5yr_last

BE_Inflation_5yr_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = BE_Inflation_5yr['2005/2018'],
            aes(x = Index, y = Close, color = "Close")) +
  xlab("") + ylab("") +
  labs(title="5-Year Breakeven Inflation Rate") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("black")) +
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

#DXY Index
DXY <- na.omit(getSymbols("DXY", src = "yahoo", from = from, to = to,auto.assign = F))

DXY_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = DXY['2016/'],
            aes(x = Index, y = DXY.Close)) +
  xlab("") + ylab("") +
  labs(title="DXY") +
  scale_y_continuous(position = c("left"),
                     limits = c(700,1300),
                     breaks=seq(700,1300, by=100)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c( "black")) +
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

DXY_last <-paste(
  format(index(DXY$DXY.Close)[length(DXY$DXY.Close)],"%B %d %Y"),": ",
  round(as.numeric(DXY$DXY.Close[length(DXY$DXY.Close)]),2),sep="")
DXY_last.1 <- as.numeric(last(DXY$DXY.Close))
DXY_1mo <- as.numeric(DXY$DXY.Close[DateAdj_1mo])
DXY_6mo <- as.numeric(DXY$DXY.Close[DateAdj_6mo])
DXY_1mo_chg <- paste(round((DXY_last.1/DXY_1mo-1)*100,2), "%", sep="")
DXY_6mo_chg <- paste(round((DXY_last.1/DXY_6mo-1)*100,2), "%", sep="")

#SP500 Index Breadth - 200dma
SP500.2 <- getSymbols("^GSPC", from = from, to = to, auto.assign = F)
SP500.200dma <- rollmean(SP500.2,200,align=c("right"))
SP500.Breadth.200dma <- ((SP500.2$GSPC.Close/SP500.200dma$GSPC.Close)-1)*100

SP500.Breadth.200dma_plot <-
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = SP500.Breadth.200dma['2016/'],
            aes(x = Index, y = SP500.Breadth.200dma[,1])) +
  xlab("") + ylab("") +
  labs(title="S&P 500 Breadth",
       subtitle="Vs 200dma") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"))+
                     # limits = c(700,1300),
                     # breaks=seq(700,1300, by=100)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "2 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c( "black")) +
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

SP500.Breadth.200dma_last <-paste(
  format(index(SP500.Breadth.200dma)[length(SP500.Breadth.200dma)],"%B %d %Y"),": ",
  round(as.numeric(SP500.Breadth.200dma[,1][length(SP500.Breadth.200dma)]),2),"%",sep="")
SP500.Breadth.200dma_prior.1 <-paste(
  "1 Month Prior: ",
  round(as.numeric(SP500.Breadth.200dma[,1][length(SP500.Breadth.200dma)-22]),2),"%",sep="")
SP500.Breadth.200dma_prior.2 <-paste(
  "3 Months Prior: ",
  round(as.numeric(SP500.Breadth.200dma[,1][length(SP500.Breadth.200dma)-66]),2),"%",sep="")

#Rolling Correlation: 10Yr vs CPI
US10Yr <- as.xts(na.exclude(DGS10))
colnames(US10Yr) <- c("10Yr")
CPI_US <- na.omit(getSymbols("CPIAUCSL", src = "FRED", from = from, to = to,auto.assign = F))/1000

US10Yr.CPI.Corr.3mo <- merge(diff(US10Yr),Return.calculate(CPI_US),join="left",fill = na.locf)
US10Yr.CPI.Corr.3mo <- US10Yr.CPI.Corr.3mo["1962-02-01/"]
US10Yr.CPI.Corr.3mo <- runCor(US10Yr.CPI.Corr.3mo[,1],US10Yr.CPI.Corr.3mo[,2],3*25)
plot(US10Yr.CPI.Corr.3mo["2016/"])

US10Yr.CPI.Corr.3mo_plot <-
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = US10Yr.CPI.Corr.3mo["2015/"],
            aes(x = Index, y = US10Yr.CPI.Corr.3mo["2015/"][,1])) +
  xlab("") + ylab("") +
  labs(title="US 10-Year vs CPI",
       subtitle="Rolling Correlation (Yield diff, CPI daily return), 3 Months") +
  scale_y_continuous(position = c("left"))+
                     # limits = c(700,1300),
                     # breaks=seq(700,1300, by=100)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c( "black")) +
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#Rolling Correlation: 10Yr vs Jobless Claims
WkClaims <- na.omit(getSymbols("IC4WSA", src = "FRED", from = from, to = to,auto.assign = F))/1000
US10Yr.2 <- xts(US10Yr,order.by=index(US10Yr)+1)
US10Yr.WkClaims.Corr.3mo <- merge(diff(WkClaims),diff(US10Yr.2),join="left",fill = na.locf)
US10Yr.WkClaims.Corr.3mo <- runCor(US10Yr.WkClaims.Corr.3mo[,1],US10Yr.WkClaims.Corr.3mo[,2],3*25)

US10Yr.WkClaims.Corr.3mo_plot <-
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = US10Yr.WkClaims.Corr.3mo["2015/"],
            aes(x = Index, y = US10Yr.WkClaims.Corr.3mo["2015/"][,1])) +
  xlab("") + ylab("") +
  labs(title="US 10-Year vs Weekly Jobless Claims",
       subtitle="Rolling Correlation (Yield diff, Weekly Claims diff), 3 Months") +
  scale_y_continuous(position = c("left"))+
  # limits = c(700,1300),
  # breaks=seq(700,1300, by=100)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c( "black")) +
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#Rolling Correlation: 10Yr-2Yr Spread vs Gold
# TreasurySpread10_2Yr <- US10Yr - US2Yr
# Gold <- na.locf(getSymbols("GOLDAMGBD228NLBM", src = "FRED", from = from, to = to, auto.assign = F))
# colnames(Gold) <- c("Gold")
#
# TreasSpreadvsGold <- merge(TreasurySpread10_2Yr,Gold,join="left",fill = na.locf)
# TreasSpreadvsGold <- TreasSpreadvsGold["2010/"]
# TreasSpreadvsGold.Corr.3mo <- runCor(TreasSpreadvsGold[,1],
#                                      TreasSpreadvsGold[,2],
#                                      3*25)
# # TreasSpreadvsGold.Corr.3mo <- runCor(diff(TreasSpreadvsGold[,1],1),
#                                      diff(TreasSpreadvsGold[,2],1),
#                                      3*25)
# TreasSpreadvsGold.Corr.3mo <- runCor(log(TreasSpreadvsGold[,1]),
#                                      log(TreasSpreadvsGold[,2]),
#                                      3*25)
# plot(TreasSpreadvsGold.Corr.3mo)
# cor(TreasSpreadvsGold[,1],TreasSpreadvsGold[,2])


#Chicago Fed National Financial Conditions
# https://speculatorsanonymous.com/articles/measuring-market-liquidity/

NFCI <- na.locf(getSymbols("NFCI", src = "FRED", from = from, to = to, auto.assign = F))
colnames(NFCI) <- c("NFCI")
NFCI.LevSubIndex <- na.locf(getSymbols("NFCILEVERAGE", src = "FRED", from = from, to = to, auto.assign = F))
colnames(NFCI.LevSubIndex) <- c("NFCI.LevSubIndex")

FinancialConditions_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = NFCI['2012/'],
            aes(x = Index, y = NFCI, color = "Financial Conditions Index")) +
  geom_line(data = NFCI.LevSubIndex['2012/'],
            aes(x = Index, y = NFCI.LevSubIndex, color = "Leverage Subindex")) +
  xlab("") + ylab("") +
  labs(title="Chicago Fed National Financial Conditions",
       caption = "Weekly") +
  scale_y_continuous(position = c("left"))+
                     # limits = c(0,9),
                     # breaks=seq(0, 9, by=2)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red")) +
  theme(
    legend.position = "bottom",
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

