#US Yield Curve
#VIX CHART
#US PMI Composite Index
#VIX Compare Plot (CBOE 10yr Treasury Vol Index Spread Data)
#10 Year Breakeven Inflation
#Crude Oil Prices: West Texas Intermediate (WTI)

#US Yield Curve #####
yield.today <- adjust.previous(Sys.Date()-2, cal)
yield.1mo <- adjust.previous(Sys.Date()-30, cal)
yield.6mo <- adjust.previous(Sys.Date()-180, cal)
yield.1yr <- adjust.previous(Sys.Date()-364, cal)

getSymbols("DGS1MO", src = "FRED", from = from, to = to)
US1Mo <- as.xts(na.exclude(DGS1MO))
colnames(US1Mo) <- c("1Mo")
getSymbols("DGS3MO", src = "FRED", from = from, to = to)
US3Mo <- as.xts(na.exclude(DGS3MO))
colnames(US3Mo) <- c("3Mo")
getSymbols("DGS6MO", src = "FRED", from = from, to = to)
US6Mo <- as.xts(na.exclude(DGS6MO))
colnames(US6Mo) <- c("6Mo")
getSymbols("DGS1", src = "FRED", from = from, to = to)
US1Yr <- as.xts(na.exclude(DGS1))
colnames(US1Yr) <- c("1Yr")
getSymbols("DGS2", src = "FRED", from = from, to = to)
US2Yr <- as.xts(na.exclude(DGS2))
colnames(US2Yr) <- c("2Yr")
getSymbols("DGS5", src = "FRED", from = from, to = to)
US5Yr <- as.xts(na.exclude(DGS5))
colnames(US5Yr) <- c("5Yr")
getSymbols("DGS7", src = "FRED", from = from, to = to)
US7Yr <- as.xts(na.exclude(DGS7))
colnames(US7Yr) <- c("7Yr")
getSymbols("DGS10", src = "FRED", from = from, to = to)
US10Yr <- as.xts(na.exclude(DGS10))
colnames(US10Yr) <- c("10Yr")
getSymbols("DGS20", src = "FRED", from = from, to = to)
US20Yr <- as.xts(na.exclude(DGS20))
colnames(US20Yr) <- c("20Yr")
getSymbols("DGS30", src = "FRED", from = from, to = to)
US30Yr <- as.xts(na.exclude(DGS30))
colnames(US30Yr) <- c("30Yr")

closest_value <- function(series, date) {
  test <- which.min(abs(date - index(series)))
  return(as.numeric(series[test]))
}

Yield_dates <- c("1Mo","3Mo","6Mo","1Yr","2Yr","5Yr","7Yr","10Yr","20Yr","30Yr")

US_today_curve <- c(
  closest_value(US1Mo, yield.today),
  closest_value(US3Mo, yield.today),
  closest_value(US6Mo, yield.today),
  closest_value(US1Yr, yield.today),
  closest_value(US2Yr, yield.today),
  closest_value(US5Yr, yield.today),
  closest_value(US7Yr, yield.today),
  closest_value(US10Yr, yield.today),
  closest_value(US20Yr, yield.today),
  closest_value(US30Yr, yield.today)
)

US_1mo_curve <- c(
  closest_value(US1Mo, yield.1mo),
  closest_value(US3Mo, yield.1mo),
  closest_value(US6Mo, yield.1mo),
  closest_value(US1Yr, yield.1mo),
  closest_value(US2Yr, yield.1mo),
  closest_value(US5Yr, yield.1mo),
  closest_value(US7Yr, yield.1mo),
  closest_value(US10Yr, yield.1mo),
  closest_value(US20Yr, yield.1mo),
  closest_value(US30Yr, yield.1mo)
)

US_6mo_curve <- c(
  closest_value(US1Mo, yield.6mo),
  closest_value(US3Mo, yield.6mo),
  closest_value(US6Mo, yield.6mo),
  closest_value(US1Yr, yield.6mo),
  closest_value(US2Yr, yield.6mo),
  closest_value(US5Yr, yield.6mo),
  closest_value(US7Yr, yield.6mo),
  closest_value(US10Yr, yield.6mo),
  closest_value(US20Yr, yield.6mo),
  closest_value(US30Yr, yield.6mo)
)

US_1yr_curve <- c(
  closest_value(US1Mo, yield.1yr),
  closest_value(US3Mo, yield.1yr),
  closest_value(US6Mo, yield.1yr),
  closest_value(US1Yr, yield.1yr),
  closest_value(US2Yr, yield.1yr),
  closest_value(US5Yr, yield.1yr),
  closest_value(US7Yr, yield.1yr),
  closest_value(US10Yr, yield.1yr),
  closest_value(US20Yr, yield.1yr),
  closest_value(US30Yr, yield.1yr)
)

US_YieldCurve <- data.frame(Yield_dates,
                            US_1yr_curve,
                            US_6mo_curve,
                            US_1mo_curve,
                            US_today_curve)
colnames(US_YieldCurve) <- c("Yield_dates",
                             "1Yr Prior",
                             "6Mo Prior",
                             "1Mo Prior",
                             "Today")
US_YieldCurve <- reshape2::melt(US_YieldCurve, id.vars = 'Yield_dates')

US_YieldCurve_plot <- ggplot(data = US_YieldCurve,
                             mapping = aes(Yield_dates, value, group=factor(variable))) +
  scale_x_discrete(limits = Yield_dates) +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     #limits = c(0,3),
                     breaks=seq(0,5,by=.25)) +
  geom_line(aes(color=factor(variable))) +
  geom_point(aes(color=factor(variable))) +
  xlab("") + ylab("") +
  labs(title="U.S. Treasury Yield Curve") +
  theme(
    legend.position = "right",
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

#VIX CHART #############################################
VIX <- na.exclude(
  getSymbols("VIXCLS", src = "FRED", from = from, to = to, auto.assign = F))
colnames(VIX) <- c("Close")

VIX_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-25")), linetype="dashed") +
  annotate("text", x = as.Date("2018-08-15"), y = 40,
           label = "Retail sales + prod data signal slowdown in China",
           size = 3, family = "Palatino", hjust = 0) +
  annotate("text", x = as.Date("2018-08-15"), y = 37,
           label = "Fed lowers growth forecast and signals hikes",
           size = 3, family = "Palatino", hjust = 0) +
  geom_line(data = VIX,
            aes(x = Index, y = Close)) +
  xlab("") + ylab("") +
  labs(title="CBOE VIX") +
  scale_y_continuous(position = c("left"),
                     limits = c(5,40),
                     breaks=seq(0.00,40,by=5)) +
  scale_x_date(limits = c(Sys.Date() - 360, NA),
               date_labels = "%b %y",
               date_breaks = "1 month") +
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

VIX_last <- as.numeric(last(VIX$Close))
VIX_last
VIX_date_last <- last(index(VIX))
VIX_date_last

VIX_last <- as.numeric(last(VIX$Close))
VIX_last
VIX_1wk <- as.numeric(VIX$Close[DateAdj_7days])
VIX_1wk
VIX_1mo <- as.numeric(VIX$Close[DateAdj_1mo])
VIX_1mo

VIX_1wk_chg <- paste(round((VIX_last/VIX_1wk-1)*100,2), "%", sep="")
VIX_1wk_chg
VIX_1mo_chg <- paste(round((VIX_last/VIX_1mo-1)*100,2), "%", sep="")
VIX_1mo_chg

#US PMI Composite Index
Quandl.api_key("h8vqKTPis9Jf25z6woGz")
PMI.US <- Quandl("ISM/MAN_PMI")
PMI.US <- xts(PMI.US[,2], order.by = PMI.US[,1])
colnames(PMI.US) <- c("PMI.US")
tail(PMI.US)

PMI.US.date.last <- format(last(index(PMI.US)), "%B %Y")
PMI.US.last <- last(PMI.US)
PMI.US.3moAvg <- round(mean(tail(PMI.US,3)),2)
PMI.US.6moAvg <- round(mean(tail(PMI.US,6)),2)

PMI.US_plot <- ggplot() +
  geom_hline(yintercept = 50) +
  geom_line(data = PMI.US['2012/'],
            aes(x = Index, y = PMI.US)) +
  xlab("") + ylab("") +
  labs(title="U.S. PMI Composite Index",
       caption = "Frequency: Monthly") +
  scale_y_continuous(position = c("left"),
                     limits = c(45,65),
                     breaks=seq(45,65,by=5)) +
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(index(PMI.US)),
                            max(index(PMI.US)),
                            by="4 months"),
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

#Crude Oil Prices: West Texas Intermediate (WTI) #####
CrudeOil <- na.omit(getSymbols("DCOILWTICO", src = "FRED", from = from, to = to,auto.assign = F))
colnames(CrudeOil) <- c("Close")
CrudeOil_date_last <- last(index(CrudeOil))
CrudeOil_last <- as.numeric(last(CrudeOil$Close))
CrudeOil_1wk <- as.numeric(CrudeOil$Close[DateAdj_7days])
CrudeOil_1mo <- as.numeric(CrudeOil$Close[DateAdj_1mo])
CrudeOil_1wk_chg <- paste(round((CrudeOil_last/CrudeOil_1wk-1)*100,2), "%", sep="")
CrudeOil_1mo_chg <- paste(round((CrudeOil_last/CrudeOil_1mo-1)*100,2), "%", sep="")

CrudeOil_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = CrudeOil['2018/'],
            aes(x = Index, y = Close)) +
  xlab("") + ylab("") +
  labs(title="Crude Oil Prices: West Texas Intermediate (WTI)") +
  scale_y_continuous(position = c("left"),
                     limits = c(40,80),
                     breaks=seq(40, 80, by=10)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
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

#10 Year Breakeven Inflation ###########
getSymbols("T10YIE", src = "FRED", from = from, to = to)
BE_Inflation <- na.omit(as.xts(T10YIE))
colnames(BE_Inflation) <- c("Close")

BE_Inflation_plot <- ggplot(BE_Inflation, aes(x = Index, y = Close)) +
  geom_line(color = "black", size = .5) +
  xlab("") + ylab("") +
  ggtitle("10 Year Breakeven Inflation Rate") +
  scale_y_continuous(position = c("left"),
                     limits = c(1.6,2.2),
                     breaks=seq(1.6,2.2,by=.1),
                     minor_breaks = waiver()) +
  scale_x_date(limits = c(Sys.Date() - 360, NA),
               date_labels = "%b %y",
               date_breaks = "1 month") +
  theme(
    plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=12,
      hjust = 0),
    axis.text.y=element_text(
      color = "black",
      family = "Palatino",
      size=10),
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

BE_Inflation_date_last <- last(index(BE_Inflation))
BE_Inflation_date_last
BE_Inflation_last <- paste(as.numeric(last(BE_Inflation$Close)), "%", sep="")
BE_Inflation_last
BE_Inflation_1mo <- paste(as.numeric(BE_Inflation$Close[DateAdj_1mo]), "%", sep="")
BE_Inflation_1mo
BE_Inflation_6mo <- paste(as.numeric(BE_Inflation$Close[DateAdj_6mo]), "%", sep="")
BE_Inflation_6mo

#VIX Compare Plot (CBOE 10yr Treasury Vol Index Spread Data) #################################
getSymbols("VXTYN", src = "FRED", from = from, to = to)
CBOE_10YrTreasury_VIX <- as.xts(na.exclude(VXTYN))
colnames(CBOE_10YrTreasury_VIX) <- c("Close")

CBOE_10YrTreasury_VIX_last <- as.numeric(last(CBOE_10YrTreasury_VIX$Close))
CBOE_10YrTreasury_VIX_last
CBOE_10YrTreasury_VIX_date_last <- last(index(CBOE_10YrTreasury_VIX))
CBOE_10YrTreasury_VIX_date_last
CBOE_10YrTreasury_VIX_1wk <- as.numeric(CBOE_10YrTreasury_VIX$Close[DateAdj_7days])
CBOE_10YrTreasury_VIX_1wk
CBOE_10YrTreasury_VIX_1mo <- as.numeric(CBOE_10YrTreasury_VIX$Close[DateAdj_1mo])
CBOE_10YrTreasury_VIX_1mo
CBOE_10YrTreasury_VIX_1wk_chg <- paste(round((CBOE_10YrTreasury_VIX_last/CBOE_10YrTreasury_VIX_1wk-1)*100,2), "%", sep="")
CBOE_10YrTreasury_VIX_1wk_chg
CBOE_10YrTreasury_VIX_1mo_chg <- paste(round((CBOE_10YrTreasury_VIX_last/CBOE_10YrTreasury_VIX_1mo-1)*100,2), "%", sep="")
CBOE_10YrTreasury_VIX_1mo_chg

getSymbols("VIXCLS", src = "FRED", from = from, to = to)
VIX <- as.xts(na.exclude(VIXCLS))
colnames(VIX) <- c("Close")
VIX_last <- as.numeric(last(VIX$Close))
VIX_date_last <- last(index(VIX))
VIX_last <- as.numeric(last(VIX$Close))
VIX_1wk <- as.numeric(VIX$Close[DateAdj_7days])
VIX_1mo <- as.numeric(VIX$Close[DateAdj_1mo])
VIX_1wk_chg <- paste(round((VIX_last/VIX_1wk-1)*100,2), "%", sep="")

VIX_1mo_chg <- paste(round((VIX_last/VIX_1mo-1)*100,2), "%", sep="")

CBOE_10YrTreasury_VIX_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = CBOE_10YrTreasury_VIX['2016-12-28/'],
            aes(x = Index, y = Close, color = "10Yr Treasury ")) +
  xlab("") + ylab("") +
  labs(title="10Yr Treasury Note Volatilty Futures") +
  scale_y_continuous(position = c("left"),
                     limits = c(0,9),
                     breaks=seq(0, 9, by=2)) +
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

VIX_Compare_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = CBOE_10YrTreasury_VIX['2013-12-28/'],
            aes(x = Index, y = Close, color = "10Yr Treasury VIX")) +
  xlab("") + ylab("") +
  labs(title="S&P500 VIX vs 10Yr Treasury VIX") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               expand = expand_scale(mult = c(0, .01))) +
  geom_line(data = VIX['2013-12-19/'], aes(x = Index, y = Close, color = "S&P500 VIX")) +
  scale_color_manual(values=c( "red", "blue")) +
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

Corr_SP500Vix_10YrTrVix_30day <- round(as.numeric(cor(CBOE_10YrTreasury_VIX['2018-11-29/'], VIX['2018-11-29/'])),3)
Corr_SP500Vix_10YrTrVix_6mo <- round(as.numeric(cor(CBOE_10YrTreasury_VIX['2018-06-24/'], VIX['2018-06-24/'])),3)
Corr_SP500Vix_10YrTrVix_2yr <- round(as.numeric(cor(CBOE_10YrTreasury_VIX['2016-12-24/'], VIX['2016-12-24/'])),3)
