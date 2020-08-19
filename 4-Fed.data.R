#FF10 Spread
#FOMC Summary Predictions
#FedWatch Tool
#Delinquency Rate on Credit Card Loans, All Commercial Banks
#Delinquency Rate on Loans Secured by Real Estate, All Commercial Banks

#FF10 Spread ####################################
getSymbols("DGS10",src="FRED", from = from, to = to)
getSymbols("DFEDTARU",src="FRED", from = from, to = to)
FF10_data <- merge(DFEDTARU, DGS10, join = "left", fill = na.locf)
colnames(FF10_data) <- c("FedFunds","US10YR")
FF10_data$US10YR_FF_Spread <- FF10_data$US10YR - FF10_data$FedFunds

FF10_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = FF10_data["2010/"],
            aes(x = Index, y = FedFunds, color = "FedFunds")) +
  geom_line(data = FF10_data["2010/"],
            aes(x = Index, y = US10YR, color = "U.S. 10Yr")) +
  geom_line(data = FF10_data["2010/"],
            aes(x = Index, y = US10YR_FF_Spread, color = "Spread")) +
  xlab("") + ylab("") +
  labs(title="10Yr Treasury - Fed Funds Spread") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "6 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue", "red", "black")) +
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

# FF10_plotly <- ggplotly(FF10_plot)
#
# FF10_plotly <- layout(FF10_plotly,
#                       margin = list(l = 0, r = 0, t = 40, b = 0),
#                       annotations = list(
#                       yref="paper",
#                       xref="paper",
#                       y=1.3,
#                       x= 0,
#                       text= "U.S. 10Yr Treasury vs. Fed Funds Spread",
#                       font=list(size=18, family = "Palatino"),
#                       showarrow=F))
# FF10_plotly

FF10_last <- round(as.numeric(last(FF10_data$US10YR_FF_Spread)), 2)
FF10_last
FF10_last_date <- last(index(FF10_data$US10YR_FF_Spread))
FF10_last_date
FF10_10yr_last <- as.numeric(last(FF10_data$US10YR))
FF10_10yr_last
FF10_FF_last <- as.numeric(last(FF10_data$FedFunds))
FF10_FF_last

#FOMC Summary Predictions #####
getSymbols("FEDTARMD", src = "FRED", from = from, to = to)
FOMC_Projection_FFRMedian <- as.xts(FEDTARMD)
colnames(FOMC_Projection_FFRMedian) <- c("Median")

getSymbols("FEDTARCTM", src = "FRED", from = from, to = to)
FOMC_Projection_FFRMidpoint <- as.xts(FEDTARCTM)
colnames(FOMC_Projection_FFRMidpoint) <- c("Midpoint")

getSymbols("JCXFERM", src = "FRED", from = from, to = to)
FOMC_Projection_CorePCE <- as.xts(JCXFERM)
colnames(FOMC_Projection_CorePCE) <- c("CorePCE")

FOMC_Projections_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  labs(title="FOMC Projections") +
  geom_line(data = FOMC_Projection_FFRMedian,
            aes(x = Index, y = Median, color = "FFR, Median")) +
  geom_line(data = FOMC_Projection_FFRMidpoint,
            aes(x = Index, y = Midpoint, color = "FFR, Midpoint")) +
  geom_line(data = FOMC_Projection_CorePCE,
            aes(x = Index, y = CorePCE, color = "Core PCE")) +
  xlab("") + ylab("") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),  #adds % to y-axis (or change original data before plotting)
                     limits = c(1.50,3.50),
                     breaks=seq(1.50,3.50,by=.25),
                     minor_breaks = waiver()) +
  scale_color_manual(values=c( "red", "blue", "black")) +
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))

#FedWatch Tool #####
#https://www.cmegroup.com/trading/interest-rates/countdown-to-fomc.html

FedWatch_MeetingDates <- c("6/19/2019","7/31/2019", "9/18/2019", "10/30/2019",
                           "12/11/2019", "1/29/2020", "3/18/2020", "4/29/2020")
FedWatch_Rates <- c("2.5%", "2.25%", "2.0%", "2.0%", "2.0%", "1.75%",
                    "1.75%", "1.75")
FedWatch_Prob <- c("82.5%", "71.2%", "51.1%", "45.8%", "35.4%", "34.6%",
                   "33.4%", "32.5")
FedWatch_dt <- data.frame(FedWatch_MeetingDates, FedWatch_Rates, FedWatch_Prob)
colnames(FedWatch_dt) <- c("Meeting Dates", "Expected FF Rate", "Probability")
FedWatch_kable <- kable_styling(kable(FedWatch_dt, caption = "Updated: 6/17/19"),
                                bootstrap_options = c("striped", "hover", "responsive"))


#Delinquency Rate on Credit Card Loans, All Commercial Banks
DeliquencyRates_CC <- getSymbols("DRCCLACBS", src = "FRED", from = from, to = to, auto.assign = F)
colnames(DeliquencyRates_CC) <- c("Rate")

DeliquencyRates_CC_plot <-
ggplot() +
  geom_hline(yintercept = 0) +
  labs(title="Delinquency Rate on Credit Card Loans",
       subtitle="SA",
       caption="Frequency: Quarterly") +
  geom_line(data = DeliquencyRates_CC['2012/'],
            aes(x = Index, y = Rate)) +
  xlab("") + ylab("") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(1.50,3.50),
                     breaks=seq(1.50,3.50,by=.5),
                     minor_breaks = waiver()) +
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(index(GDP_US['2012/'])),
                            max(index(GDP_US)),
                            by="2 quarters"),
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("black")) +
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
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=10),
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

DeliquencyRates_CC_last <-paste(
  format(index(DeliquencyRates_CC)[length(DeliquencyRates_CC)],"%B %Y"),": ",
  round(as.numeric(DeliquencyRates_CC$Rate[length(DeliquencyRates_CC)]),2),"%",sep="")
DeliquencyRates_CC_1.prior <-paste(
  format(index(DeliquencyRates_CC)[length(DeliquencyRates_CC)-1],"%B %Y"),": ",
  round(as.numeric(DeliquencyRates_CC$Rate[length(DeliquencyRates_CC)-1]),2),"%",sep="")
DeliquencyRates_CC_2.prior <-paste(
  format(index(DeliquencyRates_CC)[length(DeliquencyRates_CC)-2],"%B %Y"),": ",
  round(as.numeric(DeliquencyRates_CC$Rate[length(DeliquencyRates_CC)-2]),2),"%",sep="")


#Delinquency Rate on Loans Secured by Real Estate, All Commercial Banks
DeliquencyRates_RE <- getSymbols("DRSREACBS", src = "FRED", from = from, to = to, auto.assign = F)
colnames(DeliquencyRates_RE) <- c("Rate")

DeliquencyRates_RE_plot <-
  ggplot() +
  geom_hline(yintercept = 0) +
  labs(title="Delinquency Rate on Loans Secured by Real Estate",
       subtitle="SA",
       caption="Frequency: Quarterly") +
  geom_line(data = DeliquencyRates_RE['2008/'],
            aes(x = Index, y = Rate)) +
  xlab("") + ylab("") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"))+
                     # limits = c(1.50,3.50),
                     # breaks=seq(1.50,3.50,by=.5),
                     # minor_breaks = waiver()) +
  scale_x_date(date_labels = "%b %Y",
               breaks = seq(min(index(DeliquencyRates_RE['2008/'])),
                            max(index(DeliquencyRates_RE)),
                            by="2 quarters"),
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("black")) +
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
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=10),
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

DeliquencyRates_RE_last <-paste(
  format(index(DeliquencyRates_RE)[length(DeliquencyRates_RE)],"%B %Y"),": ",
  round(as.numeric(DeliquencyRates_RE$Rate[length(DeliquencyRates_RE)]),2),"%",sep="")
DeliquencyRates_RE_1.prior <-paste(
  format(index(DeliquencyRates_RE)[length(DeliquencyRates_RE)-1],"%B %Y"),": ",
  round(as.numeric(DeliquencyRates_RE$Rate[length(DeliquencyRates_RE)-1]),2),"%",sep="")
DeliquencyRates_RE_2.prior <-paste(
  format(index(DeliquencyRates_RE)[length(DeliquencyRates_RE)-2],"%B %Y"),": ",
  round(as.numeric(DeliquencyRates_RE$Rate[length(DeliquencyRates_RE)-2]),2),"%",sep="")
