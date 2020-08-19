#10Yr-1Yr spread vs recessions AND 10/1Yr vs Tightening Standards
#https://speculatorsanonymous.com/articles/inverted-yield-curve-recession/

US10_1_Spread.notes <-
  "Inversion of the U.S. Treasury 10-Year and 1-Year yields has predicted the past seven
recessions (top chart). The spread inverted on March 22, 2019 and then again from March 26 - 28.
The inversion was likely due to bond investors not in agreement of the Fed's view
of 'sustained expansion of economic activity' (12/19/2018 FOMC Press Release). Using
the Vanguard Long-Term Bond Index as a proxy for inflow (left chart), bond investors are buying
long-term bonds which is pushing yields down while the index has been rising through 2019.
The chart on the right aligns the 10Yr-1Yr spread with the percentage of domestic banks
tightening lending standards (based on Fed survey data). As the spread falls and
inverts, banks tighten lending conditions. Sustained tightening will significantly
weigh on economic growth. Tracking these indicators is crucial to timing the market
cycle peak and decline."

#10Yr-1Yr spread vs recessions AND 10/1Yr vs Tightening Standards ################
US10_1_Spread <- merge(US1Yr, US10Yr, join = "left", fill = na.locf)
US10_1_Spread$Spread <- US10_1_Spread$X10Yr - US10_1_Spread$US1Yr
US10_1_Spread.1990 <-US10_1_Spread["1990/"]

BankTighteningPct <- getSymbols("DRTSCILM", src = "FRED", auto.assign = F)
Vanguard_LTBond_Index <- getSymbols("VBLTX", src = "yahoo", auto.assign = F)
colnames(Vanguard_LTBond_Index) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

US10YR_1YR_Spread_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession01,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession91,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession82,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession80,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession75,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession70,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = US10_1_Spread,
            aes(x = Index, y = Spread)) +
  xlab("") + ylab("") +
  labs(title="U.S. 10-Year - 1-Year Spread ",
       caption = "Frequency: Quarterly (Tightening Standards)") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(-4,3.5),
                     breaks=seq(-4,3.5, by=1)) +
  scale_x_date(date_labels = "%Y",
               date_breaks = "2 years",
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

US10_1Sread.Vs.BankTightening_plot <- ggplot() +
  geom_rect(data = recession08,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession01,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_rect(data = recession91,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="grey",alpha=0.5) +
  geom_hline(yintercept = 0) +
  geom_line(data = US10_1_Spread.1990,
            aes(x = Index,
                y = Spread,
                color = "US 10Yr-1Yr Spread (Left Axis)")) +
  geom_line(data = BankTighteningPct,
            aes(x = Index,
                y = DRTSCILM/25,
                color = "% of Banks Tightening (Right Axis)")) +
  xlab("") + ylab("") +
  labs(title="U.S. 10-Year/1-Year Spread vs Tightening Standards",
       subtitle = "Tightening Standards: Net Percentage of Domestic Banks Tightening
       Standards for Commercial and Industrial Loans to Large and Middle-Market Firms",
       caption = "Frequency: Quarterly (Tightening Standards)") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%"),
                     limits = c(-3,3.5),
                     breaks=seq(-3,3.5, by=1),
                     sec.axis = sec_axis(~.*25,
                                         labels = function(x) paste0(x, "%"))) +         #transforming axis
  scale_x_date(date_labels = "%Y",
               date_breaks = "2 years",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","black")) +
  theme(
    legend.position = "bottom",
    legend.title=element_blank(),
    legend.margin=margin(t=-.5, r=0, b=.2, l=0, unit="cm"),
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
      size=8,
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

VBLTX.Rect <- data.frame(xmin=as.Date("2018-12-01"),
                         xmax=Sys.Date(),
                         ymin=-Inf,
                         ymax=Inf)

VBLTX_plot <- ggplot() +
  geom_rect(data = VBLTX.Rect,
            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill="light blue",alpha=0.5) +
  geom_line(data = Vanguard_LTBond_Index["2018/"],
            aes(x = Index, y = Close)) +
  xlab("") + ylab("") +
  labs(title="Vanguard Long-Term Bond Index Fund Investor Shares",
       subtitle = "Symbol: VBLTX") +
  scale_y_continuous(position = c("left"))+
                     # labels = function(x) paste0(x, "%"),
                     # limits = c(-3,3.5),
                     # breaks=seq(-3,3.5, by=1)) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "1 month",
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
      family = "Palatino"),
    axis.text.x=element_text(
      color = "black",
      family = "Palatino",
      size=8,
      angle=40,
      vjust=.8,
      hjust=0.8))
