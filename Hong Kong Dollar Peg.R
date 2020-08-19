# Hong Kong Dollar Peg
# https://speculatorsanonymous.com/articles/hong-kong-dollar-break/
# https://www.scmp.com/business/money/markets-investing/article/2189319/hong-kongs-monetary-authority-sells-us-dollars-prop
# https://www.scmp.com/business/money/markets-investing/article/2189319/hong-kongs-monetary-authority-sells-us-dollars-prop
# https://www.scmp.com/business/companies/article/2138536/hong-kongs-us1835-trillion-reserve-stockpile-so-big-its-actually

HKD_Peg_Notes <-
  "Since 2005, the Hong Kong Dollar has been pegged within a band of 7.75 - 7.85.
In March 2019, and in its first intervention since August 2018,
the Hong Kong Monetary Authority sold $700 million of reserves to defend the peg. Still, the HKD
remains at the top of the band. While the HKMA's accumulated surplus of the Exchange Fund
will allow it to defend the peg over the long-term, rising US interest rates and a
strengthening dollar are adding pressure to a weakened Hong Kong economy. GDP is falling,
the property market is in decline and the demand for loans is slowing (the latter
two presented above). Additionally, the slowdown in China also weighs on Hong Kong's economy,
while carry trade activity will keep the HKD weak.
To avoid further surplus expenditures, and to stimulate growth,
the HKMA must cut interest rates. This would require abandoning the peg and if so,
the HKD could see a drop of +30%."

# USDHKD ###########################
USDHKD <- na.exclude(getSymbols("DEXHKUS", src = "FRED", from = from, to = to, auto.assign=FALSE))
colnames(USDHKD) <- c("USDHKD")

USDHKD_plot <- ggplot() +
  geom_line(data = USDHKD['2016/'],
            aes(x = Index, y = USDHKD)) +
  xlab("") + ylab("") +
  labs(title="HKD / USD") +
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


# Real Residential Property Prices for Hong Kong ###########################
HKPP <- na.exclude(getSymbols("QHKR628BIS", src = "FRED", from = from, to = to, auto.assign=FALSE))
colnames(HKPP) <- c("HKPP")

HKPP_plot <- ggplot() +
  geom_line(data = HKPP['2016/'],
            aes(x = Index, y = HKPP)) +
  xlab("") + ylab("") +
  labs(title="Hong Kong Real Residential Property Prices",
       subtitle = "Index 2010=100, NSA",
       caption = "Quarterly") +
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

# Total Credit to Private Non-Financial Sector ###########################
HK_PrvtCredit <- na.exclude(getSymbols("CRDQHKAPABIS", src = "FRED", from = from, to = to, auto.assign=FALSE))
colnames(HK_PrvtCredit) <- c("HK_PrvtCredit")

HK_PrvtCredit_plot <- ggplot() +
  geom_line(data = HK_PrvtCredit['2006/'],
            aes(x = Index, y = HK_PrvtCredit)) +
  xlab("") + ylab("") +
  labs(title="Hong Kong Total Credit to Private Non-Financial Sector",
       subtitle = "Billions of HKD, NSA",
       caption = "Quarterly") +
  scale_y_continuous(position = c("left"))+
  # limits = c(700,1300),
  # breaks=seq(700,1300, by=100)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "12 months",
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
