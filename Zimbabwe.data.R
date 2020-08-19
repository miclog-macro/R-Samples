### Gross Debt Position of the General Government for Zimbabwe
### External debt stocks, short-term (DOD, current US$) - Zimbabwe
### Net Reserves by Months of Imported Goods and Services for Zimbabwe

Zimbabwe.notes <-
  "In October 2018, the IMF and World Bank allowed Zimbabwe to clear $2.2bn in arrears
to international creditors. However, the move has not been enough to quell the currency
crisis and rampant inflation. Finance Minister Ncube has been working to secure new
financing and restructure mulilateral debts. If this should happen as government debt
and short-term external debt stocks rise while net reserves fall, it should support
government bonds and the quasi-currency bond notes. Current prices offer a distressed
purchase opportunity."

Zimbabwe.Debt2GDP <- getSymbols("ZWEGGXWDGG01GDPPT", src = "FRED", from = from, to = to, auto.assign = F)
colnames(Zimbabwe.Debt2GDP) <- c("GovtDebt")

Zimbabwe.Debt2GDP_plot <-
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = Zimbabwe.Debt2GDP['2006/'],
            aes(x = Index, y = GovtDebt)) +
  xlab("") + ylab("") +
  labs(title="Zimbabwe Government Debt",
       subtitle = "% of GDP, NSA",
       caption = "Frequency: Annual") +
  scale_y_continuous(position = c("left"),
                     labels = function(x) paste0(x, "%")) +
                     # limits = c(12,30),
                     # breaks=seq(12,30,by=2)) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(index(Zimbabwe.Debt2GDP)),
                            max(index(Zimbabwe.Debt2GDP)),
                            by="1 year"),
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "NULL",
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
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

### External debt stocks, short-term (DOD, current US$) - Zimbabwe
Quandl.api_key("h8vqKTPis9Jf25z6woGz")
Zimbabwe.STdebt <- Quandl("WWDI/ZWE_DT_DOD_DSTC_CD")
Zimbabwe.STdebt <- xts(Zimbabwe.STdebt[,2], order.by = Zimbabwe.STdebt[,1])
Zimbabwe.STdebt[,1] <- Zimbabwe.STdebt[,1]/1000000000
colnames(Zimbabwe.STdebt) <- c("STdebt")

Zimbabwe.STdebt_plot <-
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = Zimbabwe.STdebt['2000/'],
            aes(x = Index, y = STdebt)) +
  xlab("") + ylab("") +
  labs(title="Zimbabwe External Debt Stocks, Short-Term ",
       subtitle = "$ Billions",
       caption = "Frequency: Annual") +
  scale_y_continuous(position = c("left"),
                      limits = c(0,3.5),
                      breaks=seq(0,3.5,by=.5)) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(index(Zimbabwe.STdebt)),
                            max(index(Zimbabwe.STdebt)),
                            by="1 year"),
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "NULL",
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
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

### Net Reserves by Months of Imported Goods and Services for Zimbabwe
Zimbabwe.NetRes <- getSymbols("ZWEBRASSMIMH", src = "FRED", from = from, to = to, auto.assign = F)
colnames(Zimbabwe.NetRes) <- c("NetReserves")
#Total reserves comprise holdings of monetary gold, special drawing rights, reserves of
# IMF members held by the IMF, and holdings of foreign exchange under the control of
# monetary authorities. The gold component of these reserves is valued at year-end
# (December 31) London prices. This item shows reserves expressed in terms of the number
# of months of imports of goods and services they could pay for [Reserves/(Imports/12)].
# https://data.worldbank.org/indicator/fi.res.totl.mo

Zimbabwe.NetRes_plot <-
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = Zimbabwe.NetRes['2000/'],
            aes(x = Index, y = NetReserves)) +
  xlab("") + ylab("") +
  labs(title="Zimbabwe Net Reserves by Months of Imported Goods and Services",
       subtitle = "Months, NSA",
       caption = "Frequency: Annual") +
  scale_y_continuous(position = c("left"),
                     limits = c(0,1),
                     breaks=seq(0,1,by=.1)) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(index(Zimbabwe.NetRes)),
                            max(index(Zimbabwe.NetRes)),
                            by="1 year"),
               expand = expand_scale(mult = c(0, .01))) +
  theme(
    legend.position = "NULL",
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=10,
      hjust = 0),
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=9),
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
