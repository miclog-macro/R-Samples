# https://www.theguardian.com/environment/2018/mar/21/flooding-and-heavy-rains-rise-50-worldwide-in-a-decade-figures-show

Flood.index.notes <- "Global flooding and extreme rainfall have surged more than 50% this decade (Easac). From 2009-2018 the total cost of flood damages globally is $350bn. The increase in frequency and severity of flooding will drive demand for construction of flood-control infrastructure globally.
Additionally, rising sea levels will prompt the construction of flood-control infrastructure.
As most governments prioritize short-term economic benefits over long-term sustainable solutions to mitigate
the effects of climate change, the construction of flood-control infrastructure will
play an increasingly important role.

The Flood Index above is a float weighted index of the following companies:
Aecom (ACM), Arcadis (ARCAD.AS),Flowserve Corporation (FLS), Jacobs Engineering Group Inc. (JEC), ROTORK PLC (RTOXY), Royal Boskalis Westminster N.V. (BOKA.AS)"

# Aecom (ACM)
# Arcadis (ARCAD.AS)  (EUR)
# Flowserve Corporation (FLS)
# Jacobs Engineering Group Inc. (JEC)
# ROTORK PLC (RTOXY)
# Royal Boskalis Westminster N.V. (BOKA.AS) (EUR)

EURUSD_spot <- 1/last(na.locf(getSymbols("EUR=X", auto.assign = F)[,4]))

ACM_price <- na.locf(getSymbols("ACM", auto.assign = F)[,4])
ACM_sharesout <- 156100000
ACM_float <- 153960000
ACM_float_factor <- ACM_float/ACM_sharesout
ACM_mktcap <- ACM_sharesout*ACM_price
ACM_float <- ACM_mktcap*ACM_float_factor
ACM_float_base <- as.numeric(ACM_float["2015-01-02"])

ARCAD.AS_price <- na.locf(getSymbols("ARCAD.AS", auto.assign = F)[,4])
ARCAD.AS_price <- ARCAD.AS_price*EURUSD_spot
ARCAD.AS_sharesout <- 87660000
ARCAD.AS_float <- 70870000
ARCAD.AS_float_factor <- ARCAD.AS_float/ARCAD.AS_sharesout
ARCAD.AS_mktcap <- ARCAD.AS_sharesout*ARCAD.AS_price
ARCAD.AS_float <- ARCAD.AS_mktcap*ARCAD.AS_float_factor
ARCAD.AS_float_base <- as.numeric(ARCAD.AS_float["2015-01-02"])

FLS_price <- na.locf(getSymbols("FLS", auto.assign = F)[,4])
FLS_sharesout <- 130980000
FLS_float <- 130770000
FLS_float_factor <- FLS_float/FLS_sharesout
FLS_mktcap <- FLS_price*FLS_sharesout
FLS_float <- FLS_mktcap*FLS_float_factor
FLS_float_base <- as.numeric(FLS_float["2015-01-02"])

JEC_price <- na.locf(getSymbols("JEC", auto.assign = F)[,4])
JEC_sharesout <- 139520000
JEC_float <- 138900000
JEC_float_factor <- JEC_float/JEC_sharesout
JEC_mktcap <- JEC_sharesout*JEC_price
JEC_float <- JEC_mktcap*JEC_float_factor
JEC_float_base <- as.numeric(JEC_float["2015-01-02"])

RTOXY_price <- na.locf(getSymbols("RTOXY", auto.assign = F)[,4])
RTOXY_sharesout <- 870000000
RTOXY_float <- 216110000
RTOXY_float_factor <- RTOXY_float/RTOXY_sharesout
RTOXY_mktcap <- RTOXY_price*RTOXY_sharesout
RTOXY_float <- RTOXY_mktcap*RTOXY_float_factor
RTOXY_float_base <- as.numeric(RTOXY_float["2015-01-02"])

BOKA.AS_price <- na.locf(getSymbols("BOKA.AS", auto.assign = F)[,4])
BOKA.AS_sharesout <- 134070000
BOKA.AS_float <- 79460000
BOKA.AS_float_factor <- BOKA.AS_float/BOKA.AS_sharesout
BOKA.AS_mktcap <- BOKA.AS_price*BOKA.AS_sharesout
BOKA.AS_float <- BOKA.AS_mktcap*BOKA.AS_float_factor
BOKA.AS_float_base <- as.numeric(BOKA.AS_float["2015-01-02"])

floodindex_base.2015.01.02 <- sum(ACM_float_base,
                       ARCAD.AS_float_base,
                       FLS_float_base,
                       JEC_float_base,
                       RTOXY_float_base,
                       BOKA.AS_float_base)

Flood_index <-merge(
                ACM_float/floodindex_base.2015.01.02,
                ARCAD.AS_float/floodindex_base.2015.01.02,
                #FLS_float/floodindex_base.2015.01.02,
                #JEC_float/floodindex_base.2015.01.02,
                #RTOXY_float/floodindex_base.2015.01.02,
                #BOKA.AS_float/floodindex_base.2015.01.02,
                join = "left")

Flood_index <-merge(Flood_index,FLS_float/floodindex_base.2015.01.02,join="left")
Flood_index <-merge(Flood_index,JEC_float/floodindex_base.2015.01.02,join="left")
Flood_index <-merge(Flood_index,RTOXY_float/floodindex_base.2015.01.02,join="left")
Flood_index <-merge(Flood_index,BOKA.AS_float/floodindex_base.2015.01.02,join="left")

Flood_index$Flood_index <- Flood_index$ACM.Close +
                               Flood_index$ARCAD.AS.Close +
                               Flood_index$FLS.Close +
                               Flood_index$JEC.Close +
                               Flood_index$RTOXY.Close +
                               Flood_index$BOKA.AS.Close

Flood_index$Flood_index[2015-01-02]

SP500 <- getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", to = Sys.Date(), auto.assign = F)
colnames(SP500) <- c("Open","High","Low","Close","Volume","Adj")
SP500_flood.index <- SP500$Adj['2015/']/as.numeric(SP500$Adj["2015-01-02"])

plot(Flood_index$Flood_index['2015/']*100)
lines(SP500_flood.index*100, col = "green")

Flood_index$Flood_index['2015/']
SP500_flood.index

Flood_index_plot <- ggplot() +
  labs(title="Flood Index vs S&P500 Performance",
       subtitle = "Index = 2015",
       y = "") +
  scale_x_date(name = NULL, date_labels = "%b %y",
               date_breaks = "3 months",
               expand = expand_scale(mult = c(0, .01))) +
  geom_line(data = Flood_index['2015/'],
            aes(x = Index,
                y = Flood_index,
                color = "Flood index")) +
  geom_line(data = SP500_flood.index,
            aes(x = Index,
                y = Adj,
                color = "S&P500")) +
  scale_y_continuous(position = c("left"),
                     limits = c(0,2),
                     breaks=seq(0, 2, by=.25)) +
  geom_hline(yintercept = 0, color = "dark gray") +
  scale_color_manual(values=c( "red", "blue")) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title=element_blank(),
    legend.margin=margin(t=-.4, r=0, b=0, l=0, unit="cm"),
    legend.text = element_text(family = "Palatino",size=8,
                               margin = margin(l=2, r=10, unit = "pt")),
    plot.margin = margin(t = 8, r = 5, b = 5, l = 3, "pt"),
    plot.title=element_text(
      family = "Palatino",
      color = "black",
      size=12,
      hjust = 0),
    plot.subtitle=element_text(
      family = "Palatino",
      color = "black",
      size=10,
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
