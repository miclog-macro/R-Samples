#Currency Correlations
### USDEUR ###
### USDGBP ###
### JPYUSD ###
### CHFUSD ###
### AUDUSD ###
### RUBUSD ###
### ARSUSD ###
### Other and Correlations ###
start_ccy_date <- Sys.Date()-365*5
end_ccy_date <- Sys.Date()

### USDEUR ###
getSymbols("EUR=X", from = start_ccy_date, to = end_ccy_date)
USDEUR <- na.locf(`EUR=X`)
USDEUR.50dayMA<-rollmean(1/USDEUR,50,align=c("right"))
USDEUR <- merge(USDEUR,USDEUR.50dayMA[,4])
USDEUR.200dayMA<-rollmean(1/USDEUR,200,align=c("right"))
USDEUR <- merge(USDEUR,USDEUR.200dayMA[,4])
colnames(USDEUR) <- c("Open", "High", "Low", "Close", "Vol", "Adj","50dma","200dma")

USDEUR_plot <- ggplot() +
  geom_line(data = USDEUR['2018/'],
            aes(x = Index, y = 1/Close)) +
  geom_line(data = USDEUR['2018/'],
            aes(x = Index, y = `50dma`, color="50dma")) +
  geom_line(data = USDEUR['2018/'],
            aes(x = Index, y = `200dma`, color="200dma")) +
  xlab("") + ylab(" ") +
  labs(title="USD / EUR") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
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

USDEUR_last <- paste(
  format(last(index(USDEUR)),"%B %d"),": ",round(as.numeric(last(1/USDEUR$Close)),3),sep="")
USDEUR_last.1 <- as.numeric(last(1/USDEUR$Close))
USDEUR_1wk <- as.numeric(1/USDEUR$Close[DateAdj_7days])
USDEUR_1mo <- as.numeric(1/USDEUR$Close[DateAdj_1mo])
USDEUR_1wk_chg <- paste(round((USDEUR_last.1/USDEUR_1wk-1)*100,2), "%", sep="")
USDEUR_1mo_chg <- paste(round((USDEUR_last.1/USDEUR_1mo-1)*100,2), "%", sep="")


### USDGBP ###
getSymbols("GBP=X", from = start_ccy_date, to = end_ccy_date)
USDGBP <- na.locf(`GBP=X`)
USDGBP.50dayMA<-rollmean(1/USDGBP,50,align=c("right"))
USDGBP <- merge(USDGBP,USDGBP.50dayMA[,4])
USDGBP.200dayMA<-rollmean(1/USDGBP,200,align=c("right"))
USDGBP <- merge(USDGBP,USDGBP.200dayMA[,4])
colnames(USDGBP) <- c("Open", "High", "Low", "Close", "Vol", "Adj","50dma","200dma")

USDGBP_plot <- ggplot() +
  geom_line(data = USDGBP['2018/'],
            aes(x = Index, y = 1/Close)) +
  geom_line(data = USDGBP['2018/'],
            aes(x = Index, y = `50dma`, color="50dma")) +
  geom_line(data = USDGBP['2018/'],
            aes(x = Index, y = `200dma`, color="200dma")) +
  xlab("") + ylab(" ") +
  labs(title="USD / GBP") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
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

USDGBP_last <- paste(
  format(last(index(USDGBP)),"%B %d"),": ",round(as.numeric(last(1/USDGBP$Close)),3),sep="")
USDGBP_last.1 <- as.numeric(last(1/USDGBP$Close))
USDGBP_1wk <- as.numeric(1/USDGBP$Close[DateAdj_7days])
USDGBP_1mo <- as.numeric(1/USDGBP$Close[DateAdj_1mo])
USDGBP_1wk_chg <- paste(round((USDGBP_last.1/USDEUR_1wk-1)*100,2), "%", sep="")
USDGBP_1mo_chg <- paste(round((USDGBP_last.1/USDEUR_1mo-1)*100,2), "%", sep="")

### JPYUSD ###
getSymbols("JPY=X", from = start_ccy_date, to = end_ccy_date)
USDJPY <- na.locf(`JPY=X`)
USDJPY.50dayMA<-rollmean(USDJPY,50,align=c("right"))
USDJPY <- merge(USDJPY,USDJPY.50dayMA[,4])
USDJPY.200dayMA<-rollmean(USDJPY,200,align=c("right"))
USDJPY <- merge(USDJPY,USDJPY.200dayMA[,4])
colnames(USDJPY) <- c("Open", "High", "Low", "Close", "Vol", "Adj","50dma","200dma")

USDJPY_plot <- ggplot() +
  geom_line(data = USDJPY['2018/'],
            aes(x = Index, y = Close)) +
  geom_line(data = USDJPY['2018/'],
            aes(x = Index, y = `50dma`, color="50dma")) +
  geom_line(data = USDJPY['2018/'],
            aes(x = Index, y = `200dma`, color="200dma")) +
  xlab("") + ylab(" ") +
  labs(title="JPY / USD") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
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

USDJPY_last <- paste(
  format(last(index(USDJPY)),"%B %d"),": ",round(as.numeric(last(USDJPY$Close)),3),sep="")
USDJPY_last.1 <- as.numeric(last(USDJPY$Close))
USDJPY_1wk <- as.numeric(USDJPY$Close[DateAdj_7days])
USDJPY_1mo <- as.numeric(USDJPY$Close[DateAdj_1mo])
USDJPY_1wk_chg <- paste(round((USDJPY_last.1/USDJPY_1wk-1)*100,2), "%", sep="")
USDJPY_1mo_chg <- paste(round((USDJPY_last.1/USDJPY_1mo-1)*100,2), "%", sep="")

### CHFUSD ###
getSymbols("CHF=X", from = start_ccy_date, to = end_ccy_date)
USDCHF <- na.locf(`CHF=X`)
USDCHF.50dayMA<-rollmean(USDCHF,50,align=c("right"))
USDCHF <- merge(USDCHF,USDCHF.50dayMA[,4])
USDCHF.200dayMA<-rollmean(USDCHF,200,align=c("right"))
USDCHF <- merge(USDCHF,USDCHF.200dayMA[,4])
colnames(USDCHF) <- c("Open", "High", "Low", "Close", "Vol", "Adj","50dma","200dma")

USDCHF_plot <- ggplot() +
  geom_line(data = USDCHF['2018/'],
            aes(x = Index, y = Close)) +
  geom_line(data = USDCHF['2018/'],
            aes(x = Index, y = `50dma`, color="50dma")) +
  geom_line(data = USDCHF['2018/'],
            aes(x = Index, y = `200dma`, color="200dma")) +
  xlab("") + ylab(" ") +
  labs(title="CHF / USD") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
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

USDCHF_last <- paste(
  format(last(index(USDCHF)),"%B %d"),": ",round(as.numeric(last(USDCHF$Close)),3),sep="")
USDCHF_last.1 <- as.numeric(last(USDCHF$Close))
USDCHF_1wk <- as.numeric(USDCHF$Close[DateAdj_7days])
USDCHF_1mo <- as.numeric(USDCHF$Close[DateAdj_1mo])
USDCHF_1wk_chg <- paste(round((USDCHF_last.1/USDCHF_1wk-1)*100,2), "%", sep="")
USDCHF_1mo_chg <- paste(round((USDCHF_last.1/USDCHF_1mo-1)*100,2), "%", sep="")

### AUDUSD ###
getSymbols("AUD=X", from = start_ccy_date, to = end_ccy_date)
AUDUSD <- na.locf(`AUD=X`)
AUDUSD <- 1/AUDUSD
AUDUSD.50dayMA<-rollmean(AUDUSD,50,align=c("right"))
AUDUSD <- merge(AUDUSD,AUDUSD.50dayMA[,4])
AUDUSD.200dayMA<-rollmean(AUDUSD,200,align=c("right"))
AUDUSD <- merge(AUDUSD,AUDUSD.200dayMA[,4])
colnames(AUDUSD) <- c("Open", "High", "Low", "Close", "Vol", "Adj","50dma","200dma")

AUDUSD_plot <- ggplot() +
  geom_line(data = AUDUSD['2018/'],
            aes(x = Index, y = Close)) +
  geom_line(data = AUDUSD['2018/'],
            aes(x = Index, y = `50dma`, color="50dma")) +
  geom_line(data = AUDUSD['2018/'],
            aes(x = Index, y = `200dma`, color="200dma")) +
  xlab("") + ylab(" ") +
  labs(title="USD / AUD") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
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

AUDUSD_last <- paste(
  format(last(index(AUDUSD)),"%B %d"),": ",round(as.numeric(last(AUDUSD$Close)),3),sep="")
AUDUSD_last.1 <- as.numeric(last(AUDUSD$Close))
AUDUSD_1wk <- as.numeric(AUDUSD$Close[DateAdj_7days])
AUDUSD_1mo <- as.numeric(AUDUSD$Close[DateAdj_1mo])
AUDUSD_1wk_chg <- paste(round((AUDUSD_last.1/AUDUSD_1wk-1)*100,2), "%", sep="")
AUDUSD_1mo_chg <- paste(round((AUDUSD_last.1/AUDUSD_1mo-1)*100,2), "%", sep="")

### RUBUSD ###
getSymbols("RUB=X", from = start_ccy_date, to = end_ccy_date)
RUBUSD <- na.locf(`RUB=X`)
RUBUSD.50dayMA<-rollmean(RUBUSD,50,align=c("right"))
RUBUSD <- merge(RUBUSD,RUBUSD.50dayMA[,4])
RUBUSD.200dayMA<-rollmean(RUBUSD,200,align=c("right"))
RUBUSD <- merge(RUBUSD,RUBUSD.200dayMA[,4])
colnames(RUBUSD) <- c("Open", "High", "Low", "Close", "Vol", "Adj","50dma","200dma")

RUBUSD_plot2 <- ggplot() +
  geom_line(data = RUBUSD['2018/'],
            aes(x = Index, y = Close)) +
  geom_line(data = RUBUSD['2018/'],
            aes(x = Index, y = `50dma`, color="50dma")) +
  geom_line(data = RUBUSD['2018/'],
            aes(x = Index, y = `200dma`, color="200dma")) +
  xlab("") + ylab(" ") +
  labs(title="RUB / USD") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
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

RUBUSD_last <- paste(
  format(last(index(RUBUSD)),"%B %d"),": ",round(as.numeric(last(RUBUSD$Close)),3),sep="")
RUBUSD_last.1 <- as.numeric(last(RUBUSD$Close))
RUBUSD_1wk <- as.numeric(RUBUSD$Close[DateAdj_7days])
RUBUSD_1mo <- as.numeric(RUBUSD$Close[DateAdj_1mo])
RUBUSD_1wk_chg <- paste(round((RUBUSD_last.1/RUBUSD_1wk-1)*100,2), "%", sep="")
RUBUSD_1mo_chg <- paste(round((RUBUSD_last.1/RUBUSD_1mo-1)*100,2), "%", sep="")

### ARSUSD ###
getSymbols("ARS=X", from = start_ccy_date, to = end_ccy_date)
ARSUSD <- na.locf(`ARS=X`)
ARSUSD.50dayMA<-rollmean(ARSUSD,50,align=c("right"))
ARSUSD <- merge(ARSUSD,ARSUSD.50dayMA[,4])
ARSUSD.200dayMA<-rollmean(ARSUSD,200,align=c("right"))
ARSUSD <- merge(ARSUSD,ARSUSD.200dayMA[,4])
colnames(ARSUSD) <- c("Open", "High", "Low", "Close", "Vol", "Adj","50dma","200dma")

ARSUSD_plot2 <- ggplot() +
  geom_line(data = ARSUSD['2018/'],
            aes(x = Index, y = Close)) +
  geom_line(data = ARSUSD['2018/'],
            aes(x = Index, y = `50dma`, color="50dma")) +
  geom_line(data = ARSUSD['2018/'],
            aes(x = Index, y = `200dma`, color="200dma")) +
  xlab("") + ylab(" ") +
  labs(title="ARS / USD") +
  scale_y_continuous(position = c("left")) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "1 months",
               expand = expand_scale(mult = c(0, .01))) +
  scale_color_manual(values=c("blue","red","black")) +
  theme(
    legend.position = "bottom",
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

ARSUSD_last <- paste(
  format(last(index(ARSUSD)),"%B %d"),": ",round(as.numeric(last(ARSUSD$Close)),3),sep="")
ARSUSD_last.1 <- as.numeric(last(ARSUSD$Close))
ARSUSD_1wk <- as.numeric(ARSUSD$Close[DateAdj_7days])
ARSUSD_1mo <- as.numeric(ARSUSD$Close[DateAdj_1mo])
ARSUSD_1wk_chg <- paste(round((ARSUSD_last.1/ARSUSD_1wk-1)*100,2), "%", sep="")
ARSUSD_1mo_chg <- paste(round((ARSUSD_last.1/ARSUSD_1mo-1)*100,2), "%", sep="")

### Other and Correlations ###
getSymbols("NZD=X", from = start_ccy_date, to = end_ccy_date)
USDNZD <- na.locf(`NZD=X`)
colnames(USDNZD) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

getSymbols("CAD=X", from = start_ccy_date, to = end_ccy_date)
USDCAD <- na.locf(`CAD=X`)
colnames(USDCAD) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

getSymbols("NOK=X", from = start_ccy_date, to = end_ccy_date)
USDNOK <- na.locf(`NOK=X`)
colnames(USDNOK) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

getSymbols("SEK=X", from = start_ccy_date, to = end_ccy_date)
USDSEK <- na.locf(`SEK=X`)
colnames(USDSEK) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

getSymbols("GBP=X", from = start_ccy_date, to = end_ccy_date)
USDGBP <- na.locf(`GBP=X`)
colnames(USDGBP) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

getSymbols("JPY=X", from = start_ccy_date, to = end_ccy_date)
USDJPY <- na.locf(`JPY=X`)
colnames(USDJPY) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

getSymbols("CHF=X", from = start_ccy_date, to = end_ccy_date)
USDCHF <- na.locf(`CHF=X`)
colnames(USDCHF) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

getSymbols("AUD=X", from = start_ccy_date, to = end_ccy_date)
AUDUSD <- na.locf(`AUD=X`)
colnames(AUDUSD) <- c("Open", "High", "Low", "Close", "Vol", "Adj")

G10_CCY <- cbind(USDEUR[,4], USDGBP[,4], USDJPY[,4], AUDUSD[,4], USDNZD[,4],
                  USDCAD[,4], USDCHF[,4], USDNOK[,4], USDSEK[,4])
colnames(G10_CCY) <- c("USDEUR", "USDGBP", "USDJPY", "AUDUSD", "USDNZD",
                       "USDCAD", "USDCHF", "USDNOK", "USDSEK")

#runCor(x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE)
# tail(tq_transmute_xy(test, x=USDEUR, y=USDGBP, mutate_fun = runCor,
#                 n = 6*25,     #check days
#                 col_rename = "rolling.corr"))

CorrMatrix_G10_6mo <- cor(G10_CCY["2018-07-20/"])
CorrMatrix_G10_2yr <- cor(G10_CCY["2017-01-09/"])
CorrDiffTable <- CorrMatrix_G10_2yr-CorrMatrix_G10_6mo

CorrMatrix_G10_6mo_kable <- kable_styling(kable(CorrMatrix_G10_6mo, caption = "CorrMatrix_G10_6mo"),
                                          bootstrap_options = c("striped", "hover", "responsive"))

CorrMatrix_G10_2yr_kable <- kable_styling(kable(CorrMatrix_G10_2yr, caption = "CorrMatrix_G10_2yr"),
                                          bootstrap_options = c("striped", "hover", "responsive"))

CorrDiffTable_kable <- kable_styling(kable(CorrDiffTable, caption = "CorrDiff Matrix"),
                                     bootstrap_options = c("striped", "hover", "responsive"))

# CorrMatrix_G10_6mo_chart <- chart.Correlation(G10_CCY["2018-07-20/"], histogram=TRUE, pch=19)
# CorrMatrix_G10_2yr_chart <- chart.Correlation(G10_CCY["2017-01-09/"], histogram=TRUE, pch=19)
#
# Corr_EURGBP_6mo <- runCor(G10_CCY[,1],G10_CCY[,2],6*25)
# Corr_EURJPY_6mo <- runCor(G10_CCY[,1],G10_CCY[,3],6*25)
# Corr_EURAUD_6mo <- runCor(G10_CCY[,1],G10_CCY[,4],6*25)
# Corr_EURNZD_6mo <- runCor(G10_CCY[,1],G10_CCY[,5],6*25)
# Corr_EURCAD_6mo <- runCor(G10_CCY[,1],G10_CCY[,6],6*25)
# Corr_EURCHF_6mo <- runCor(G10_CCY[,1],G10_CCY[,7],6*25)
# Corr_EURNOK_6mo <- runCor(G10_CCY[,1],G10_CCY[,8],6*25)
# Corr_EURSEK_6mo <- runCor(G10_CCY[,1],G10_CCY[,9],6*25)
#
# Corr_GBPJPY_6mo <- runCor(G10_CCY[,2],G10_CCY[,3],6*25)
# Corr_GBPAUD_6mo <- runCor(G10_CCY[,2],G10_CCY[,4],6*25)
# Corr_GBPNZD_6mo <- runCor(G10_CCY[,2],G10_CCY[,5],6*25)
# Corr_GBPCAD_6mo <- runCor(G10_CCY[,2],G10_CCY[,6],6*25)
# Corr_GBPCHF_6mo <- runCor(G10_CCY[,2],G10_CCY[,7],6*25)
# Corr_GBPNOK_6mo <- runCor(G10_CCY[,2],G10_CCY[,8],6*25)
# Corr_GBPSEK_6mo <- runCor(G10_CCY[,2],G10_CCY[,9],6*25)
#
# Corr_JPYAUD_6mo <- runCor(G10_CCY[,3],G10_CCY[,4],6*25)
# Corr_JPYNZD_6mo <- runCor(G10_CCY[,3],G10_CCY[,5],6*25)
# Corr_JPYCAD_6mo <- runCor(G10_CCY[,3],G10_CCY[,6],6*25)
# Corr_JPYCHF_6mo <- runCor(G10_CCY[,3],G10_CCY[,7],6*25)
# Corr_JPYNOK_6mo <- runCor(G10_CCY[,3],G10_CCY[,8],6*25)
# Corr_JPYSEK_6mo <- runCor(G10_CCY[,3],G10_CCY[,9],6*25)
#
# Corr_AUDNZD_6mo <- runCor(G10_CCY[,4],G10_CCY[,5],6*25)
# Corr_AUDCAD_6mo <- runCor(G10_CCY[,4],G10_CCY[,6],6*25)
# Corr_AUDCHF_6mo <- runCor(G10_CCY[,4],G10_CCY[,7],6*25)
# Corr_AUDNOK_6mo <- runCor(G10_CCY[,4],G10_CCY[,8],6*25)
# Corr_AUDSEK_6mo <- runCor(G10_CCY[,4],G10_CCY[,9],6*25)
#
# Corr_NZDCAD_6mo <- runCor(G10_CCY[,5],G10_CCY[,6],6*25)
# Corr_NZDCHF_6mo <- runCor(G10_CCY[,5],G10_CCY[,7],6*25)
# Corr_NZDNOK_6mo <- runCor(G10_CCY[,5],G10_CCY[,8],6*25)
# Corr_NZDSEK_6mo <- runCor(G10_CCY[,5],G10_CCY[,9],6*25)
#
# Corr_CADCHF_6mo <- runCor(G10_CCY[,6],G10_CCY[,7],6*25)
# Corr_CADNOK_6mo <- runCor(G10_CCY[,6],G10_CCY[,8],6*25)
# Corr_CADSEK_6mo <- runCor(G10_CCY[,6],G10_CCY[,9],6*25)
#
# Corr_CHFNOK_6mo <- runCor(G10_CCY[,7],G10_CCY[,8],6*25)
# Corr_CHFSEK_6mo <- runCor(G10_CCY[,7],G10_CCY[,9],6*25)
#
# Corr_NOKSEK_6mo <- runCor(G10_CCY[,8],G10_CCY[,9],6*25)
#
# mean(na.exclude(runCor(G10_CCY[,1],G10_CCY[,2],6*25)))
