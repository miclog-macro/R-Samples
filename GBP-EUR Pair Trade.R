# # 1. R packages
# library('tseries')          #good
# library('quantmod')       #good
# library('PerformanceAnalytics')       #good
# library('roll')     #out
# library('urca')     #out
# library('ggplot2')    #good
# library('data.table')       #good
# library('kableExtra')       #good

# 2. Pairs Trading Analysis Data
# Data Reading
data2 <- xts(cbind(USDGBP[,4],USDEUR[,4]),order.by=as.Date(index(USDGBP)))
colnames(data2) <- c("USDGBP", "USDEUR")

# 2.2. Training and Testing Ranges Delimiting
tdata2 <- data2['2014-12-31::2017-12-31']    #training
fdata2 <- data2['2018-01-01/']    #test

# 3. Pairs Identification
tGBP <- tdata2[,1]
rtGBP <- dailyReturn(tGBP,type='arithmetic')
colnames(rtGBP) <- 'USDGBP Return'

tEUR <- tdata2[,2]
rtEUR <- dailyReturn(tEUR,type='arithmetic')
colnames(rtEUR) <- 'USDEUR Return'

round(as.numeric(cor(rtEUR,rtGBP)),3)

chart.CumReturns(cbind(rtGBP,rtEUR),
                 main='GBP-EUR Pair Cumulative Returns',
                 legend.loc='topleft')

GBPEUR.returns <- chart.CumReturns(cbind(rtGBP,rtEUR),
                                   main='Returns',
                                   legend.loc='topleft')

# 4. Pairs Spread Co-Integration
# Spread Calculation
exp_spread <- tGBP-lm(tGBP~0+tEUR)$coefficients[1]*tEUR
exp_spread_mean <- xts(rep(mean(exp_spread),length(exp_spread)),
                       order.by=as.Date(index(exp_spread)))
GBPEUR_spread_plot <- plot(cbind(exp_spread,exp_spread_mean),main='Spread')

as.numeric(round(lm(tGBP~0+tEUR)$coefficients[1],2))

# (Non) Stationary Prices
adf.test(tGBP)
adf.test(tEUR)
adf.test(na.exclude(diff(tGBP,lag=1)))
adf.test(na.exclude(diff(tEUR,lag=1)))

# Stationary Spread
adf.test(exp_spread)

# Pairs Trading Strategies
#Rolling Spread Z-Score
fGBP <- fdata2[,1]
fEUR <- fdata2[,2]
exp_spread2 <- fGBP-lm(tGBP~0+tEUR)$coefficients[1]*fEUR
spread_z <- roll_scale(exp_spread2,width=21)
z1 <- xts(rep(1,length(fdata2[,1])),order.by=as.Date(index(fdata2)))
z2 <- xts(rep(2,length(fdata2[,1])),order.by=as.Date(index(fdata2)))
z3 <- xts(rep(-1,length(fdata2[,1])),order.by=as.Date(index(fdata2)))
z4 <- xts(rep(-2,length(fdata2[,1])),order.by=as.Date(index(fdata2)))
plot(cbind(spread_z,z1,z2,z3,z4),main='GBP-EUR Rolling Spread Z-Score')

GBPEUR.zscore_plot <- plot(cbind(spread_z,z1,z2,z3,z4),main='Rolling Z-Score')

# CAN-RSA Trading Strategy Signals
signals <- lag(ifelse(lag(spread_z)>(-2)&spread_z<(-2),-2,   #enter long
                       ifelse(lag(spread_z)<(-1)&spread_z>(-1),-1,   #exit long
                              ifelse(lag(spread_z)<2&spread_z>2,2,    #enter short
                                     ifelse(lag(spread_z)>1&spread_z<1,1,0)))))  #exit short
signals[is.na(signals)] <- 0   #substituting na with 0


positions <- ifelse(signals>2,1,0)   #creating a variable that is all zero with same characteristics and length
for(i in 1:length(spread_z)){positions[i] <- ifelse(signals[i]==-2,1,  #enter long
                                                 ifelse(signals[i]==-1,0,  #exit long
                                                        ifelse(signals[i]==2,-1,  #enter short
                                                               ifelse(signals[i]==1,0,positions[i-1]))))}  #exit short
positions[is.na(positions)] <- 0
tradedata <- cbind(spread_z,signals,positions)
colnames(tradedata) <- c('spread_z','signals','positions')

# 6. Pairs Strategies Performance Comparison
# 6.1. Single Pair Strategies Performance Comparison
rfGBP <- dailyReturn(fGBP,type='arithmetic')   #testing range - GBP return
rfEUR <- dailyReturn(fEUR,type='arithmetic')   #testing range - EUR return
exp_return_spread <- rfGBP-lm(tGBP~0+tEUR)$coefficients[1]*rfEUR
pos_size <- positions*exp_return_spread   #return x position signal
final_data <- cbind(pos_size,rfGBP,rfEUR)
colnames(final_data) <- c('Pair Trade','USDGBP','USDEUR')
charts.PerformanceSummary(final_data)
table.AnnualizedReturns(final_data)

Return_Metrics <- c("Annualized Return","Annualized Std Dev", "Annualized Sharpe (Rf=0%)")
Annualized_Returns_DT<- data.table(Return_Metrics,
           table.AnnualizedReturns(final_data)[1],
           table.AnnualizedReturns(final_data)[2],
           table.AnnualizedReturns(final_data)[3])

Annualized_Returns_kable <- kable_styling(kable(Annualized_Returns_DT),
                                          bootstrap_options = c("striped", "hover", "responsive"))



GBP_EUR_historical_plot <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = USDGBP['2014-12-31/'],
            aes(x = Index, y = Close, color = "USDGBP")) +
  geom_line(data = USDEUR['2014-12-31/'],
            aes(x = Index, y = Close, color = "USDEUR")) +
  xlab("") + ylab("") +
  labs(title="GBP vs EUR Perfomance") +
  scale_y_continuous(position = c("left"),
                     limits = c(0.5,1),
                     breaks=seq(.5,1,by=.1)) +
  scale_x_date(date_labels = "%b %y",
               date_breaks = "3 months",
               expand = expand_scale(mult = c(0, .01))) +
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

