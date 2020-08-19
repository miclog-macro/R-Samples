## Macro Arbitrage - Country ETFs to Asia ExJapan

MacroArb.Notes.1 <- "
The charts above show potential macro spread trades based on country ETFs with similar
betas to the Asia-ExJapan index (AAXJ). Betas were calculated over 5 years, 2 year and 6 month
time horizons. Given similar sensitivities, the spreads should track
the Asia-ExJapan index. Where there is a divergence, a spread trade by going long one
country ETF and short the other will deliver a positive return as the spread falls back
in line with Asia-ExJapan."
MacroArb.Notes.2 <- "
Spain (EWP)/Sweden (EWD) vs Asia-ExJapan: In June 2018 a profit opportunity resulted in
going short EWP and long EWD, as the spread diverged from Asia-ExJapan. In 4Q18, another
trade opportunity evolved from again going short EWP and long EWD. Currently, the spread has
diverged and an investor could profit by going short EWP and long EWD."
MacroArb.Notes.3 <- "
Netherlands (EWN)/Finland (EFNL) vs Asia-ExJapan: Over a 6-month horizon, a divergence
between a Netherlands/Finland spread and Asia-ExJapan appeared in May 2019. A spread trade
opportunity exists by going short EWN and long EFNL."

# Asia ex Japan - iShares MSCI All Country Asia ex Japan ETF (USD)
AsiaExJapan <- na.locf(getSymbols("AAXJ", src = "yahoo", auto.assign = F))
colnames(AsiaExJapan) <- c("Open","High","Low","Close","Volume","Adj")
AsiaExJapan <- AsiaExJapan[,"Close"]

#2 Year - Spain/Sweden
# Spain - iShares MSCI Spain Capped ETF (USD)
Spain.EWP <- na.locf(getSymbols("EWP", src = "yahoo", auto.assign = F))
colnames(Spain.EWP) <- c("Open","High","Low","Close","Volume","Adj")
Spain.EWP <- Spain.EWP[,"Close"]
# Sweden - iShares MSCI Sweden ETF (USD)
Sweden.EWD <- na.locf(getSymbols("EWD", src = "yahoo", auto.assign = F))
colnames(Sweden.EWD) <- c("Open","High","Low","Close","Volume","Adj")
Sweden.EWD <- Sweden.EWD[,"Close"]

# Merge + Plot
Spain.Sweden.2yr <- Spain.EWP["2017-06-09/"]/Sweden.EWD["2017-06-09/"]
Spain.Sweden.Merge <- merge(Spain.Sweden.2yr,AsiaExJapan$Close["2017-06-09/"])
Spain.Sweden.Merge <- data.frame(Spain.Sweden.Merge)
colnames(Spain.Sweden.Merge) <- c("Spain.Sweden","AsiaExJapan")


plot(as.Date(row.names(Spain.Sweden.Merge)), Spain.Sweden.Merge$Spain.Sweden, col="blue", type='l', ylab="", xlab="")
par(mar=c(4, 2, 2, 2))
title("Spain (EWP)/Sweden (EWD) vs Asia-ExJapan", line = 0.3)
par(new=TRUE)
plot(as.Date(row.names(Spain.Sweden.Merge)), Spain.Sweden.Merge$AsiaExJapan,col="red",
     type='l', ylab="", xlab="",xaxt="n", yaxt='n')
axis(4)
grid(10,10)
legend("bottomleft",col=c("blue","red"),lty=1,legend=c("Spain/Sweden (left)","Asia-ExJapan (right)"),
       cex=0.8)
Spain.Sweden.plot <- recordPlot()

# Netherlands/Finland (6mo)
# Netherlands - iShares MSCI Netherlands ETF (USD)
Netherlands.EWN <- na.locf(getSymbols("EWN", src = "yahoo", auto.assign = F))
colnames(Netherlands.EWN) <- c("Open","High","Low","Close","Volume","Adj")
Netherlands.EWN <- Netherlands.EWN[,"Close"]
# Finland - iShares MSCI Finland ETF (USD)
Finland.EFNL <- na.locf(getSymbols("EFNL", src = "yahoo", auto.assign = F))
colnames(Finland.EFNL) <- c("Open","High","Low","Close","Volume","Adj")
Finland.EFNL <- Finland.EFNL[,"Close"]

# Merge + Plot
Netherlands.Finland.6mo <- Netherlands.EWN["2018-12-09/"]/Finland.EFNL["2018-12-09/"]
Netherlands.Finland.Merge <- merge(Netherlands.Finland.6mo,AsiaExJapan$Close["2018-12-09/"])
Netherlands.Finland.Merge <- data.frame(Netherlands.Finland.Merge)
colnames(Netherlands.Finland.Merge) <- c("Netherlands.Finland","AsiaExJapan")

par(mar=c(4, 2, 2, 2))
plot(x = as.Date(row.names(Netherlands.Finland.Merge)), y = Netherlands.Finland.Merge$Netherlands.Finland, col="blue", type='l', ylab="", xlab="")
title("Netherlands (EWN)/Finland (EFNL) vs Asia-ExJapan", line = 0.3)
par(new=TRUE)
plot(x = as.Date(row.names(Netherlands.Finland.Merge)), y = Netherlands.Finland.Merge$AsiaExJapan,col="red",
     type='l', ylab="", xlab="",xaxt="n", yaxt='n')
axis(4)
grid(10,10)
legend("bottomleft",col=c("blue","red"),lty=1,legend=c("Netherlands/Finland (left)","AsiaExJapan (right)"),
       cex=0.8)
Netherlands.Finland.plot <- recordPlot()

# #2 Year - Netherlands/France
# # Netherlands - iShares MSCI Netherlands ETF (USD)
# Netherlands.EWN <- na.locf(getSymbols("EWN", src = "yahoo", auto.assign = F))
# colnames(Netherlands.EWN) <- c("Open","High","Low","Close","Volume","Adj")
# Netherlands.EWN <- Netherlands.EWN[,"Close"]
# # France - iShares MSCI France ETF (USD)
# France.EWQ <- na.locf(getSymbols("EWQ", src = "yahoo", auto.assign = F))
# colnames(France.EWQ) <- c("Open","High","Low","Close","Volume","Adj")
# France.EWQ <- France.EWQ[,"Close"]
#
# # Merge + Plot
# Netherlands.France.5yr <- Netherlands.EWN["2014-06-09/"]/France.EWQ["2014-06-09/"]
# Netherlands.France.Merge <- merge(Netherlands.France.5yr,AsiaExJapan$Close["2014-06-09/"])
# Netherlands.France.Merge <- data.frame(Netherlands.France.Merge)
# colnames(Netherlands.France.Merge) <- c("Netherlands.France","AsiaExJapan")
#
# par(mar=c(4, 2, 2, 2))
# plot(as.Date(row.names(Netherlands.France.Merge)), Netherlands.France.Merge$Netherlands.France, col="blue", type='l', ylab="", xlab="")
# title("Netherlands (EWN)/France (EWQ) vs Asia-ExJapan", line = 0.3)
# par(new=TRUE)
# plot(Netherlands.France.Merge$AsiaExJapan,col="red",
#      type='l', ylab="", xlab="",xaxt="n", yaxt='n')
# axis(4)
# grid(10,10)
# legend("bottomleft",col=c("blue","red"),lty=1,legend=c("Netherlands/France (left)","Asia-ExJapan (right)"),
#        cex=0.8)
# Netherlands.France.plot <- recordPlot()


# ggplot() +
#   geom_line(data = Spain.Sweden.Merge,
#             aes(x = as.Date(rownames(Spain.Sweden.Merge)), y = Spain.Sweden, color = "Spain/Sweden (left)")) +
#   geom_line(data = Spain.Sweden.Merge,
#             aes(x = as.Date(rownames(Spain.Sweden.Merge)), y = AsiaExJapan/75, color = "Asia-ExJapan (right)")) +
#   xlab("") + ylab("") +
#   labs(title="Spain (EWP)/Sweden (EWD) vs Asia-ExJapan") +
#   scale_y_continuous(position = c("left"),
#                      limits = c(.88,1.02),
#                      breaks=seq(.88,1.02,by=.2),
#                      sec.axis = sec_axis(~.*75)) +
#   scale_x_date(date_labels = "%b %y",
#                date_breaks = "6 months",
#                expand = expand_scale(mult = c(0, .01))) +
#   scale_color_manual(values=c("blue", "red")) +
#   theme(
#     legend.position = "bottom",
#     legend.title=element_blank(),
#     legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
#     legend.text = element_text(family = "Palatino",size=10,
#                                margin = margin(l=2, r=10, unit = "pt")),
#     plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
#     plot.title=element_text(
#       family = "Palatino",
#       color = "black",
#       size=14,
#       hjust = 0),
#     axis.text.y=element_text(
#       color = "black",
#       family = "Palatino"),
#     axis.text.x=element_text(
#       color = "black",
#       family = "Palatino",
#       size=8,
#       angle=40,
#       vjust=.8,
#       hjust=0.8))
