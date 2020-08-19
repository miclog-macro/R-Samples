start_ccy_date <- Sys.Date()-365*5
end_ccy_date <- Sys.Date()

#Applying prior 1 year inflation as proxy for expected inflation

#Purchasing Power Parity (inflation differentials)
#USD/GBP (USD per GBP)
GBPUSD <- 1/na.locf(getSymbols("GBP=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(GBPUSD) <- c("GBPUSD")
GBPUSD_spot <- round(last(GBPUSD),3)

#BMI Fitch Research
ExpInfl_2019_USD <- 0.023
ExpInfl_2019_GBP <- 0.022

GBPUSD_Parity_1yr <- round(
  GBPUSD_spot*((1+ExpInfl_2019_USD)/(1+ExpInfl_2019_GBP)),3)
#GBP expected to appreciate because it has a lower expected inflation

#CHF/USD (CHF per USD)
USDCHF <- na.locf(getSymbols("CHF=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(USDCHF) <- c("Close")
USDCHF_spot <- round(last(USDCHF),3)

#BMI Fitch Research
ExpInfl_2019_CHF <- 0.01

USDCHF_Parity_1yr <- round(
  USDCHF_spot*((1+ExpInfl_2019_CHF)/(1+ExpInfl_2019_USD)),3)
#CHF expected to appreciate because it has lower inflation rate

#JPY/USD (JPY per USD)
USDJPY <- na.locf(getSymbols("JPY=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(USDJPY) <- c("USDJPY")
USDJPY_spot <- round(last(USDJPY),3)

#BMI Fitch Research
ExpInfl_2019_JPY <- 0.016

USDJPY_Parity_1yr <- round(
  USDJPY_spot*((1+ExpInfl_2019_JPY)/(1+ExpInfl_2019_USD)),3)
#JPY expected to appreciate because it has lower inflation rate

#AUD/USD (AUD per USD)
AUDUSD <- na.locf(getSymbols("AUD=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(AUDUSD) <- c("AUDUSD")
AUDUSD_spot <- round(last(AUDUSD),3)

#BMI Fitch Research
ExpInfl_2019_AUD <- 0.021

AUDUSD_Parity_1yr <- round(
  AUDUSD_spot*((1+ExpInfl_2019_AUD)/(1+ExpInfl_2019_USD)),3)
#AUD expected to appreciate because it has lower expected inflation rate

PPP_RowNames <- c("Spot","Exp US Inflation (1yr)","Exp FC Inflation (1yr)","Pair Parity")
PPP_GBPUSD <- c(GBPUSD_spot, ExpInfl_2019_USD*100, ExpInfl_2019_GBP*100, GBPUSD_Parity_1yr)
PPP_USDCHF <- c(USDCHF_spot, ExpInfl_2019_USD*100, ExpInfl_2019_CHF*100, USDCHF_Parity_1yr)
PPP_USDJPY <- c(USDJPY_spot, ExpInfl_2019_USD*100, ExpInfl_2019_JPY*100, USDJPY_Parity_1yr)
PPP_AUDUSD <- c(AUDUSD_spot, ExpInfl_2019_USD*100, ExpInfl_2019_AUD*100, AUDUSD_Parity_1yr)

PPP_dataframe <- data.frame(PPP_RowNames,PPP_GBPUSD,PPP_USDCHF,PPP_USDJPY,PPP_AUDUSD)
colnames(PPP_dataframe) <- c("Pair","USD/GBP","CHF/USD","JPY/USD","AUD/USD")

PPP_kable <- kable_styling(kable(PPP_dataframe, caption = "Inflation expectations updated: 3/14/19"),
                           bootstrap_options = c("striped", "hover", "responsive"))




#International Fisher Effect (expected spot instead of fwd rate (IRP))
#USD/GBP (USD per GBP)
GBPUSD <- 1/na.locf(getSymbols("GBP=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(GBPUSD) <- c("GBPUSD")
GBPUSD_spot <- round(last(GBPUSD),3)

US1Yr <- getSymbols("DGS1", src = "FRED", auto.assign = F)
colnames(US1Yr) <- c("US1Yr")
US1Yr_last <- last(US1Yr)

GBP1Yr_last <- 0.718   #(3/7/19)
GBPUSD_FP <- US1Yr_last-GBP1Yr_last   #Forward Premium
GBPUSD_IFE_1yr <- round(GBPUSD_spot*GBPUSD_FP,3)

#JPY/USD (JPY per USD)
USDJPY <- na.locf(getSymbols("JPY=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(USDJPY) <- c("USDJPY")
USDJPY_spot <- round(last(USDJPY),3)

JPY1Yr_last <- -0.0014   #(3/7/19)
USDJPY_FP <- JPY1Yr_last-US1Yr_last/100
USDJPY_IFE_1yr <- round(USDJPY_spot*(1+USDJPY_FP),3)

#AUD/USD (AUD per USD)
AUDUSD <- na.locf(getSymbols("AUD=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(AUDUSD) <- c("AUDUSD")
AUDUSD_spot <- round(last(AUDUSD),3)
AUD1Yr_last <- 1.708   #(3/7/19)
AUDUSD_FP <- AUD1Yr_last/100-US1Yr_last/100
AUDUSD_IFE_1yr <- round(AUDUSD_spot*(1+AUDUSD_FP),3)

#CHF/USD (CHF per USD)
USDCHF <- na.locf(getSymbols("CHF=X", from = start_ccy_date, to = end_ccy_date, auto.assign = F)[,4])
colnames(USDCHF) <- c("Close")
USDCHF_spot <- round(last(USDCHF),3)
CHF1Yr_last <- -0.00540
USDCHF_FP <- CHF1Yr_last-US1Yr_last/100
USDCHF_IFE_1yr <- round(USDCHF_spot*(1+AUDUSD_FP),3)

IFE_RowNames <- c("Spot","US 1yr Rate(%)","FC 1yr Rate(%)","Fwd Premium(%)","Pair Parity")
IFE_GBPUSD <- c(GBPUSD_spot, US1Yr_last, GBP1Yr_last, GBPUSD_FP,GBPUSD_IFE_1yr)
IFE_USDCHF <- c(USDCHF_spot, US1Yr_last, CHF1Yr_last*100, USDCHF_FP,USDCHF_IFE_1yr)
IFE_USDJPY <- c(USDJPY_spot, US1Yr_last, JPY1Yr_last*100, USDJPY_FP,USDJPY_IFE_1yr)
IFE_AUDUSD <- c(AUDUSD_spot, US1Yr_last, AUD1Yr_last, AUDUSD_FP*100,AUDUSD_IFE_1yr)

IFE_dataframe <- data.frame(IFE_RowNames,IFE_GBPUSD,IFE_USDCHF,IFE_USDJPY,IFE_AUDUSD)
colnames(IFE_dataframe) <- c("Pair","USD/GBP","CHF/USD","JPY/USD","AUD/USD")
#Country with lower interest rate - ccy expected to appreciate

IFE_kable <- kable_styling(kable(IFE_dataframe, caption = "Updated: 3/14/19"),
                                bootstrap_options = c("striped", "hover", "responsive"))


