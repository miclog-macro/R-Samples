server <- function(input, output) {
  output$name <- renderText({input$name})
  ### Key Indicators
  output$US_YieldCurve_plot <- renderPlot({plot(US_YieldCurve_plot)})
  output$VIX_plot <- renderPlot({plot(VIX_plot)})
  output$PMI.US_plot <- renderPlot({plot(PMI.US_plot)})
  output$VIX_Compare_plot <- renderPlot({plot(VIX_Compare_plot)})
  output$BE_Inflation_plot <- renderPlot({plot(BE_Inflation_plot)})
  output$CrudeOil_plot <- renderPlot({plot(CrudeOil_plot)})

  ### Trade Themes
  output$GBPEUR_spread_plot <- renderPlot({plot(GBPEUR_spread_plot)})
  output$GBPEUR.returns <- renderPlot({plot(GBPEUR.returns)})
  output$GBPEUR.zscore_plot <- renderPlot({plot(GBPEUR.zscore_plot)})
  output$Annualized_Returns_kable <- renderText({
    kable(Annualized_Returns_DT) %>%
      kable_styling(
        font_size = 15,
        bootstrap_options = c("striped", "hover", "condensed"))})

  output$BLL_plot <- renderPlot({plot(BLL_plot)})
  output$MKC_plot <- renderPlot({plot(MKC_plot)})
  output$CHD_plot <- renderPlot({plot(CHD_plot)})

  output$Spain.Sweden.plot <- renderPlot({replayPlot(Spain.Sweden.plot)})
  output$Netherlands.Finland.plot <- renderPlot({replayPlot(Netherlands.Finland.plot)})

  output$Zimbabwe.Debt2GDP_plot <- renderPlot({plot(Zimbabwe.Debt2GDP_plot)})
  output$Zimbabwe.STdebt_plot <- renderPlot({plot(Zimbabwe.STdebt_plot)})
  output$Zimbabwe.NetRes_plot <- renderPlot({plot(Zimbabwe.NetRes_plot)})

  output$Flood_index_plot <- renderPlot({plot(Flood_index_plot)})

  output$USDHKD_plot <- renderPlot({plot(USDHKD_plot)})
  output$HKPP_plot <- renderPlot({plot(HKPP_plot)})
  output$HK_PrvtCredit_plot <- renderPlot({plot(HK_PrvtCredit_plot)})

  output$US10YR_1YR_Spread_plot <- renderPlot({plot(US10YR_1YR_Spread_plot)})
  output$US10_1Sread.Vs.BankTightening_plot <- renderPlot({plot(US10_1Sread.Vs.BankTightening_plot)})
  output$VBLTX_plot <- renderPlot({plot(VBLTX_plot)})

  # ### U.S. Indicators
  # output$US.TreasuryRate.Move_plot <- renderPlot({plot(US.TreasuryRate.Move_plot)})
  # output$GDP_US_plot <- renderPlot({plot(GDP_US_plot)})
  # output$US10Yr.CPI.Corr.3mo_plot <- renderPlot({plot(US10Yr.CPI.Corr.3mo_plot)})
  # output$US10Yr.WkClaims.Corr.3mo_plot <- renderPlot({plot(US10Yr.WkClaims.Corr.3mo_plot)})
  # output$ConsSentiment_plot <- renderPlot({plot(ConsSentiment_plot)})
  # output$RealOutputPerHrPP_US_plot <- renderPlot({plot(RealOutputPerHrPP_US_plot)})
  # output$LaborProductivity_US_plot <- renderPlot({plot(LaborProductivity_US_plot)})
  # output$RealOutputManufac_US_plot <- renderPlot({plot(RealOutputManufac_US_plot)})
  # output$RetailSales_exAuto_plot <- renderPlot({plot(RetailSales_exAuto_plot)})
  # output$CurrenAccount_US_plot <- renderPlot({plot(CurrenAccount_US_plot)})
  # output$PPI_OilTrans_plot <- renderPlot({plot(PPI_OilTrans_plot)})
  # output$CorpProfits_plot <- renderPlot({plot(CorpProfits_plot)})
  # output$Swing_2pct_chart <- renderPlot({plot(Swing_2pct_chart)})
  # output$SP500.PE.Ratio_plot <- renderPlot({plot(SP500.PE.Ratio_plot)})
  # output$DXY_plot <- renderPlot({plot(DXY_plot)})
  # output$SP500.Breadth.200dma_plot <- renderPlot({plot(SP500.Breadth.200dma_plot)})
  # output$FinancialConditions_plot <- renderPlot({plot(FinancialConditions_plot)})
  #
  # ### Federal Reserve Indicators
  # output$FF10_plot <- renderPlot({plot(FF10_plot)})
  # output$FOMC_Projections_plot <- renderPlot({plot(FOMC_Projections_plot)})
  # output$DeliquencyRates_CC_plot <- renderPlot({plot(DeliquencyRates_CC_plot)})
  # output$DeliquencyRates_RE_plot <- renderPlot({plot(DeliquencyRates_RE_plot)})
  # output$FedWatch_kable <- renderText({
  #   kable(FedWatch_dt, caption = "Updated: 6/17/19") %>%
  #     kable_styling(
  #       font_size = 15,
  #       bootstrap_options = c("striped", "hover", "condensed"))})
  #
  # ### Labor Market Indicators
  # output$Job_Growth_plot <- renderPlot({plot(Job_Growth_plot)})
  # output$Avg_Hourly_Earnings_plot <- renderPlot({plot(Avg_Hourly_Earnings_plot)})
  # output$Unemployment_US_plot <- renderPlot({plot(Unemployment_US_plot)})
  # output$ParticipationRate_US_plot <- renderPlot({plot(ParticipationRate_US_plot)})
  #
  # ### China Indicators
  # output$USDCNY_plot <- renderPlot({plot(USDCNY_plot)})
  # output$China_GDP_plot <- renderPlot({plot(China_GDP_plot)})
  # output$China_CPI_plot <- renderPlot({plot(China_CPI_plot)})
  # output$China_Exports_plot <- renderPlot({plot(China_Exports_plot)})
  # output$China_PropertyPrices_plot <- renderPlot({plot(China_PropertyPrices_plot)})
  # output$China_ExIm_Ratio_plot <- renderPlot({plot(China_ExIm_Ratio_plot)})
  #
  # ### Japan Indicators
  # output$JGB10yr2yr_plot <- renderPlot({plot(JGB10yr2yr_plot)})
  # output$Japan_Infl_plot <- renderPlot({plot(Japan_Infl_plot)})
  # output$Japan_Yield_Vol_plot <- renderPlot({plot(Japan_Yield_Vol_plot)})
  # output$Japan_Bond_TO_plot <- renderPlot({plot(Japan_Bond_TO_plot)})
  #
  # ### Argentina Indicators
  # output$USDARS_plot <- renderPlot({plot(USDARS_plot)})
  # output$ARGT_ETF_plot <- renderPlot({plot(ARGT_ETF_plot)})
  # output$Argentina_CPI_plot <- renderPlot({plot(Argentina_CPI_plot)})
  # output$Argentina_Exp_plot <- renderPlot({plot(Argentina_Exp_plot)})
  # output$Argentina_CA_plot <- renderPlot({plot(Argentina_CA_plot)})
  # output$Argentina_Res_plot <- renderPlot({plot(Argentina_Res_plot)})
  # output$Argentina_YouthUn_plot <- renderPlot({plot(Argentina_YouthUn_plot)})
  #
  # ### Russia Indicators
  # output$RUBUSD_plot <- renderPlot({plot(RUBUSD_plot)})
  # output$Russia_Index_plot <- renderPlot({plot(Russia_Index_plot)})
  # output$Russia_GDP_plot <- renderPlot({plot(Russia_GDP_plot)})
  # output$Russia_Unemp_plot <- renderPlot({plot(Russia_Unemp_plot)})
  # output$Russia_CPI_plot <- renderPlot({plot(Russia_CPI_plot)})
  #
  # ### Currencies Indicators
  # output$IFE_kable <- renderText({
  #   kable(IFE_dataframe, caption = "FC rates updated on: 3/14/19") %>%
  #     kable_styling(
  #       font_size = 12,
  #       bootstrap_options = c("striped", "hover", "condensed"))})
  # output$PPP_kable <- renderText({
  #   kable(PPP_dataframe, caption = "Inflation expectations updated: 3/14/19") %>%
  #     kable_styling(
  #       font_size = 12,
  #       bootstrap_options = c("striped", "hover", "condensed"))})
  #
  # output$USDEUR_plot <- renderPlot({plot(USDEUR_plot)})
  # output$USDGBP_plot <- renderPlot({plot(USDGBP_plot)})
  # output$USDCHF_plot <- renderPlot({plot(USDCHF_plot)})
  # output$USDJPY_plot <- renderPlot({plot(USDJPY_plot)})
  # output$AUDUSD_plot <- renderPlot({plot(AUDUSD_plot)})
  # output$RUBUSD_plot2 <- renderPlot({plot(RUBUSD_plot2)})
  # output$ARSUSD_plot2 <- renderPlot({plot(ARSUSD_plot2)})
  #
  # output$CorrDiffTable_kable <- renderText({
  #   kable(CorrDiffTable, caption = "G10 Currences 6mo vs 2yr Correlation Differences") %>%
  #     kable_styling(
  #       font_size = 15,
  #       bootstrap_options = c("striped", "hover", "condensed"))})
  # output$CorrMatrix_G10_6mo_kable <- renderText({
  #   kable(CorrMatrix_G10_6mo, caption = "G10 Currences 6 month Correlation") %>%
  #     kable_styling(
  #       font_size = 15,
  #       bootstrap_options = c("striped", "hover", "condensed"))})
  # output$CorrMatrix_G10_2yr_kable <- renderText({
  #   kable(CorrMatrix_G10_2yr, caption = "G10 Currences 2 year Correlation") %>%
  #     kable_styling(
  #       font_size = 15,
  #       bootstrap_options = c("striped", "hover", "condensed"))})

  # ### Economic Calendar
  # output$event_dates <- renderText({
  #   kable(event_dates) %>%
  #     kable_styling(
  #       font_size = 15,
  #       bootstrap_options = c("striped", "hover", "condensed"))})
  # url2 <- a("FX Street Calendar", href="https://www.fxstreet.com/economic-calendar")
  # output$tab2 <- renderUI({
  #   tagList("URL link:", url2)})

  ### Contact
  url <- a("LinkedIn", href="https://www.linkedin.com/in/michaellogalbo/")
  output$tab <- renderUI({
    tagList("URL link:", url)})
}


# Plotly Code
# output$Unemployment_US_plotly <- renderPlotly({
#   print(
#     layout(
#     ggplotly(
#       ggplot() +
#         geom_rect(data = recession08,
#                   aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
#                   fill="grey",alpha=0.5) +
#         geom_hline(yintercept = 0) +
#         geom_line(data = Unemployment_US['2005/'],
#                   aes(x = Index, y = Rate)) +
#         xlab("") + ylab("") +
#         labs(title="") +                         #Removed title for plotly
#         scale_y_continuous(position = c("left"),
#                            labels = function(x) paste0(x, "%"),
#                            limits = c(floor(min(Unemployment_US['2005/']))-1,
#                                       ceiling(max(Unemployment_US['2005/']))+1),
#                            breaks=seq(floor(min(Unemployment_US['2005/']))-1,
#                                       ceiling(max(Unemployment_US['2005/']))+1,
#                                       by=2)) +
#         scale_x_date(date_labels = "%b %y",
#                      date_breaks = "6 months",
#                      expand = expand_scale(mult = c(0, .01))) +
#         theme(
#           legend.position = "none",
#           legend.title=element_blank(),
#           legend.margin=margin(t=-.5, r=0, b=0, l=0, unit="cm"),
#           legend.text = element_text(family = "Palatino",size=10,
#                                      margin = margin(l=2, r=10, unit = "pt")),
#           plot.margin = margin(t = 8, r = 5, b = 5, l = 0, "pt"),
#           plot.title=element_text(
#             family = "Palatino",
#             color = "black",
#             size=14,
#             hjust = 0),
#           axis.text.y=element_text(
#             color = "black",
#             family = "Palatino"),
#           axis.text.x=element_text(
#             color = "black",
#             family = "Palatino",
#             size=8,
#             angle=40,
#             vjust=.8,
#             hjust=0.8))),
#     margin = list(l = 0, r = 0, t = 40, b = 0),
#     annotations = list(
#       yref="paper",
#       xref="paper",
#       y=1.3,
#       x= 0,
#       text= "U.S. Participation Rate",
#       font=list(size=18, family = "Palatino"),
#       showarrow=F)))})
