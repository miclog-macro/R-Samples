
header <- dashboardHeader(
  title = "Michael LoGalbo Macroeconomic Dashboard",
  titleWidth = 450
  # tags$head(tags$style(HTML('.main-title {font-weight: bold;
  #                               font-family: Palatino}')))
  # dropdownMenu(
  # type = "messages",
  #   messageItem(
  #     from = "Lucy",
  #     message = "You can view the International Space Station!",
  #     href = "https://spotthestation.nasa.gov/sightings/"),
  #   messageItem(
  #     from = "Lucy",
  #     message = "Learn more about the International Space Station",
  #     href = "https://spotthestation.nasa.gov/faq.cfm")),
  # dropdownMenu(
  #   type = "notifications",
  #   notificationItem("The International Space Station is overhead!"),
  #   icon = icon("coffee")),
  # dropdownMenu(
  #   type = "notifications",
  #   notificationItem(
  #     text = "The International Space Station is overhead!",
  #     icon = icon("rocket"))),
  # dropdownMenu(
  #   type = "tasks",
  #   taskItem(
  #     text = "Mission Learn Shiny Dashboard",
  #     value = 10))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Trade Themes",
             tabName = "TradeThemes"),
    menuItem(text = "Key Indicators",
             tabName = "Key"),        #tabName has to be only 1 word
    # menuItem(text = "U.S.",
    #          tabName = "US"),
    # menuItem(text = "Federal Reserve",
    #          tabName = "Fed"),
    # menuItem(text = "Labor Market",
    #          tabName = "Labor"),
    # menuItem(text = "China",
    #          tabName = "China"),
    # menuItem(text = "Japan",
    #          tabName = "Japan"),
    # menuItem(text = "Argentina",
    #          tabName = "Argentina"),
    # menuItem(text = "Russia",
    #          tabName = "Russia"),
    # menuItem(text = "Currencies",
    #          tabName = "Currencies"),
    # menuItem(text = "Economic Release Calendar",
    #          tabName = "EconomicCalendar"),
    menuItem(text = "Contact",
             tabName = "Contact")
    )
  # actionButton("restart", "Restart"),
  # sliderInput(
  #   inputId = "height",
  #   label = "Height",
  #   min = 66,
  #   max = 264,
  #   value = 264),
  # selectInput(inputId = "name",
  #             label = "Name",
  #             choices = c("A","B","C"))
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "Key",
      #tags$head(tags$style(HTML('h3 {font-weight: bold;}'))),
      tags$head(tags$style(HTML('h3 {font-weight: bold;
                                font-family: Palatino}',
                                'p {font-family: Palatino}',
                                'strong {font-family: Palatino}'))),
      fluidRow(
        shinydashboard::box(
          width = 12,
          title = "Key Market Indicators"
          )
        ),
      fluidRow(
        column(width = 6,
               plotOutput("US_YieldCurve_plot",
                          width = "100%",
                          height = "200px"),
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p("Notes: Yield curve flattening. Expect market overreaction
                   when 10Yr and 2Yr invert.")
                 )
               ),
        column(width = 6,
               #VIX CHART
               plotOutput("VIX_plot",
                          width = "100%",
                          height = "200px"),
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p("Date: ",VIX_date_last),
                 p("Last: ",VIX_last),
                 p("1 week change: ",VIX_1wk_chg),
                 p("1 mo change: ",VIX_1mo_chg)
               )
               )
      ),
      fluidRow(
        # Row 3, Column 1
        column(width = 6,
               plotOutput("PMI.US_plot",
                          width = "100%",
                          height = "200px"),
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p("Date: ",PMI.US.date.last),
                 p("Last: ",PMI.US.last),
                 p("Trailing 3mo Avg: ",PMI.US.3moAvg),
                 p("Trailing 6mo Avg: ",PMI.US.6moAvg),
                 p("Notes: Declining from a high in August 2018")
               )
        ),
        column(width = 6,
               plotOutput("VIX_Compare_plot",
                          width = "100%",
                          height = "200px"),
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p("Correlation:"),
                 p("30 days: ", Corr_SP500Vix_10YrTrVix_30day),
                 p("6 months: ", Corr_SP500Vix_10YrTrVix_6mo),
                 p("2 years: ", Corr_SP500Vix_10YrTrVix_2yr ),
                 p("Central bank easing and liquidty conditions driving increased
                   correlation.")
               )
        )
      ),
      fluidRow(
        # Row 4, Column 1 - Inflation Chart
        column(width = 6,
               plotOutput("BE_Inflation_plot",
                          width = "100%",
                          height = "200px"),
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p("Date: ", BE_Inflation_date_last),
                 p("Last: ", BE_Inflation_last),
                 p("1mo Prior: ", BE_Inflation_1mo),
                 p("6mo Prior: ", BE_Inflation_6mo),
                 p("Notes: Market re-priced after Fed signalled slowing hikes.")
               )
        ),
        column(width = 6,
               plotOutput("CrudeOil_plot",
                          width = "100%",
                          height = "200px"),
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p("Date: ", CrudeOil_date_last),
                 p("Last: ", CrudeOil_last),
                 p("1Week Change: ", CrudeOil_1wk_chg),
                 p("1Mo Change: ", CrudeOil_1mo_chg),
                 p("Upsdide Risks: Export disruptions in Venezuela;
                   easing of trade tensions, China consumption resiliancy,;
                   political instability in Libya"),
                 p("Downside Risks: OPEC production cut repealed,
                   credit crunch in India, global recession")
               )
        )
      )
    ),
    # tabItem(
    #   tabName="US",                               ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;}'))),
    #   fluidRow(
    #     shinydashboard::box(
    #       width = 12,
    #       title = "U.S. Economy Indicators",
    #       p("Upcoming releases: "))),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("US.TreasuryRate.Move_plot",
    #                       width = "100%",
    #                       height = "200px")
    #     ),
    #     column(width = 6,
    #            plotOutput("GDP_US_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(GDP_US_last),
    #              p(GDP_US_1.prior),
    #              p(GDP_US_2.prior)
    #            )
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("US10Yr.CPI.Corr.3mo_plot",
    #                       width = "100%",
    #                       height = "200px")
    #     ),
    #     column(width = 6,
    #            plotOutput("US10Yr.WkClaims.Corr.3mo_plot",
    #                       width = "100%",
    #                       height = "200px")
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("ConsSentiment_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(ConsSentiment_last),
    #              p(ConsSentiment_1mo.prior),
    #              p(ConsSentiment_2mo.prior)
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("RealOutputPerHrPP_US_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(RealOutputPerHrPP_US_last),
    #              p(RealOutputPerHrPP_US_1mo.prior),
    #              p(RealOutputPerHrPP_US_2mo.prior)
    #              )
    #            )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("LaborProductivity_US_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(LaborProductivity_US_last),
    #              p(LaborProductivity_US_1yr.prior),
    #              p(LaborProductivity_US_2yr.prior),
    #              p("Notes: Labor productivity growth slow but positive. Far from
    #                recession peak. Expected to remain positive in 2018.")
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("RealOutputManufac_US_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(RealOutputManufac_US_last),
    #              p(RealOutputManufac_US_1.prior),
    #              p(RealOutputManufac_US_2.prior)
    #            )
    #     )
    #     ),
    #   # Row 5 #####
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("RetailSales_exAuto_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(RetailSales_exAuto_last),
    #              p(RetailSales_exAuto_prior.1),
    #              p(RetailSales_exAuto_prior.2)
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("CurrenAccount_US_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Notes: Dollar strength driving up import volumes.")
    #            )
    #     )
    #   ),
    #   # Row 6 #####
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("PPI_OilTrans_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Notes: Leading indicator of economic activity. Data signalling expansion continuing.")
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("CorpProfits_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Date of Latest Release: ", CorpProfits_date_last),
    #              p("QoQ: ", CorpProfits_1qtrChng_Pct),
    #              p("YoY: ", CorpProfits_1yrChng_Pct),
    #              p("Notes: Lagged data showing declining corporate profits in 1Q19.
    #                Likely driven by uncertainty around government shutdown.")
    #            )
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            #Swings Chart
    #            plotOutput("Swing_2pct_chart",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Notes: Equity market volatility tracking market cycles.
    #                Current volatility not signalling recession.")
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("SP500.PE.Ratio_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Notes: Equity pricing in Fed interest rate hikes.")
    #            )
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("DXY_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(DXY_last),
    #              p("1 month change: ", DXY_1mo_chg),
    #              p("6 month change: ", DXY_6mo_chg)
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("SP500.Breadth.200dma_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(SP500.Breadth.200dma_last),
    #              p(SP500.Breadth.200dma_prior.1),
    #              p(SP500.Breadth.200dma_prior.2)
    #            )
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("FinancialConditions_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Credit cycle and liquidity indicator"),
    #              p("+1: Tightening; -1: Loosening"),
    #              p("
    #                The leveraged sub-index which tracks debt and equity metrics
    #                is a leading indicator within the broader Financial Conditions
    #                Index. The sub-index rose ahead of the broad index through 2018
    #                indicating further tightening and reduced liquidity.")
    #            )
    #     )
    # )
    # ),
    # #*2.0 Fed tab
    # tabItem(
    #   tabName="Fed",                               ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;}'))),
    #   fluidRow(
    #     shinydashboard::box(
    #       width = 12,
    #       title = "Federal Reserve Indicators",
    #       p("Upcoming releases: Fed Interest Rate Decision - 4/30")),
    #     shinydashboard::box(
    #       width = 12,
    #       p(Fed_notes.1.31.19)
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            #FF10 Spread Chart
    #            plotOutput("FF10_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Date: ", FF10_last_date),
    #              p("Spead: ", FF10_last),
    #              p("10 Year: ", FF10_10yr_last),
    #              p("FedFunds: ", FF10_FF_last),
    #              p("Notes: Fed cuts rates when 10Yr-FF inverts. FOMC has no room
    #                to hike until 10yr rises. The spread inverted on 3/2/19 and has remained
    #                 in negative territory since 5/7/19. Indicator signalling
    #                 a rate cut in June 2019.")
    #              )
    #            ),
    #     column(width = 6,
    #            plotOutput("FOMC_Projections_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Notes: FOMC projecting 0 rate hikes in 2019")
    #            )
    #     )
    #     ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("DeliquencyRates_CC_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(DeliquencyRates_CC_last),
    #              p(DeliquencyRates_CC_1.prior),
    #              p(DeliquencyRates_CC_2.prior),
    #              p("Notes: Credit card deliquiency rates rising with Fed hikes. Noted by
    #                FOMC in support of dovish monetary policy.")
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("DeliquencyRates_RE_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(DeliquencyRates_RE_last),
    #              p(DeliquencyRates_RE_1.prior),
    #              p(DeliquencyRates_RE_2.prior),
    #              p("Notes: Real estate deliquency has remained low since the
    #                financial crisis.")
    #            )
    #     )
    #   ),
    #   fluidRow(
    #     ###FedWatch Table
    #     column(width = 6,
    #            tableOutput("FedWatch_kable"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Notes: Market expecting two rate cuts through 2019."),
    #              p("Source: CME, 30-Day Fed Fund futures pricing data")
    #              )
    #            )
    #     )
    #   ),
    # #*3.0 Labor tab
    # tabItem(
    #   tabName="Labor",                               ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;}'))),
    #   fluidRow(
    #     # Row 1
    #     shinydashboard::box(
    #       width = 12,
    #       title = "Labor Market Indicators",
    #       p("Upcoming releases:")),
    #     ###Job Growth Chart
    #     column(width = 6,
    #            plotOutput("Job_Growth_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Job_Growth_last),
    #              p("12mo Average: ", Job_Growth_12moAvg),
    #              p("Notes: Potentially negative signal but Feb hiring data indicative of
    #                government shutdown, midwest weather and job openings exceeding the number of unemployed persons.")
    #            )
    #     ),
    #     ####Hourly Earnings Chart
    #     column(width = 6,
    #            plotOutput("Avg_Hourly_Earnings_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Avg_Hourly_Earnings_last),
    #              p("YoY Growth ($): ", Avg_Hourly_Earnings_12moChng_USD),
    #              p("YoY Growth (%): ", Avg_Hourly_Earnings_12moChng_pct),
    #              p("Notes: Wage data indicating continued growth in labor market.
    #                Outpacing labor productivity growth will drive inflation.")
    #              )
    #            )
    #     ),
    #     fluidRow(
    #       column(width = 6,
    #              plotOutput("Unemployment_US_plot",
    #                           width = "100%",
    #                           height = "200px"),
    #              shinydashboard::box(
    #                width = NULL,
    #                height = NULL,
    #                p(Unemployment_US_last),
    #                p(Unemployment_US_1.prior),
    #                p(Unemployment_US_2.prior)
    #              )
    #              ),
    #       column(width = 6,
    #              plotOutput("ParticipationRate_US_plot",
    #                           width = "100%",
    #                           height = "200px"),
    #              shinydashboard::box(
    #                width = NULL,
    #                height = NULL,
    #                p(ParticipationRate_US_last),
    #                p(ParticipationRate_US_1.prior),
    #                p(ParticipationRate_US_2.prior)
    #              )
    #              )
    #       )
    # ),
    # #*4.0 China tab
    # tabItem(
    #   tabName="China",                      ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;
    #            font-size:300%}'))),
    #   fluidRow(
    #     # Row 1
    #     shinydashboard::box(
    #       width = 12,
    #       title = "China Market Indicators",
    #       p("Upcoming releases:")),
    #     shinydashboard::box(
    #       width = 12,
    #       p(China_notes.1),
    #       p(China_notes.2),
    #       p(China_notes.3),
    #       p(China_notes.4)
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("USDCNY_plot",
    #                       width = "100%",
    #                       height = "200px")),
    #     column(width = 6,
    #            plotOutput("China_GDP_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p("Notes: Data differs than government's release.")
    #            )
    #   )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("China_CPI_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(China_CPI_last),
    #              p(China_CPI_1.prior),
    #              p(China_CPI_2.prior)
    #            )
    #            ),
    #     column(width = 6,
    #            plotOutput("China_Exports_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(China_Exports_last),
    #              p(China_Exports_1.prior),
    #              p(China_Exports_1.prior),
    #              p("Notes: Trade war weighing on export growth.")
    #            )
    #            )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("China_PropertyPrices_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(China_PropertyPrices_last),
    #              p(China_PropertyPrices_1.prior),
    #              p(China_PropertyPrices_2.prior),
    #              p("Notes: Potential bubble in housing market.
    #                Could burst as global growth slows.")
    #            )
    #            ),
    #     column(width = 6,
    #            plotOutput("China_ExIm_Ratio_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(China_ExIm_Ratio_last),
    #              p(China_ExIm_Ratio_1.prior),
    #              p(China_ExIm_Ratio_2.prior)
    #              )
    #            )
    #   )
    # ),
    # #Argentina tab
    # tabItem(
    #   tabName="Argentina",                      ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;
    #            font-size:300%}'))),
    #   fluidRow(
    #     # Row 1
    #     shinydashboard::box(
    #       width = 12,
    #       title = "Argentina Market Indicators",
    #       p("Upcoming releases:")),
    #     shinydashboard::box(
    #       width = 12,
    #       p(ARG_notes.1),
    #       p(ARG_notes.2),
    #       p(ARG_notes.3)
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("USDARS_plot",
    #                       width = "100%",
    #                       height = "200px")),
    #     column(width = 6,
    #            plotOutput("ARGT_ETF_plot",
    #                       width = "100%",
    #                       height = "200px"))
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("Argentina_CPI_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Argentina_CPI_last),
    #              p(Argentina_CPI_1.prior),
    #              p(Argentina_CPI_2.prior),
    #              p("Notes: President Macri's approval ratings negatively correlated with
    #                inflation.")
    #            )
    #            ),
    #     column(width = 6,
    #            plotOutput("Argentina_Exp_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Argentina_Exp_last),
    #              p(Argentina_Exp_1.prior),
    #              p(Argentina_Exp_2.prior),
    #              p("Notes: Exports expected to increase in 2019 on weak ARS
    #                and stable commodity prices.")
    #              )
    #   )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("Argentina_CA_plot",
    #                       width = "100%",
    #                       height = "200px")),
    #     column(width = 6,
    #            plotOutput("Argentina_Res_plot",
    #                       width = "100%",
    #                       height = "200px"))
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("Argentina_YouthUn_plot",
    #                       width = "100%",
    #                       height = "200px"))
    #   )
    # ),
    # #*5.0 Russia tab
    # tabItem(
    #   tabName="Russia",                      ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;
    #            font-size:300%}'))),
    #   fluidRow(
    #     # Row 1
    #     shinydashboard::box(
    #       width = 12,
    #       title = "Russia Market Indicators",
    #       p("Upcoming releases:")),
    #     shinydashboard::box(
    #       width = 12,
    #       p(Russia_notes.1),
    #       p(Russia_notes.2),
    #       p(Russia_notes.3),
    #       p(Russia_notes.4)
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("RUBUSD_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(RUBUSD_last),
    #              p(RUBUSD_Chng.1mo),
    #              p(RUBUSD_Chng.1yr)
    #              )
    #            ),
    #     column(width = 6,
    #            plotOutput("Russia_Index_plot",
    #                       width = "100%",
    #                       height = "200px"))
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("Russia_GDP_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Russia_GDP_last),
    #              p(Russia_GDP_1.prior),
    #              p(Russia_GDP_2.prior)
    #            )
    #            ),
    #     column(width = 6,
    #            plotOutput("Russia_Unemp_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Russia_Unemp_last),
    #              p(Russia_Unemp_1.prior),
    #              p(Russia_Unemp_2.prior)
    #            )
    #   )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("Russia_CPI_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Russia_CPI_last),
    #              p(Russia_CPI_1.prior),
    #              p(Russia_CPI_2.prior)
    #            )
    #            )
    #   )
    # ),
    # #*6.0 Japan tab
    # tabItem(
    #   tabName="Japan",                      ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #             h3 {font-weight: bold;
    #             font-size:300%}'))),
    #   fluidRow(
    #     # Row 1
    #     shinydashboard::box(
    #       width = 12,
    #       title = "Japan Market Indicators",
    #       p("Upcoming releases:")),
    #     shinydashboard::box(
    #       width = 12,
    #       p(Japan_notes.1),
    #       p(Japan_notes.2),
    #       p(Japan_notes.3)
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 12,
    #            plotOutput("JGB10yr2yr_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = 4,
    #              height = NULL,
    #              p("Date: ", JGB10yr2yr_5Yr_date_last),
    #              p("10-Year: ", JGB10yr_last),
    #              p("2-Year: ", JGB2yr_last),
    #              p("Spread: ", JGB10yr2yr_last)
    #            ),
    #            shinydashboard::box(
    #              width = 8,
    #              height = NULL,
    #              p("Yield Curve Control (YCC) Announcement: 9/21/16"),
    #              p("Quantitative and qualitative easing with yield curve control (QQE-YCC) policy to anchor 10-year yields close to zero, but risk of 10-year target adjustment in 2019"),
    #              p("Inflation signal: Wage increases have started to move in the right direction, but CPI inflation is still far from the BOJ’s 2% target."),
    #              p("The BOJ will likely stick with its QQE-YCC program, designed to cap 10-year yields, through 2019. But further tweaks are possible.")
    #              )
    #            )
    #     ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("Japan_Infl_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Japan_Infl_last),
    #              p(Japan_Infl_1.prior),
    #              p(Japan_Infl_2.prior),
    #              p("Notes: Inflation reached 2014 high around tax hike. Expect similar rise
    #                around October 2019 hike.")
    #            )
    #            ),
    #     column(width = 6,
    #            plotOutput("Japan_Yield_Vol_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(Japan_Yield_Vol_last),
    #              p(Japan_Yield_Vol_1.prior),
    #              p(Japan_Yield_Vol_2.prior)
    #            ))),
    #   fluidRow(
    #     column(width = 12,
    #            plotOutput("Japan_Bond_TO_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = 12,
    #              height = NULL,
    #              div(style = 'overflow-y: scroll'),    #not working
    #              collapsible = TRUE,
    #              p(Japan_Bond_TO_last),
    #              p(Japan_Bond_TO_1.prior),
    #              p(Japan_Bond_TO_2.prior),
    #              p("Notes: Chart shows JGB liquidity driven by trading volume -->
    #                increasing volume --> turnover ratio rises implying more liqudity
    #                --> prices fall")
    #              )
    #            )
    #     )
    #   ),
    tabItem(
      tabName="TradeThemes",                      ####new tab here
      tags$head(
        tags$style(
          HTML('
               h3 {font-weight: bold;
               font-size:300%}'))),
      fluidRow(
        # Row 1
        shinydashboard::box(
          width = 12,
          title = "Trade Themes")
        ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          strong("GBP-EUR Pair Trade")
      )
      ),
      fluidRow(
        column(width = 4,
               plotOutput("GBPEUR_spread_plot",
                          width = "100%",
                          height = "200px")
               ),
        column(width = 4,
               plotOutput("GBPEUR.returns",
                          width = "100%",
                          height = "200px")
               ),
        column(width = 4,
               plotOutput("GBPEUR.zscore_plot",
                          width = "100%",
                          height = "200px")
        )
      ),
      fluidRow(
        column(width = 6,
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p("The GBP weakened in 2016 as the BOE cut its benchmark rate by a quarter
                   point to stimulate the economy following the Brexit vote. The move showed
                   the GBP to break historical correlations with the EUR. The GBP/USD and
                   EUR/USD were found to be trend stationary. A mean reversion trade strategy
                   implemented in January 2018 delivers the results in the table to the right.")
               )
        ),
        column(width = 6,
               tableOutput("Annualized_Returns_kable")
               )
        ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          strong("Hong Kong Dollar Peg")
        )
      ),
      fluidRow(
        column(width = 4,
               plotOutput("USDHKD_plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 4,
               plotOutput("HKPP_plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 4,
               plotOutput("HK_PrvtCredit_plot",
                          width = "100%",
                          height = "200px")
        )
      ),
      fluidRow(
        column(width = 12,
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p(HKD_Peg_Notes)
               )
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          strong("Yield Curve Inversion and Lending Standards")
        )
      ),
      fluidRow(
        column(width = 12,
               plotOutput("US10YR_1YR_Spread_plot",
                          width = "100%",
                          height = "200px")
        )
        ),
      fluidRow(
        column(width = 6,
               plotOutput("VBLTX_plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 6,
               plotOutput("US10_1Sread.Vs.BankTightening_plot",
                          width = "100%",
                          height = "200px")
        )
        ),
      fluidRow(
        column(width = 12,
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p(US10_1_Spread.notes)
               )
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          strong("Frog in the Pan Hypothesis: Information Discreteness for Return Continuation")
        )
      ),
      fluidRow(
        column(width = 4,
               plotOutput("BLL_plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 4,
               plotOutput("MKC_plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 4,
               plotOutput("CHD_plot",
                          width = "100%",
                          height = "200px")
        )
      ),
      fluidRow(
        column(width = 12,
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p(Momentum_ID.Notes.1),
                 p(Momentum_ID.Notes.2),
                 p(Momentum_ID.Notes.3),
                 p(Momentum_ID.Notes.4)
               )
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          strong("Macro Spread Trades: Country ETFs to Asia-ExJapan")
        )
      ),
      fluidRow(
        column(width = 6,
               plotOutput("Spain.Sweden.plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 6,
               plotOutput("Netherlands.Finland.plot",
                          width = "100%",
                          height = "200px")
        )
      ),
      fluidRow(
        column(width = 12,
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p(MacroArb.Notes.1),
                 p(MacroArb.Notes.2),
                 p(MacroArb.Notes.3)
               )
        )
      ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          strong("Zimbabwe Government Debt")
        )
      ),
      fluidRow(
        column(width = 4,
               plotOutput("Zimbabwe.Debt2GDP_plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 4,
               plotOutput("Zimbabwe.STdebt_plot",
                          width = "100%",
                          height = "200px")
        ),
        column(width = 4,
               plotOutput("Zimbabwe.NetRes_plot",
                          width = "100%",
                          height = "200px")
        )
      ),
      fluidRow(
        column(width = 12,
               shinydashboard::box(
                 width = NULL,
                 height = NULL,
                 p(Zimbabwe.notes)
               )
        )
        ),
      fluidRow(
        shinydashboard::box(
          width = 12,
          strong("Flood-control Infrastructure")
        )
      ),
      fluidRow(
        column(width = 6,
                plotOutput("Flood_index_plot",
                           width = "100%",
                           height = "200px")
               ),
        column(width = 6,
                shinydashboard::box(
                  width = NULL,
                  height = NULL,
                  p(Flood.index.notes)
                  )
               )
        )
      ),
    # tabItem(
    #   tabName="Currencies",                      ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;
    #            font-size:300%}'))),
    #   fluidRow(
    #     # Row 1
    #     shinydashboard::box(
    #       width = 12,
    #       title = "Currencies",
    #       p("Upcoming releases:")
    #       )
    #     ),
    #   fluidRow(
    #     column(width = 6,
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              strong("International Fisher Effect")
    #              ),
    #            tableOutput("IFE_kable")
    #     ),
    #     column(width = 6,
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              strong("Purchasing Power Parity")
    #            ),
    #            tableOutput("PPP_kable")
    #     )
    #     ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("USDEUR_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(USDEUR_last),
    #              p("1 week change: ", USDEUR_1wk_chg),
    #              p("1 month change: ", USDEUR_1mo_chg)
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("USDGBP_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(USDGBP_last),
    #              p("1 week change: ", USDGBP_1wk_chg),
    #              p("1 month change: ", USDGBP_1mo_chg)
    #            )
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("USDJPY_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(USDJPY_last),
    #              p("1 week change: ", USDJPY_1wk_chg),
    #              p("1 month change: ", USDJPY_1mo_chg)
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("USDCHF_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(USDCHF_last),
    #              p("1 week change: ", USDCHF_1wk_chg),
    #              p("1 month change: ", USDCHF_1mo_chg)
    #            )
    #            )
    #     ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("AUDUSD_plot",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(AUDUSD_last),
    #              p("1 week change: ", AUDUSD_1wk_chg),
    #              p("1 month change: ", AUDUSD_1mo_chg)
    #            )
    #     ),
    #     column(width = 6,
    #            plotOutput("RUBUSD_plot2",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(RUBUSD_last),
    #              p("1 week change: ", RUBUSD_1wk_chg),
    #              p("1 month change: ", RUBUSD_1mo_chg)
    #            )
    #     )
    #   ),
    #   fluidRow(
    #     column(width = 6,
    #            plotOutput("ARSUSD_plot2",
    #                       width = "100%",
    #                       height = "200px"),
    #            shinydashboard::box(
    #              width = NULL,
    #              height = NULL,
    #              p(ARSUSD_last),
    #              p("1 week change: ", ARSUSD_1wk_chg),
    #              p("1 month change: ", ARSUSD_1mo_chg)
    #            )
    #     )
    #     ),
    #   fluidRow(
    #     column(width = 12,
    #            tableOutput("CorrDiffTable_kable"))),
    #   fluidRow(
    #     column(width = 12,
    #            tableOutput("CorrMatrix_G10_6mo_kable"))),
    #   fluidRow(
    #     column(width = 12,
    #            tableOutput("CorrMatrix_G10_2yr_kable")
    #     )
    #   )
    #   ),
    # tabItem(
    #   tabName="EconomicCalendar",                      ####new tab here
    #   tags$head(
    #     tags$style(
    #       HTML('
    #            h3 {font-weight: bold;
    #            font-size:300%}'))),
    #   fluidRow(
    #     # Row 1
    #     shinydashboard::box(
    #       width = 12,
    #       p("Date Updated: ", event_dates.date),
    #       uiOutput("tab2")
    #     )),
    #   fluidRow(
    #     column(width = 12,
    #            tableOutput("event_dates")
    #            )
    #     )
    #   ),
    tabItem(
      tabName="Contact",                      ####new tab here
      tags$head(
        tags$style(
          HTML('
               h3 {font-weight: bold;
               font-size:300%}'))),
      fluidRow(
        # Row 1
        shinydashboard::box(
          width = 12,
          strong("Michael LoGalbo, CFA"),
          p("MLoGalbo@wharton.upenn.edu"),
          uiOutput("tab")
        )
        )
    )
    )
)

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin = "blue")

