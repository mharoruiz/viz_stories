library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(viridis)

# Import data
dENERGY = read_csv("prices18.csv", col_types = "Dcn")
dSCOT <- read_csv("scot_wide.csv")
dFAO <- read_csv("fao_fpi.csv")
d5 <- read_csv('topbanks.csv')
d6 <- read_csv('countries.csv')

##  *****  UI  *****
ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "litera", 
                                        primary ="#0a7e8c"),
                title = "Visual stories | Miguel Haro Ruiz",
                fluidRow(
                  column(width = 10, offset = 1,
                         
                         tags$h1('A collection of visual stories'),
                         tags$p('Miguel Haro Ruiz',
                                       style = 'text-align:justify;
                                       margin-bottom: 40px;
                                       margin-top: 10px;
                                       line-height: 27px;
                                       font-size: 20px;'),
                         
                         tags$p('This page contains some of the data-driven projects I 
                                have been working on over the last months.',
                                tags$b('The graphs displayed below are fully interactive;'),
                                'you can hover around them with your mouse, zoom in and out, 
                                and select the information that you want to see. 
                                This way you may discover some of the stories that I missed.',
                                style = 'text-align:justify;
                                       margin-bottom: 40px;
                                       margin-top: 20px;
                                       line-height: 27px;
                                       font-size: 18px;'),
                         
                         tags$hr(),
                         
                         #tags$br(),
                         
                         tags$h2('What can we learn about the EU energy market 3 
                                 months after the implementation of the Iberian exception?',
                                 style = 'margin-bottom: 40px'),
                         fluidRow(
                           column(width = 4,
                                  tags$p('On June 15, the so-called Iberian exception 
                                         entered into force, allowing', 
                                         tags$b('Spain and Portugal to cap the price of gas'),
                                         'at €40 per megawatt-hour. The mechanism includes a 
                                         gas fee passed onto consumers to compensate gas firms, 
                                         computed as the difference between the auctioned price 
                                         of electricity at a given hour and the cap. It has 
                                         been argued that the new policy not only has failed 
                                         to lower electricity prices but pushed them 
                                         up instead. As seen in the figure on the right, post-intervention 
                                         prices are higher than before the policy. However,',
                                         tags$b('how has the price of electricity evolved in  
                                                countries with no control mechanism?'),
                                         style = 'text-align:justify;
                                         margin-tom: 15px;
                                         margin-bottom: 15px;
                                         line-height: 30px;
                                         font-size: 16px;'  ) ),
                           column(width = 8,
                                  plotlyOutput("fig1", height = 500) )
                         ), # Figure 1
                         
                         tags$br(),
                         
                         fluidRow(
                           column(width = 8,
                                  plotlyOutput("fig2", height = 600) ), 
                           column(width = 4,
                                  tags$p('We can draw some key insights by looking at energy
                                         prices of other European countries before and after the 
                                         implementation of the Iberian exception. Firstly,', 
                                         tags$b('post-intervention prices are higher and more volatile'),
                                         'across the board, with large day-to-day fluctuations. Despite this,',
                                         tags$b('Spain has managed to keep average daily prices 
                                                just below €300,'),
                                         ' much lower than its neighbouring countries. 
                                         What does this say about the EU energy market? The 
                                         way the market is set up makes consumers pay for 
                                         the most expensive source of energy, usually gas and coal. 
                                         By limiting the price of gas and setting the overall 
                                         electricity price based on renewables, Spain has achieved 
                                         a systematic reduction in overall prices.',
                                         tags$b('This illustrates the need for reform in the EU’s 
                                                energy price-setting mechanism.'),
                                         'Not only to reduce prices for consumers and alleviate inflation, 
                                         but also to effectively promote a green-energy transition in the region.',
                                         style = 'text-align:justify;
                                         margin-tom: 15px;
                                         margin-bottom: 15px;
                                         line-height: 30px;
                                         font-size: 16px;' ) )
                           ), # Figure 2
                         
                         tags$br(),
                         
                         tags$hr(),
                         
                         tags$br(),
                         
                         tags$h2('Large investors are profiting from Russia’s invasion of Ukraine,
                                 amplifying global food insecuirity in the process',
                                 style = 'margin-bottom: 40px'),
                         
                         fluidRow(
                           column(width = 5,
                                  tags$p('Russia’s invasion of Ukraine is affecting all of us in one way or 
                                         another. High inflation and the loss of purchasing power have 
                                         extended across the global economy, and while the surge in energy prices is 
                                         receiving most of the attention, the price of grains has also 
                                         substantially increased after the invasion. To understand how this 
                                         happened, we must turn to futures markets. Speculation in futures 
                                         markets allows',
                                         tags$b('commercial traders'),
                                         'to protect themselves against 
                                         price increases. Furthermore,', 
                                         tags$b('non-commercial speculators'),
                                         'seek to benefit by betting long for prices to go up and short for 
                                         prices to go down. These speculators provide liquidity to markets and 
                                         take on the risk that traders seek to get rid of. A third type of 
                                         agent in futures markets are',
                                         tags$b('index speculator,'),
                                         'composed of institutional investors such as large banks and hedge 
                                         funds who are attracted to commodities such as grain in high 
                                         inflation environments. While the number of index speculators is small, 
                                         they take on very large positions, for which their actions can push 
                                         prices up. Portfolio manager Michael Masters goes as far as to 
                                         assert that index speculators',
                                         tags$b('consume liquidity and provide zero 
                                         benefits to the futures market.'),
                                         style = 'text-align:justify;
                                         margin-tom: 15px;
                                         margin-bottom: 15px;
                                         line-height: 30px;
                                         font-size: 16px;') ), 
                           column(width = 7,
                                  plotlyOutput("fig3", height = 625) ) 
                         ), # Figure 3
                         
                         fluidRow(
                           column(width = 12, 
                                  tags$p('Russia and Ukraine are among the world’s top 5 producers of wheat. 
                                         As the possibility of a direct conflict between the two countries became 
                                         apparent, investors realised the global supply of wheat would decline substantially. 
                                         The graph above shows the number of positions held by different types of agents at 
                                         the soft wheat Chicago Board of Trade, the world’s oldest futures and options exchange 
                                         and the largest commodity exchange in the world. It can be inferred that the number of 
                                         long positions held by index speculators steadily increases before to the invasion.',
                                         tags$b('As the invasion began, wheat futures markets experienced a strong surge in 
                                         index traders betting long'),
                                         'on wheat, which lasts up to a month.',
                                         style = 'text-align:justify;
                                         margin-tom: 15px;
                                         margin-bottom: 15px;
                                         line-height: 30px;
                                         font-size: 16px;')),
                           
                           column(width = 12, offset = 0,
                                  plotlyOutput("fig4")
                                  ),# Figure 4
                           
                           column(width = 12, 
                                  tags$p('In this sense, large institutional investors are profiting from the uncertainty that 
                                         Russia’s invasion of Ukraine is bringing into the international cereal market. Most 
                                         importantly, however,',
                                         tags$b('this behaviour has contributed to a large spike in global cereal prices'),
                                         'as seen on the graph above. Current food prices are the highest in more than a decade, 
                                         and people in food-importing areas of Africa and the Middle East are simply unable 
                                         to afford the grains to meet their caloric needs. This is the latest example of 
                                         the dramatic consequences of commodity price speculation by institutional investors.',
                                         style = 'text-align:justify;
                                         margin-tom: 15px;
                                         margin-bottom: 15px;
                                         line-height: 30px;
                                         font-size: 16px;'))
                         ), # Part 2
                         
                         tags$br(),
                         
                         tags$hr(),
                         
                         tags$br(),
                         
                         tags$h2('These banks are behind global deforestation',
                                 style = 'margin-bottom: 40px'),
                         
                         fluidRow(
                           column(width = 4,
                                  tags$p('How is credit channeled through the financial system into the most forest-damaging 
                                    industries worldwide? The following visualisations allow you to',
                                    tags$b('identify where the credit comes from, as well as the main banks and financial
                                    conglomerates that support deforestation-driving sectors'),
                                    'in three different world regions. The map on the right shows the total credit directed towards
                                    forest-damaging industries by country, based on banks’ headquarters. Deforestation  is 
                                    overwhelmingly funded by Brazilian banks —mainly Banco do Brasil— although 
                                    banks from the US bear responsibility as well.',
                                    style = 'text-align:justify;
                                    margin-tom: 15px;
                                    margin-bottom: 15px;
                                    line-height: 30px;
                                    font-size: 16px;') 
                                    
                           ),
                           column(width = 8,
                                  plotlyOutput(outputId = 'fig6', height = 450)
                           )
                         ), 
                         
                         tags$br(),
                         
                         fluidRow(
                           column(width = 8,
                                  fluidRow(column(width = 3, offset = 1,
                                                  uiOutput('SelectRegion5') ),
                                           column(width = 3, 
                                                  uiOutput('SelectSector5') ),
                                           column(width = 4, 
                                                  sliderInput(inputId = "SliderYear5",
                                                              label = tags$b("Adjust time range"),
                                                              min = 2013, max = 2021,
                                                              value = c(2013, 2021),
                                                              ticks = FALSE, sep = ''))
                                           ),
                                  
                                  tags$hr(),
                                  
                                  plotlyOutput(outputId = 'fig5')
                                  
                                  
                                  ),
                           
                           column( width = 4,
                                   tags$p('Next up, we turn to the main banks financing forest-damaging sectors in three different regions.',
                                          tags$b('Banco do Brasil'), 
                                          '(Brazil) stands out in Latin America, with big stakes in beef, soy and rubber production.',
                                          tags$b('Santander'),
                                          '(Spain) also has held sizeable positions in these sectors over the past decade.
                                          The playfield is more levelled in Southeast Asia, although',
                                          tags$b('Bank Rakyat'),
                                          '(Indonesia) has a strong presence in the paper sector. An interesting development in the last year 
                                          is the strong support that',
                                          tags$b('Permodalan Nasional Berhad'),
                                          'is granting to palm oil and beef in Southeast Asia, as well as palm oil in Central & West Africa. 
                                          Also significant is the',
                                          tags$b('increasingly important role of Chinese banks in all forest-damaging sectors.'),
                                          'This includes Agricultural Bank of China, Bank of China and CITIC.',
                                          style = 'text-align:justify;
                                          margin-tom: 15px;
                                          margin-bottom: 15px;
                                          line-height: 30px;
                                          font-size: 16px;') )
                           
                           )
                         ) # Main Column
                  ) # Fluid Row
                )

##  *****  SERVER  *****
server <- function (input, output){ 
  
  treatment = as.Date("2022-06-15", "%Y-%m-%d")
  
  vline <- function(x = 0, color = "black") {
    list( type = "line",
          y0 = 0, y1 = 1,
          x0 = x,  x1 = x,
          yref = "paper",
          line = list(color = color, dash = "dot") ) }
  
  
  start_date1 = as.Date("2022-01-01", "%Y-%m-%d") 
  end_date1 = as.Date("2022-09-15", "%Y-%m-%d")
  
  d_esp = dENERGY %>% 
    filter(country == "Spain" |
             country == "Spain_raw" ) %>%
    filter(date >= start_date1 &
             date <= end_date1 ) %>%
    mutate(country = case_when(
      country == "Spain" ~ "Adjusted",
      country == "Spain_raw" ~ "Unadjusted" )) %>% 
    pivot_wider(names_from = "country",  values_from = "price")
  
  xaxis1  = list(title = "", 
                 type = "date",
                 showgrid = FALSE,
                 showline = FALSE, 
                 range = c(start_date1 -5, end_date1 +5))
  
  yaxis1  = list(title = "", showgrid = TRUE, range = c(0, 600))
  title1  = list(text = "Average daily electricity prices in Spain, auctioned vs adjusted\n€/MWh",
                 x = 0.025, y = 0.95,
                 xanchor = "left")
  margin1 = list(pad = 10, t= 80, b = 65, l = 50, r = 50)
  
  output$fig1 <- renderPlotly({ 
    
    fig1 <- 
      
      plot_ly(d_esp, x = ~date, y = ~Adjusted, type = 'scatter', mode = 'lines',
              line = list(color = 'rgb(70,130,180)', width = 3),
              showlegend = FALSE, name = 'Adjusted price') %>% 
      add_trace(y = ~Unadjusted, type = 'scatter', mode = 'lines',
                fill = 'tonexty', fillcolor='rgb(255,245,238)', 
                line = list(color = 'rgb(30,144,255)', width = 3),
                showlegend = FALSE, name = 'Auction price') %>% 
      add_annotations(text = c("Policy", "intervention") ,  
                      x = rep(treatment -1, 2), y = c(580, 540),
                      showarrow = FALSE, xanchor = "right",
                      hovertext = "June 15", font = list(size = 15)) %>% 
      add_annotations(text = "Auction price",
                      x = as.Date("2022-05-02"), y = 60, 
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = 15, color = 'rgb(30,144,255)')) %>% 
      add_annotations(text = "Adjusted price",
                      x = as.Date("2022-07-23"), y = 365, 
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = 15, color = "rgb(70,130,180)")) %>%
      add_annotations(text = "Gas fee",
                      x = as.Date("2022-08-01"), y = 200, 
                      showarrow = FALSE, xanchor = "center",
                      font = list(size = 15, color = "rgb(128,128,128)")) %>% 
      add_annotations(text = "Source: ESIOS, OMIE, own elaboration.",
                      x = 0, y = -0.18,  
                      xref = "paper", yref = "paper",
                      xanchor = "left",
                      showarrow = FALSE, 
                      font = list(size = 12, color = "rgb(169,169,169)")) %>%
      
      layout(title  = title1, 
             margin = margin1,
             xaxis  = xaxis1,
             yaxis  = yaxis1,
             shapes = vline(treatment))
    
    return(fig1)
    
    })
  
  start_date2 = as.Date("2022-03-15", "%Y-%m-%d") 
  end_date2 = as.Date("2022-09-15", "%Y-%m-%d")
  
  d_four <- dENERGY %>% 
    filter(country == "France" |
             country == "Italy" |
             country == "Germany" |
             country == "Spain" ) %>%
    filter(date >= start_date2 &
             date <= end_date2 ) %>% 
    pivot_wider(names_from = "country",  values_from = "price")
  
  pre = d_four %>% 
    filter(date >= start_date2 &
             date < treatment )  %>% 
    mutate(PreFrance = mean(France),
           PreGermany = mean(Germany),
           PreItaly = mean(Italy),
           PreSpain = mean(Spain)) %>% 
    select( c(date, PreFrance, PreGermany,
              PreItaly, PreSpain))
  post = d_four %>% 
    filter(date >= treatment &
             date <= end_date2 )  %>% 
    mutate(PostFrance = mean(France),
           PostGermany = mean(Germany),
           PostItaly = mean(Italy),
           PostSpain = mean(Spain)) %>% 
    select( c(date, PostFrance, PostGermany,
              PostItaly, PostSpain))
  
  d_four = merge(merge(d_four, pre, by = "date", all = TRUE),
                post, by = "date", all = TRUE)
  
  output$fig2 <- renderPlotly({ 
    
    xaxis2  = list(title = "", 
                   type = "date",
                   showgrid = FALSE,
                   showline = FALSE, 
                   range = c(start_date2 -5, end_date2 +5))
    
    yaxis2  = list(title = "", showgrid = TRUE, range = c(0, 800))
    title2  = list(text = "Average daily electricity prices in selected countries\n€/MWh",
                   x = 0.025, y = 0.95,
                   xanchor = "left")
    margin2 = list(pad = 10, t= 80, b = 60, l = 50, r = 50)
    
    fig2 <- 
      
      # price curves
      plot_ly(d_four, x = ~date, y = ~Germany, name = "Germany", type = "scatter", mode = "lines",
              line = list(color = 'rgb(122,83,143)', width = 2)) %>% 
      add_trace(y = ~Italy, name = "Italy", 
                line = list(color = 'rgb(241,194,50)', width = 2)) %>% 
      add_trace(y = ~France, name = "France", 
                line = list(color = 'rgb(0,128,128)', width = 2)) %>%
      add_trace(y = ~Spain, name = "Spain",  
                line = list(color = 'rgb(198,63,53)', width = 2)) %>% 
      
      # pre means
      add_trace(y = ~ PreGermany, name = "Mean Germany",  showlegend = FALSE,
                line = list(color = 'rgb(122,83,143)', width = 2, dash = "dash")) %>% 
      add_trace(y = ~ PreItaly, name = "Mean Italy",  showlegend = FALSE,
                line = list(color = 'rgb(241,194,50)', width = 2, dash = "dash")) %>%
      add_trace(y = ~ PreFrance, name = "Mean France",  showlegend = FALSE,
                line = list(color = 'rgb(0,128,128)', width = 2, dash = "dash")) %>% 
      add_trace(y = ~ PreSpain, name = "Mean Spain",  showlegend = FALSE,
                line = list(color = 'rgb(198,63,53)', width = 2, dash = "dash")) %>% 
      
      # post means 
      add_trace(y = ~ PostGermany, name = "Mean Germany",  showlegend = FALSE,
                line = list(color = 'rgb(122,83,143)', width = 2, dash = "dash")) %>% 
      add_trace(y = ~ PostItaly, name = "Mean Italy",  showlegend = FALSE,
                line = list(color = 'rgb(241,194,50)', width = 2, dash = "dash")) %>%
      add_trace(y = ~ PostFrance, name = "Mean France",  showlegend = FALSE,
                line = list(color = 'rgb(0,128,128)', width = 2, dash = "dash")) %>%
      add_trace(y = ~ PostSpain, name = "Mean Spain",  showlegend = FALSE,
                line = list(color = 'rgb(198,63,53)', width = 2, dash = "dash")) %>% 
      
      
      add_annotations(text = c("Policy", "intervention") ,  
                      x = rep(treatment -1, 2), y = c(780, 740),
                      showarrow = FALSE, xanchor = "right",
                      hovertext = "June 15", font = list(size = 15)) %>% 
      add_annotations(text = "Post-intervention", 
                      x = as.Date("2022-08-28"), y = 285,
                      ax = 0, ay = 115,
                      arrowcolor = 'rgb(198,63,53)', 
                      arrowhead = 2,
                      ayref = "y",
                      font = list(size = 12, color = 'rgb(198,63,53)' )) %>%
      add_annotations(text = "mean for Spain",
                      x = as.Date("2022-08-28"), y = 85,
                      showarrow = FALSE,
                      font = list(size = 12, color = 'rgb(198,63,53)' )) %>% 
      add_annotations(text = "Source: energy-charts.info, ESIOS, own elaboration.",
                      x = 0, y = -0.12,  
                      xref = "paper", yref = "paper",
                      xanchor = "left",
                      showarrow = FALSE, 
                      font = list(size = 12, color = "rgb(169,169,169)")) %>% 
      
      layout(title  = title2, 
             margin = margin2,
             xaxis  = xaxis2,
             yaxis  = yaxis2,
             shapes = vline(treatment))
    
    return(fig2)
    }) 
  
  invasion = as.Date("2022-02-22", format = "%Y-%m-%d")
  start_date3 = as.Date("2021-12-01", "%Y-%m-%d")
  end_date3 = as.Date("2022-06-01")
  
  d3 <- dSCOT %>%
    filter(date >= start_date3 &
             date <= end_date3 ) %>%
    filter(
      #market ==  "Wheat-Hrw - Chicago Board Of Trade" |
      market == "Wheat-Srw - Chicago Board Of Trade" ) %>% 
    select(market, date, com.l, com.s, ncm.l, ncm.s, idx.l, idx.s)
  
  output$fig3 <- renderPlotly({
    
    xaxis3  = list(title = "", 
                   type = "date",
                   showgrid = FALSE,
                   showline = TRUE, 
                   range = c(start_date3 -5, end_date3 +5))
  
    yaxis3  = list(title = "", showgrid = TRUE, range = c(25000, 215000)) 
    title3  = list(text = "Long and short positions held by different agents\nSoft Wheat, Chicago Board of Trade",
                 x = 0.025, y = 0.95,
                 xanchor = "left")
    margin3 = list(pad = 10, t= 60, b = 65, l = 50, r = 50)
    
    fig3 <- 
      
      plot_ly(d3, x = ~date, y = ~idx.l, name = "Index long", 
              type = "scatter", mode = "lines",
              line = list(color = 'rgb(255,126,0)', width = 3)) %>% 
      add_trace(y = ~idx.s, name = "Index short", 
                line = list(color = 'rgb(255,126,0)', width = 3, dash = "dash")) %>%
      add_trace(y = ~com.l, name = "Commercial long",
                line = list(color = 'rgb(181,126,220)', width = 3)) %>%
      add_trace(y = ~com.s, name = "Comercial short", 
                line = list(color = 'rgb(181,126,220)', width = 3, dash = "dash")) %>% 
      add_trace(y = ~ncm.l, name = "Non-commercial long",
                line = list(color = 'rgb(0,168,107)', width = 3)) %>%
      add_trace(y = ~ncm.s, name = "Non-commercial short", 
                line = list(color = 'rgb(0,168,107)', width = 3, dash = "dash")) %>%
      
      add_annotations(text = c("Invasion", "begins") ,  
                      x = c(invasion -8, invasion +6), y = c(125000, 125000),
                      showarrow = FALSE, #xanchor = "left",
                      textangle = 270,
                      hovertext = "Feb 24", font = list(size = 15)) %>%
      add_annotations(text = "Source: CFTC, own elaboration.",
                      x = 0, y = -0.12,  
                      xref = "paper", yref = "paper",
                      xanchor = "left",
                      showarrow = FALSE, 
                      font = list(size = 12, color = "rgb(169,169,169)")) %>%
      
      layout(title  = title3, 
             margin = margin3,
             xaxis  = xaxis3,
             yaxis  = yaxis3,
             shapes = list(vline(invasion),
                           list(type = "rect", fillcolor = "red",
                                line = list(color = "red"), opacity = 0.2, 
                                y0 = 25000, y1 = 215000,
                                x0 = invasion, x1 = invasion + 28 ) ) 
             )
    
    return(fig3)
    
  })
  
  start_date4 = as.Date("2012-08-01", "%Y-%m-%d")
  end_date4 = as.Date("2022-08-01")
  
  d4 <- dFAO %>% drop_na()
  
  output$fig4 <- renderPlotly({ 
    
    xaxis4  = list(title = "", 
                   type = "date",
                   showgrid = FALSE,
                   showline = TRUE, 
                   range = c(start_date4 -5, end_date4 +5))
    
    yaxis4  = list(title = "", showgrid = TRUE, range = c(50, 260)) 
    title4  = list(text = "FAO Food Price Index\n2014-2016=100",
                   x = 0, y = 0.95,
                   xanchor = "left")
    margin4 = list(pad = 10, t= 80, b = 80, l = 50, r = 50)
    
    fig4 <- 
      
      plot_ly(d4, x = ~Date, y = ~Sugar, name = "Sugar", 
              type = "scatter", mode = "lines",
              line = list(color = 'rgb(233,205,189)', width = 3)) %>%
      add_trace(y = ~Meat, name = "Meat", 
                line = list(color = 'rgb(208,175,163)', width = 3)) %>%
      add_trace(y = ~Dairy, name = "Dairy",
                line = list(color = 'rgb(160,135,129)', width = 3)) %>%
      add_trace(y = ~`Food Price Index`, name = "Food Price Index",
                line = list(color = 'rgb(53,56,57)', width = 3)) %>%
      add_trace(y = ~Oils, name = "Oils",
                line = list(color = 'rgb(38,118,136)' , width = 3)) %>%
      add_trace(y = ~Cereals, name = "Cereals",
                line = list(color = 'rgb(255,130,67)', width = 3)) %>%
      
      add_annotations(text = c("Invasion", "begins") ,  
                      x = c(invasion -50, invasion +45), y = c(80, 80),
                      showarrow = FALSE, #xanchor = "left",
                      textangle = 270,
                      hovertext = "Feb 24", font = list(size = 15)) %>%
      add_annotations(text = "Source: FAO.",
                      x = 0, y = -0.25,  
                      xref = "paper", yref = "paper",
                      xanchor = "left",
                      showarrow = FALSE, 
                      font = list(size = 12, color = "rgb(169,169,169)")) %>% 
      
      layout(title  = title4, 
             margin = margin4,
             xaxis  = xaxis4,
             yaxis  = yaxis4,
             shapes = list(vline(invasion) ) )
    
    return(fig4)

    })
  
  output$SelectRegion5 <- renderUI ({
    selectInput(inputId = 'SelectRegion5',
                label = tags$b('Select region'),
                choices = unique(d5 %>% dplyr::select(Region)),
                selected = 'Latin America')
  })
  
  output$SelectSector5 <- renderUI ({
    req(input$SelectRegion5)
    
    d_one5 <- subset(d5, Region == as.character(input$SelectRegion5))
    
    selectInput(inputId = 'SelectSector5',
                label = tags$b('Select sector'),
                choices = unique(d_one5 %>% dplyr::select(Sector)),
                selected = 'All sectors')
  })
  
  d_plot5 <- reactive({ 
    req(input$SelectSector5)
    subset(d5, Region == as.character(input$SelectRegion5) &
             Sector == as.character(input$SelectSector5) )
  })
  
  brks5 <- reactive({
    c(input$SliderYear5[1]:input$SliderYear5[2])
    
  })
  
  labs5 <- reactive({
    as.character(c(input$SliderYear5[1]:input$SliderYear5[2]))
  })
  
  lims5 <- reactive({
    c(input$SliderYear5[1] -0.5, input$SliderYear5[2] +0.5)
  })
  
  output$fig5 <- renderPlotly({
    req(brks5())
    req(labs5())
    req(lims5())
    req(d_plot5())
    
    text5 = paste("Top 10 banks financing ", as.character(input$SelectSector5), 
                  " in ", as.character(input$SelectRegion5), "\nMillion $US", sep = "")
    
    title5  = list(text = text5,
                   x = 0, y = 0.95,
                   xanchor = "left")
    
    
    fig5 <-
      ggplotly(
        ggplot(d_plot5(), aes(Year, USDmillions, fill = Bank)) +
          theme_minimal() +
          geom_col() +
          scale_fill_viridis(discrete = TRUE, direction = -1) +
          xlab('') +
          ylab('') +
          scale_x_continuous(breaks = brks5(),
                             labels = labs5(),
                             limits = lims5()) +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                #panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                plot.margin = margin(t = 45, 
                                     r = 5, 
                                     b = 30, 
                                     l = 5) )
             )
    
    fig5 <- fig5 %>%
      add_annotations(text = "Source: Forest & finance.",
                      x = 0, y = -0.15,  
                      xref = "paper", yref = "paper",
                      xanchor = "left",
                      showarrow = FALSE, 
                      font = list(size = 12, color = "rgb(169,169,169)"))%>%
      layout(title = title5) 
    
    return(fig5)
        
  })
  
  
  d_plot6 = d6 %>%
    filter(Sector == "All sectors",
           Region == "All regions")
    
  output$fig6 <- renderPlotly({
    fig6 <- plot_ly(d_plot6, 
                   type = 'choropleth', 
                   locations = d_plot6$Code, 
                   z = d_plot6$USDmillions,
                   frame = d_plot6$Year,
                   text = d_plot6$Country, 
                   colorscale = 'Cividis',
                   reversescale =T)
    
    g <- list(
      projection = list(type = 'eckert4'),
      showland = TRUE,
      landcolor = toRGB("grey90"),
      showcountries = TRUE,
      #showocean = TRUE,
      #oceancolor = toRGB("LightBlue"),
      countrycolor = toRGB("white"))
    
    t <- list(text = "Origin of credit, based on banks’ headquarters",
              font = list(size = 19),
              xanchor = 'left',
              xref = 'container',
              x = 0.05,
              yanchor = 'bottom',
              yref = 'container',
              y = 0.96)
    
    fig6 <- fig6 %>% 
      layout(title = t, geo = g) %>%
      colorbar(title = 'Millions US$') %>%
      add_annotations(text = "Source: Forest & finance, own elaboration.",
                      x = 0, y = -0.1,  
                      xref = "paper", yref = "paper",
                      xanchor = "left",
                      showarrow = FALSE, 
                      font = list(size = 12, color = "rgb(169,169,169)"))
    
    return(fig6)
    
  })
  
  
  
}


shinyApp(ui = ui,  server = server)



