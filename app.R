
library(shiny)
library(shinydashboard)
library(ggplot2)


# height data
men_mean <- 71.3 * 2.54
men_sd <- 2.92 * 2.54
women_mean <- 64.1 * 2.54
women_sd <- 2.75 * 2.54
m_x <- rnorm(2000, men_mean, men_sd)
f_x <- rnorm(2000, women_mean, women_sd)
boot_df <- c(m_x, f_x)

ui <- dashboardPage(
  dashboardHeader(
    title = "Uncertainty in HTA"
  ),
  
  # SIDEBAR -------------
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Presentation", tabName = "pres_tab", icon = icon("chalkboard-teacher")),
      menuItem("Stochastic distributions",icon = icon("chart-bar"),
               menuItem("Normal", tabName = "rnorm"),
               menuItem("Beta", tabName = "rbeta"),
               menuItem("Gamma", tabName = "rgamma"),
               menuItem("Lognormal", tabName = "rlnorm"),
               menuItem("Exponential", tabName = "rexp"),
               menuItem("Bootstrapping", tabName = "boot")
               ),
      menuItem("Excel example", tabName = "xls_tab", icon = icon("file-excel")),
      menuItem("References", tabName = "refs_tab", icon = icon("book"))
    )
  ),
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
      
      # presentation
      tabItem(
        tabName = "pres_tab",
        div(style = "resize: both; overflow: auto; width = 85vh; height: 85vh;",
            HTML(
              '<iframe src="https://docs.google.com/presentation/d/e/2PACX-1vSp4gWDhab1aCTJNFDeV9zIGaotk9KfB1d0veQGhdroud_b9WX3c4oBr3TKBn0z-z9lXWF44n0Y5f4i/embed?start=false&loop=true&delayms=60000"
            frameborder="0" width="100%" height="100%" allowfullscreen="true" mozallowfullscreen="true" webkitallowfullscreen="true"></iframe>'
            )
        )
      ),
      
      # NORMAL dist tab ----
      tabItem(
        tabName = "rnorm",
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            sliderInput(inputId = "rnorm_n",label = "n", min = 1, max = 10000,value = 1000, step = 100),
            sliderInput(inputId = "rnorm_mean",label = "mean", min = -5, max = 5,value = 1, step = 0.1),
            sliderInput(inputId = "rnorm_sd",label = "sd", min = 0, max = 10,value = 1, step = 0.1),
            actionButton("rnorm_sample","Draw again"),
            hr(),
            a("Normal distribution Wikipedia page", href = "https://en.wikipedia.org/wiki/Normal_distribution", target="_blank")
            ),
          mainPanel(
            plotOutput("rnorm_plot",width = "100%"),
            div(class = "txt_box",
              textOutput("rnorm_txt")
            ),
            br(),
            div(
              style = "margin-top: 20px; width: 50%;",
              verbatimTextOutput("rnorm_txt2")
            )
            )
          )
      ),
      
      
      # BETA dist tab ---- 
      tabItem(
        tabName = "rbeta",
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            sliderInput(inputId = "rbeta_n",label = "n", min = 1, max = 10000,value = 1000, step = 100),
            sliderInput(inputId = "rbeta_a",label = "Alpha", min = 0, max = 5,value = 2, step = 0.1),
            sliderInput(inputId = "rbeta_b",label = "Beta", min = 0, max = 5,value = 4, step = 0.1),
            actionButton("rbeta_sample","Draw again"),
            hr(),
            a("Beta distribution Wikipedia page", href = "https://en.wikipedia.org/wiki/Beta_distribution", target="_blank")
          ),
          mainPanel(
            plotOutput("rbeta_plot",width = "100%"),
            div(class = "txt_box",
                textOutput("rbeta_txt")
            ),
            br(),
            div(
              style = "margin-top: 20px; width: 50%;",
              verbatimTextOutput("rbeta_txt2")
            )
          )
        )
      ),
      
      
      
      # GAMMA dist tab ---- 
      tabItem(
        tabName = "rgamma",
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            sliderInput(inputId = "rgamma_n",label = "n", min = 1, max = 10000,value = 1000, step = 100),
            sliderInput(inputId = "rgamma_shape",label = "Shape", min = 0, max = 10,value = 2, step = 0.1),
            sliderInput(inputId = "rgamma_scale",label = "Scale", min = 0, max = 10,value = 4, step = 0.1),
            actionButton("rgamma_sample","Draw again"),
            hr(),
            a("Gamma distribution Wikipedia page", href = "https://en.wikipedia.org/wiki/Gamma_distribution", target="_blank")
          ),
          mainPanel(
            plotOutput("rgamma_plot",width = "100%"),
            div(class = "txt_box",
                textOutput("rgamma_txt")
            ),
            br(),
            div(
              style = "margin-top: 20px; width: 50%;",
              verbatimTextOutput("rgamma_txt2")
            )
          )
        )
      ),
      
      
      
      
      # LOGNORMAL dist tab ----
      tabItem(
        tabName = "rlnorm",
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            sliderInput(inputId = "rlnorm_n", label = "n", min = 1, max = 10000,value = 1000, step = 100),
            sliderInput(inputId = "rlnorm_mean",label = "meanlog", min = -3, max = 3,value = 1, step = 0.1),
            sliderInput(inputId = "rlnorm_sd",label = "sdlog", min = 0, max = 2,value = 1, step = 0.1),
            actionButton("rlnorm_sample","Draw again"),
            hr(),
            a("Log-Normal distribution Wikipedia page", href = "https://en.wikipedia.org/wiki/Log-normal_distribution", target="_blank")
          ),
          mainPanel(
            plotOutput("rlnorm_plot",width = "100%"),
            div(class = "txt_box",
                textOutput("rlnorm_txt")
            ),
            br(),
            div(
              style = "margin-top: 20px; width: 50%;",
              verbatimTextOutput("rlnorm_txt2")
            )
          )
        )
      ),
      
      
      
      
      # EXP dist tab ----
      tabItem(
        tabName = "rexp",
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            sliderInput(inputId = "rexp_n", label = "n", min = 1, max = 10000,value = 1000, step = 100),
            sliderInput(inputId = "rexp_mean",label = "rate", min = 0, max = 0.5,value = 0.05, step = 0.005),
            actionButton("rexp_sample","Draw again"),
            hr(),
            a("Normal distribution Wikipedia page", href = "https://en.wikipedia.org/wiki/Exponential_distribution", target="_blank")
          ),
          mainPanel(
            plotOutput("rexp_plot",width = "100%"),
            div(class = "txt_box",
                textOutput("rexp_txt")
            ),
            br(),
            div(
              style = "margin-top: 20px; width: 50%;",
              verbatimTextOutput("rexp_txt2")
            )
          )
        )
      ),
      
      
      
      # BOOTSTRAPPING ----
      tabItem(
        tabName = "boot",
        sidebarLayout(
          sidebarPanel(
            width = 3, 
            actionButton("boot_sample","Draw again"),
            hr(),
            a("Bootstrapping Wikipedia page", href = "https://en.wikipedia.org/wiki/Bootstrapping_(statistics)", target="_blank")
          ),
          mainPanel(
            column(6, 
                   plotOutput("boot1_plot",width = "100%"),
                   div(class = "txt_box",
                       textOutput("boot_txt")
                   )
            ),
            column(6, 
                   plotOutput("boot2_plot",width = "100%"),
                   div(class = "txt_box",
                       textOutput("boot_txt2")
                   )
            )
          )
        )
      ),
      
      
      
      # EXCEL -----------
      tabItem(
        tabName = "xls_tab",
        
        h4("Download the sample Excel workbook and complete the following tasks."),
        tags$ol(
        tags$li("Do this "),
        tags$li("And that "),
        tags$li("And this again ")
        ),
        
        br(),br(),
        
        HTML('<a class = "a-btn" href="workbook.xlsm" download="workbook.xlsm" target="_blank">
             <i class="fas fa-download"></i>
             Download Excel Workbook</a>')
        ),
      
      
      
      # REFERENCES -----------
      tabItem(
        tabName = "refs_tab",
        
        h4("References."),
        column(6,
        tags$li(a(
          "Husereau D, Drummond M, Petrou S, Carswell C, Moher D, Greenberg D, Augustovski F, Briggs AH, Mauskopf J, Loder E, ISPOR Health Economic Evaluation Publication Guidelines-CHEERS Good Reporting Practices Task Force. Consolidated health economic evaluation reporting standards (CHEERS)—explanation and elaboration: a report of the ISPOR health economic evaluation publication guidelines good reporting practices task force. Value in health. 2013 Mar 1;16(2):231-50.",
          href = "https://pubmed.ncbi.nlm.nih.gov/23538175/", 
          target="_blank"
          )),
        
        tags$li(a(
          "NICE. Guide to the Methods of Technology Appraisal 2013.",
          href = "https://www.nice.org.uk/process/pmg9/resources/guide-to-the-methods-of-technology-appraisal-2013-pdf-2007975843781", 
          target="_blank"
        )),
        
        tags$li(a(
          "Briggs AH, Weinstein MC, Fenwick EA, Karnon J, Sculpher MJ, Paltiel AD. Model parameter estimation and uncertainty analysis: a report of the ISPOR-SMDM Modeling Good Research Practices Task Force Working Group–6. Medical decision making. 2012 Sep;32(5):722-32.",
          href = "https://pubmed.ncbi.nlm.nih.gov/22990087/", 
          target="_blank"
        )),
        tags$li(a(
          "Ghabri S, Cleemput I, Josselin JM. Towards a new framework for addressing structural uncertainty in health technology assessment guidelines.",
          href = "https://link.springer.com/article/10.1007/s40273-017-0603-4", 
          target="_blank"
        )),
        
        tags$li(a(
          "Strong M, Oakley JE, Chilcott J. Managing structural uncertainty in health economic decision models: a discrepancy approach. Journal of the Royal Statistical Society: Series C (Applied Statistics). 2012 Jan;61(1):25-45.",
          href = "https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-9876.2011.01014.x", 
          target="_blank"
        )),
        
        tags$li(a(
          "Silberzahn R, Uhlmann EL, Martin DP, Anselmi P, Aust F, Awtrey E, Bahník Š, Bai F, Bannard C, Bonnier E, Carlsson R. Many analysts, one data set: Making transparent how variations in analytic choices affect results. Advances in Methods and Practices in Psychological Science. 2018 Sep;1(3):337-56.",
          href = "https://journals.sagepub.com/doi/full/10.1177/2515245917747646", 
          target="_blank"
        )),
        
        tags$li(a(
          "Luz PM, Morris BL, Grinsztejn B, Freedberg KA, Veloso VG, Walensky RP, Losina E, Nakamura YM, Girouard MP, Sax PE, Struchiner CJ. Cost-effectiveness of genotype testing for primary resistance in Brazil. Journal of acquired immune deficiency syndromes (1999). 2015 Feb 1;68(2):152.",
          href = "https://pubmed.ncbi.nlm.nih.gov/25415289/", 
          target="_blank"
        )),
        
        
        
        
        
        ""
        )
      )
      
      
      

    )
  )
)


server <- function(input, output, session){
  
  
  
# NORM   ---------------------
  rnorm_vals <- reactive({
    input$rnorm_sample
    rnorm(n = input$rnorm_n, mean = input$rnorm_mean, sd = input$rnorm_sd)
  })
  
  output$rnorm_plot <- renderPlot({
    y <- rnorm_vals()
      ggplot() +
        geom_histogram(aes(x = y, y = ..density..), col = "gray", size = 0.2, fill = "blue", alpha = 0.5, bins = length(y)/5) +
        geom_line(aes(x = seq(-10,10,0.1), y = dnorm(seq(-10,10,0.1), mean = input$rnorm_mean, sd = input$rnorm_sd))) +
        xlim(c(-10, 10)) +
        xlab("Value") +
        ylab("Frquency") +
        theme_minimal()
  })
output$rnorm_txt <- renderText({
  paste0("Mean = ", round( mean(rnorm_vals()), 2),"; SD = ", round(sd(rnorm_vals()),3))
})  
output$rnorm_txt2 <- renderText({
  vals <- paste0("\n  ", round(head(rnorm_vals()),4), collapse = "")
  paste0("Sample values: ", vals)
})




# BETA   -------------------
rbeta_vals <- reactive({
  input$rbeta_sample
  rbeta(n = input$rbeta_n, shape1 = input$rbeta_a, shape2 = input$rbeta_b)
})

output$rbeta_plot <- renderPlot({
  y <- rbeta_vals()
  ggplot() +
    geom_histogram(aes(x = y, y = ..density..), col = "gray", size = 0.2, fill = "blue", alpha = 0.5, bins = length(y)/5) +
    geom_line(aes(x = seq(0,1,0.01), y = dbeta(seq(0,1,0.01), shape1 = input$rbeta_a, shape2 = input$rbeta_b))) +
    xlim(c(0, 1)) +
    xlab("Value") +
    ylab("Frquency") +
    theme_minimal()
})
output$rbeta_txt <- renderText({
  paste0("Mean = ", round( mean(rbeta_vals()), 2),"; SD = ", round(sd(rbeta_vals()),3))
})  
output$rbeta_txt2 <- renderText({
  vals <- paste0("\n  ", round(head(rbeta_vals()),4), collapse = "")
  paste0("Sample values: ", vals)
})





# GAMMA   -------------------
rgamma_vals <- reactive({
  input$rgamma_sample
  rgamma(n = input$rgamma_n, shape = input$rgamma_shape, scale = input$rgamma_scale)
})

output$rgamma_plot <- renderPlot({
  y <- rgamma_vals()
  ggplot() +
    geom_histogram(aes(x = y, y = ..density..), col = "gray", size = 0.2, fill = "blue", alpha = 0.5, bins = length(y)/5) +
    geom_line(aes(x = seq(0,200,0.1), y = dgamma(seq(0,200,0.1), shape = input$rgamma_shape, scale = input$rgamma_scale))) +
    coord_cartesian(xlim = c(0, 200)) +
    xlab("Value") +
    ylab("Frquency") +
    theme_minimal()
})
output$rgamma_txt <- renderText({
  paste0("Mean = ", round( mean(rgamma_vals()), 2),"; SD = ", round(sd(rbeta_vals()),3))
})  
output$rgamma_txt2 <- renderText({
  vals <- paste0("\n  ", round(head(rgamma_vals()),4), collapse = "")
  paste0("Sample values: ", vals)
})




# LOGNORM   ---------------------
rlnorm_vals <- reactive({
  input$rlnorm_sample
  rlnorm(n = input$rlnorm_n, meanlog = input$rlnorm_mean, sdlog = input$rlnorm_sd)
})

output$rlnorm_plot <- renderPlot({
  y <- rlnorm_vals()
  ggplot() +
    geom_histogram(aes(x = y, y = ..density..), col = "gray", size = 0.2, fill = "blue", alpha = 0.5, bins = length(y)/5) +
    geom_line(aes(x = seq(-10,10,0.1), y = dlnorm(seq(-10,10,0.1), mean = input$rlnorm_mean, sd = input$rlnorm_sd))) +
    coord_cartesian(xlim = c(0, 10)) +
    xlab("Value") +
    ylab("Frquency") +
    theme_minimal()
})
output$rlnorm_txt <- renderText({
  paste0("Mean = ", round( mean(rlnorm_vals()), 2),"; SD = ", round(sd(rlnorm_vals()),3))
})  
output$rlnorm_txt2 <- renderText({
  vals <- paste0("\n  ", round(head(rlnorm_vals()),4), collapse = "")
  paste0("Sample values: ", vals)
})






# EXP   ---------------------
rexp_vals <- reactive({
  input$rexp_sample
  rexp(n = input$rexp_n, rate = input$rexp_mean)
})

output$rexp_plot <- renderPlot({
  y <- rexp_vals()
  ggplot() +
    geom_histogram(aes(x = y, y = ..density..), col = "gray", size = 0.2, fill = "blue", alpha = 0.5, bins = length(y)/5) +
    geom_line(aes(x = seq(0,100,0.1), y = dexp(seq(0,100,0.1), rate = input$rexp_mean))) +
    coord_cartesian(xlim = c(0, 100)) +
    xlab("Value") +
    ylab("Frquency") +
    theme_minimal()
})
output$rexp_txt <- renderText({
  paste0("Mean = ", round( mean(rexp_vals()), 2),"; SD = ", round(sd(rexp_vals()),3))
})  
output$rexp_txt2 <- renderText({
  vals <- paste0("\n  ", round(head(rexp_vals()),4), collapse = "")
  paste0("Sample values: ", vals)
})





# BOOTSTRAPPING   ---------------------

boot_vals <- reactive({
  input$boot_sample
  boot_i <- boot_df[sample(1:length(boot_df),size = length(boot_df), replace = T)]
  boot_i
  boot_i
})

output$boot1_plot <- renderPlot({
  y <- boot_df
  ggplot() +
    geom_histogram(aes(x = y, y = ..density..), col = "red", size = 0.2, fill = "pink", alpha = 0.5, bins = length(y)/30) +
    # geom_line(aes(x = seq(0,100,0.1), y = dexp(seq(0,100,0.1), rate = input$rexp_mean))) +
    coord_cartesian(xlim = c(140, 210)) +
    xlab("Value") +
    ylab("Frquency") +
    theme_minimal() +
    ggtitle("Original")
})

output$boot2_plot <- renderPlot({
  y <- boot_vals()
  ggplot() +
    geom_histogram(aes(x = y, y = ..density..), col = "gray", size = 0.2, fill = "blue", alpha = 0.5, bins = length(y)/30) +
    # geom_line(aes(x = seq(0,100,0.1), y = dexp(seq(0,100,0.1), rate = input$rexp_mean))) +
    coord_cartesian(xlim = c(140, 210)) +
    xlab("Value") +
    ylab("Frquency") +
    theme_minimal() +
    ggtitle("Resampled")
})

output$boot_txt <- renderText({
  paste0("Original Mean = ", round( mean(boot_df), 2),"; SD = ", round(sd(boot_df),3))
})  
output$boot_txt2 <- renderText({
  paste0("Resampled Mean = ", round( mean(boot_vals()), 2),"; SD = ", round(sd(boot_vals()),3))
})  








}

shinyApp(ui, server)
