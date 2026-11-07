library(shiny)
library(bslib)
library(ggplot2)

#european pricing model
european <- function(initial, strike, vol, rate, time, n, type){
  dt <- time/n
  up <- exp(vol * sqrt(dt))
  down <- 1/up
  p <- (exp(rate * dt) - down)/(up - down)
  if (p < 0 || p > 1) return(NA_real_)
  
  #binomial 
  k = 0:n
  fin <- initial * up^k * down^(n - k)
  
  #the weights on the p of price going up
  payoff <- if(type == "call") pmax(fin - strike, 0) else pmax(strike - fin, 0)
  
  #final calc + depreciation
  exp(-rate * time) * sum(dbinom(k, n, p) * payoff)
}

#actual black-sholes
bs_price <- function(S, K, r, sigma, T, type = "call") {
  d1 <- (log(S / K) + (r + sigma^2 / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  if (type == "call") S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  else K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
}

#shiny website
ui <- page_sidebar(
  titlePanel("Black-Sholes Binomial Tree Approximation vs Actual"),
  h6("Troy Kudrjavtsev, 11/5/26"),
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   bg = "#28324f", fg = "white", "font-size-base" = "0.9rem",
                   "card-bg" = "#28324f",
                   "input-bg" = "#bdd2f2",
                   "input-color" = "black",
                   "input-border-color" = "black"),
                   
  tags$style(HTML("
      .selectize-input, .selectize-input .item, .selectize-input input {
        color: #000000 !important;
      }
      .selectize-dropdown {
        background-color: #bdd2f2 !important;
      }
      .selectize-dropdown .option {
        color: #000000 !important;
      }
      .selectize-dropdown .active {
        background-color: #8fb0e0 !important;
        color: #000000 !important;
      }
  ")),
      
  sidebar = sidebar(
    width = 300, 
    numericInput("S", "Stock Price (S):", 100),
    numericInput("K", "Strike (Execution) Price (K):", 100),
    numericInput("vol", "Volatility (σ)", 0.20, min = 0.01, step = 0.01),
    numericInput("rate", "Risk-free rate (r)", 0.05, step = 0.01),
    numericInput("t", "Time to maturity (years)", 1, min = 0.01, step = 0.25),
    selectInput("type", "Option type", c("call", "put")),
    sliderInput("n", "Number of steps (n)", min = 1, max = 1000, value = 100)
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(title = "Binomial tree", height = "90px", value = textOutput("tree_val"),  theme = value_box_theme(bg = "#545d78", fg = "#bdd2f2")),
    value_box(title = "Black-Scholes", height = "90px",value = textOutput("bs_val"),   theme = value_box_theme(bg = "#545d78", fg = "#bdd2f2")),
    value_box(title = "Difference", height = "90px", value = textOutput("diff_val"), theme = value_box_theme(bg = "#545d78", fg = "#bdd2f2"))
  ),
  
  card(
    card_header("Convergence to Black-Scholes"),
    plotOutput("convergence")
  )
)

#fills output spots
server <- function(input, output) {
  bs <- reactive({
    req(input$S, input$K, input$vol, input$rate, input$t)
    bs_price(input$S, input$K, input$rate, input$vol, input$t, input$type)
  })
  
  tree <- reactive({
    req(input$S, input$K, input$vol, input$rate, input$t)
    european(input$S, input$K, input$vol, input$rate, input$t, input$n, input$type)
  })
  
  show <- function(x) if (is.na(x)) "n/a" else sprintf("%.3f", x)
  
  output$tree_val <- renderText(show(tree()))
  output$bs_val   <- renderText(show(bs()))
  output$diff_val <- renderText(show(tree() - bs()))
  
  output$convergence <- renderPlot({
    ns <- unique(round(seq(1, input$n, length.out = min(300, input$n))))
    y  <- sapply(ns, function(k)
      european(input$S, input$K, input$vol, input$rate, input$t, k, input$type))
    validate(need(any(!is.na(y)), "p is outside [0, 1]. Raise the volatility."))
    
    ggplot(data.frame(n = ns, price = y), aes(n, price)) +
      geom_line(color = "lightblue", linewidth = 1.1) +
      geom_hline(yintercept = bs(), color = "#223a82", linetype = "dashed", linewidth = 1) +
      labs(x = "Number of steps (n)", y = "Option price",
           subtitle = "Light blue: binomial tree.  Dark Blue: Black-Scholes.") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)

