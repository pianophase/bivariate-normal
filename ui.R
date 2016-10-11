library(shiny)
shinyUI(
    fluidPage(
        titlePanel("Distribución Normal Bivariada"),
        wellPanel(
        fluidRow(
            column(6, h4("Función de densidad"),
                   plotOutput("densidad"),
                   withMathJax("$$f_X(x) = \\frac{1}{2\\pi\\sigma_x\\sigma_y\\sqrt{1-\\rho^2}} \\exp{\\left (-\\frac{1}{2 (1-\\rho^2)} \\left [\\left (\\frac{x-\\mu_x}{\\sigma_x} \\right )^2 + \\left (\\frac{y-\\mu_y}{\\sigma_y}\\right )^2 - 2\\rho \\frac{(x-\\mu_x)(y-\\mu_y)}{\\sigma_x \\sigma_y} \\right ] \\right)}$$")
                   ),
            column(6, h4("Curvas de nivel"),
                   tabsetPanel(type = "tabs", 
                               tabPanel("Contorno", plotOutput("contorno")), 
                               tabPanel("Condicional",
                                        plotOutput("condicional"),
                                        sliderInput("y0",  withMathJax("$$y_0$$"),
                                                    min = -5, max = 5, value = 0,
                                                    step=0.01)))
                   ))),
        wellPanel(
        fluidRow(
            column(3,
                   h4("Media"),
                   sliderInput("mx", withMathJax("$$\\mu_x$$"), 
                               min = -5, max = 5, value = 0,step=.1),
                   sliderInput("my", withMathJax("$$\\mu_y$$"),  
                               min = -5, max = 5, value = 0,step=.1)),
            column(3,
                   h4("Desvíos"),
                   sliderInput("sx", withMathJax("$$\\sigma_x$$"),
                               min = 0, max = 5, value = 1,step=0.01),
                   sliderInput("sy",  withMathJax("$$\\sigma_y$$"),
                               min = 0, max = 5, value = 1,step=0.01)),
            column(3, h4("Correlación"),
                   sliderInput("rho", withMathJax("$$\\rho$$"),
                               min = -1, max = 1, value = 0,step=0.01),
                   checkboxInput("calor", "Gris", value = FALSE, width = NULL)),
            column(3,h4("Vista"),
                   numericInput("phi", withMathJax("$$\\phi$$"), 30,
                                min = 1, max = 90),
                   numericInput("theta", withMathJax("$$\\theta$$"), 30,
                                min = 1, max = 90),
                   numericInput("grilla", "Grilla:", 50,
                                min = 1, max = 200)
                   ))
            ))
            )
