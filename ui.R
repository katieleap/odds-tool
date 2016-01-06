# test

library(shiny)
library(RColorBrewer)
library(ggplot2)

# this function allows text inputs to be on the same line; default has them on new lines
textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

# begin ui
shinyUI(fluidPage(
  # call mathjax cause we want to use some sweet fraction formatting  
  withMathJax(),
  # we don't like the default css, so we're going to tinker with it a little bit
  # h1 and h2 are headers; p is the paragraph
  # making sure that there is a font type selected if the browser can't load the google font: specify sans-serif, serif or cursive
  tags$head(
    tags$style(HTML("
                    @import url(https://fonts.googleapis.com/css?family=Rock+Salt|Roboto:400,700,400italic);
                    
                    h1, h2, h3, h4, h5, h6, th {
                    font-family: 'Rock Salt', cursive;
                    }
                    
                    p, footer, .nav-tabs {
                    font-family: 'Roboto', sans-serif;
                    }
                    
                    # changing table borders to gray instead of black, increasing cell padding 
                    
                    table, td, th, tr {
                    border: 1px solid gray;
                    }
                    td, th {
                    padding: 5px;
                    }
                    
                    "))
    ), 
  
  
  # Application title
  # "How biased are the odds?" might be a better technical title; current title is more whimsical
  titlePanel("What are the odds?"),
  br(),
  
  
  # layout has tabs at the top of the page to allow different inputs for different graphs
  tabsetPanel(
    # first tab is an overview panel: introduces the problem and provides an explanation of what an odds ratio is
    tabPanel("Overview", 
             # we're going to display the table and calculations to the right of the description, to give more prominence to the plot below
             # to do this we need to use fluid row
             fluidRow(
               
               column(4, offset = 1, 
                      br(),br(),
                      # making an array of text boxes to allow input resembling an exposure table
                      tags$table(
                        tags$tr(tags$th(""),tags$th("Diseased"),tags$th("Healthy")),
                        tags$tr(
                          tags$th("Exposed"),
                          tags$td(textInputRow(inputId = "disExp", label = NULL, value = "")),
                          tags$td(textInputRow(inputId = "heaExp", label = NULL, value = ""))
                        ),
                        tags$tr(
                          tags$th("Not exposed"),
                          tags$td(textInputRow(inputId = "disNexp", label = NULL, value = "")),
                          tags$td(textInputRow(inputId = "heaNexp", label = NULL, value = ""))
                        )
                      ),
                      
                      
                      br(),
                      # with the input of these four values, we calculate the odds ratio and use that value in the graphs
                      tags$table( tags$tr(tags$th("Odds Ratio:"),tags$th("     "), tags$th("Risk Ratio:")),
                                  tags$tr(tags$td(textOutput("oddsRatio")), tags$td("     "), tags$td(textOutput("riskRatio"))))),
               
               column(6, 
                      br(), br(),
                      p("Finding out how likely one event is relative to another is very useful.
                        However, sometimes we think we know the odds, when in actuality we know
                        the odds ratio."),
                      p("Suppose that you've been exposed to an infectious virus. You may 
                        be wondering, \"What are the odds that I'm going to be okay?\" Being the data-oriented person that you are, 
                        you quickly make a table."),
                      p("From this table, you calculate the odds ratio. 
                        You might assume the odds ratio is telling you the odds of getting the disease after you've been exposed.
                        However, you would be wrong in assuming that. The graph below shows how wrong you might be."))),          
             
             fluidRow(
               column(10, offset = 1,
                      
                      plotOutput("orbypPlot"),
                      
                      p("It's actually the", em("risk ratio"), "that tells you how much more likely you are to get the disease after exposure. 
                        This value takes into account the baseline probability, or the likelihood that you would end up diseased had you not been exposed."),    
                      p("If there isn't much likelihood of you getting this disease without exposure to the virus, 
                        or the odds ratio itself is small (exposure doesn't increase your risk much), the odds ratio and risk ratio are similar numbers.
                        We can see this by looking at the", tags$b("bias"), "in the odds ratio, 
                        defined as the proportion by which the odds ratio exceeds the risk ratio: 
                        $$\\frac{Odds  Ratio}{Risk  Ratio} - 1$$"),
                      p("As the probability of getting the disease increases, or the size of the odds ratio increases, 
                        the bias becomes problematic."),
                      p("These pages show several ways of exploring the differences between the odds ratio and the risk ratio.")
                      ))),
    
    # second tab
    # Effect of the Baseline
    # compute vertical line at value on slider
    # incidence rate vs. missing values
    tabPanel("Start with the Odds Ratio",
             fluidRow(
               column(6, offset = 3, align = "center",
                      h3("Risk Ratio for fixed Odds Ratio")
               )
             ),
             
             fluidRow(
               column(10, offset = 1,
                      
                      plotOutput("fixedorPlot"))),
             
             fluidRow(
               column(4, offset = 1,
                      sliderInput("OR", "Odds ratio to consider", min = 1, max = 10, round=F,
                                  step = .05, value = 2),
                      br(),
                      checkboxInput("biasyn","Include a bias marker?", value = FALSE),
                      conditionalPanel( condition = "input.biasyn ==true",
                                        sliderInput("bias", "Acceptable bias when treating the OR as an estimate of RR", 
                                                    min = 0, max = 1, round=F, step = .05, value = .1), br()  )),
               column(6,
                      p("This is the risk ratio associated with the chosen odds ratio, as the baseline 
                        probability changes. All points on the curve have the odds ratio that you chose."),
                      p("As the baseline probability (P1) changes, the exposed probability changes to maintain a fixed odds ratio, 
                        and the risk ratio changes as well. The risk ratio is similar to the odds ratio for 'small' baseline
                        probabilities, but approaches 1 as the baseline probability increases."),  
                      p("For reference, we show the exposed probability (P2) at either end of the spectrum."), 
                      p("The red dot shows the baseline probability where the odds ratio has your 'acceptable' bias (chosen from the slider) 
                        relative to the risk ratio. Bias is defined as the proportion by which the odds ratio exceeds the risk ratio: $$\\frac{OR}{RR} - 1$$"), 
                      p("Higher baseline probabilities have smaller risk ratios and thus more bias than is acceptable; 
                        lower baseline probabilities have larger risk ratios and an acceptable amount of bias.")
                      
                      ))),
    
    
    
    # initial name: "Probabilities"
    # Bias in the Odds Ratio
    tabPanel("Start with the Probabilities", 
             fluidRow(
               column(6, offset = 3, align = "center",                
                      h3("Odds Ratio and Risk Ratio for Given Probabilities"))),
             
             fluidRow(
               column(10, offset = 1,
                      
                      plotOutput("proborPlot"))),
             
             fluidRow(
               column(4, offset = 1,
                      sliderInput("bp", "Baseline probability", min = 0, max = 1, round=F,
                                  step = .005, value = .2) ,
                      uiOutput("inprob") ),
               column(6,
                      
                      
                      p("Here we plot the risk ratio by the baseline probability.
                        The figure title shows the risk ratio and odds ratio associated with the chosen probabilities.
                        The red dot appears above your input baseline probability;  
                        the label shows the bias in the OR as an indicator of RR for your probabilities.
                        The line shows the RR for other baseline probabilities with this OR.
                        As the baseline probability (p1) changes,
                        the exposed probability must change to maintain a fixed OR, and the RR changes as well.
                        You can use the plot to assess the effects on the OR/RR relationship as the baseline probability changes.
                        The risk ratio is similar to the odds ratio for 'small' baseline
                        probabilities, but approaches 1 as the baseline probability increases.
                        This is why it is crucial to know the baseline probability before attempting to interpret the OR."),                  
                      p("Note: plots may look funny for baseline probabilities > 0.6, or for very small RRs.")
                      
                      
                      ))),
    
    
    
    # initial name: "Equal RR and OR"  
    tabPanel("Hold Odds Ratio Constant", 
             
             fluidRow(
               column(6, offset = 3, align = "center",                
                      h3("Risk Ratio and Odds Ratio for Baseline Probability"))),
             
             fluidRow(
               column(10, offset = 1,
                      # compute vertical line at value on slider - add markers for points on two lines                          
                      plotOutput("orvsrrPlot"))),
             
             fluidRow(
               column(4, offset = 1,
                      sliderInput("ratio", "Ratio to consider", min = 1, max = 10, round=F,
                                  step = .05, value = 2)),  
               column(6,                 
                      
                      p("One way to think about the difference between the risk ratio and the odds ratio is to ask how high the 
                        exposed probability would have to be, in order to get the risk ratio to be as large as the odds ratio.
                        In this plot, we show the values of the baseline and exposed probabilities that have the
                        input ratio-- the odds ratio is in red, and the risk ratio is in blue.  If you look at a
                        baseline probability, the red line shows the exposed probability that generates the odds ratio.
                        To get the risk ratio to have that value, you need the exposed probability to have the value 
                        on the blue line.  Note that for some baseline values it is not possible to achieve the 
                        the RR, since the exposed probability cannot be greater than 1.")
                      
                      
                      ))),
    
    
    # footer stays at bottom of page, but isn't at the bottom of the browser window always... YET
    # to-do: figure out how to do this WITHOUT absolute positioning
    
    tags$footer(br(),tags$small("Made by",  a(href="mailto:ken dot Kleinman at gmail dot com", target="_blank", "Ken Kleinman"), 
                                " and Katie Leap.  This app generated using", a(href="http://www.rstudio.com/shiny/", target="_blank", "Shiny"),
                                "software, and hosted by the generous folks at", 
                                a(href="http://www.rstudio.com/", target="_blank", "RStudio.")))
    )))
