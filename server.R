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

shinyServer(function(input, output, session){
  
  # function that allows the exposed probability to be input through a slider
  # slider one
  output$inprob = renderUI ({
    sliderInput("ep", 
                "Exposed probability", 
                min = input$bp, 
                max = .995, 
                round=F,
                step = .005,
                value = ifelse(input$bp < .6, .6,.995) 
    ) 
  })
  
  #function that outputs a plot of "orbyp"... odds ratio by probability? 
  # plot 1
  output$orbypPlot <- renderPlot({
    # take reactive input (and make sure it's numeric)
    orbyp = function(addp=T, disNexp=(as.numeric(input$disNexp)), heaNexp=(as.numeric(input$heaNexp)), 
                     disExp=(as.numeric(input$disExp)), heaExp=(as.numeric(input$heaExp))){
      # convert reactive input into baseline probability and exposed probability
      bp=disNexp/(heaNexp+disNexp)
      ep=disExp/(heaExp+disExp)
      OR = seq(1.1,9.2,.1)
      # no 0 for p_base
      p_base = c(.001,seq(.1,.9,.01))
      odds_base = p_base/(1-p_base)
      # calculate combinations of two parameters -> MATRIX
      eg = expand.grid(odds_base,OR)
      # expand.grid makes Var1 and Var2, product makes odds of disease (case)
      odds_case = eg$Var1 * eg$Var2
      # p_case a.k.a. exposed probability
      p_case = odds_case/(1+odds_case)
      # RR to calculate bias, not shown
      RR = p_case/p_base
      bias = ((   OR /   matrix(RR, ncol =82, byrow=T)   ) ) * 100
      
      ### EG RASTER TRY TWO ###
      eg.raster <- expand.grid(odds_base,p_base)
      eg2 <- cbind(eg.raster,bias=matrix(bias, ncol=1))
      
      ## GGPLOT ##
      ggplot(eg2, aes(y=as.factor(eg2$Var1),x=eg2$Var2)) + geom_tile(aes(fill= eg2$bias)) + 
        scale_fill_gradient(low="green",high="red")
      
      #  contour(p_base, OR, t(bias), levels=c(110, 125,150), drawlabels=F, 
      #          col = c("green","orange","red"), 
      #          xlab="Baseline Probability", ylab="Odds Ratio", ylim = c(1,8))
      #  text(x=0.075,y=1.5,"Bias < 10%")
      #  text(x=.135,y = 2.4,"10% < Bias < 25%", srt=320)
      #  text(x=.28,y = 2.4,"25% < Bias < 50%", srt=340)
      #  text(x=.5,y = 4,"Bias > 50%")
      #  # abline(h=1)
      #  # abline(v=0)
      if (addp==T) points(bp, (ep/(1-ep))/(bp/(1-bp)),pch=19,col="dark blue" )
    }
    orbyp()
  })
  
  
  # function to output a plot of a fixed odds ratio
  # plot 2
  output$fixedorPlot <- renderPlot({
    fixedor = function(ORin,biasyn,accbias) {
      p1 = seq(.001, .999, .001)
      p2 = seq(.001, .999, .001)
      p1p2 = expand.grid(p1=p1,p2=p2)
      p1p2["OR"] = with(p1p2,(p2/(1-p2))/(p1/(1-p1)))
      p1p2["RR"] = with(p1p2,p2/p1)
      p1p2["bias"] = p1p2$OR/p1p2$RR -1
      
      temp = p1p2[abs(p1p2$OR - ORin)  < .005,]
      
      minrow = temp[which.min(temp$p1),]
      minlabel = paste("P2 =", minrow$p2)
      maxrow = temp[which.max(temp$p1),]
      maxlabel = paste("P2 =", maxrow$p2)
      
      minaccbias = temp[which.max(temp$bias[temp$bias < accbias]),] 
      
      datawidth= ORin -1
      
      with(temp,plot(supsmu(y=RR, x=p1), type = "l",ylab= "Risk Ratio", 
                     xlab="Baseline Probability"
                     ,main = paste("Risk Ratio by Baseline (non-exposed) Probability when the Odds Ratio =", ORin)
                     ,ylim=c(1,ceiling(ORin)), xlim=c(0,1.13)
                     ,xaxp=c(0,1,5), yaxp = c(1,ceiling(ORin),5)))
      if (biasyn ==T) {
        if (dim(minaccbias)[1] > 0) {
          points(x=minaccbias$p1, minaccbias$RR, pch=21, col = "darkred", bg = "tomato")
          text(paste0( "P1 = ",substr(minaccbias$p1,1,5),
                       ", P2 = ", substr(minaccbias$p2,1,5),                     
                       ", RR = ", substr(minaccbias$RR,1,4)),
               x=minaccbias$p1, y= minaccbias$RR, pos=4)
          # arrows(x1 = minaccbias$p1+ .01, x0 =minaccbias$p1 + 
          # .033, y0 = minaccbias$RR, length = .05)
        }
        else text("No baseline P found with bias like that", 
                  x = .5, y = ((1+max(temp$RR))/2))     
      }      
      text(minlabel,x=minrow$p1,y=minrow$RR,adj = -.3)
      points(x = minrow$p1, y = minrow$RR)
      text(maxlabel,x=maxrow$p1,y=maxrow$RR,adj = c(0, -1))
      points(x = maxrow$p1, y = maxrow$RR)
    }
    fixedor(input$OR,input$biasyn,input$bias)
  })
  
  
  # function that outputs a plot of the probability and the odds ratio
  # plot 3
  output$proborPlot  = renderPlot({
    orandrr = function(bp,ep){
      myOR = (ep/(1-ep))/(bp/(1-bp))
      myRR = ep/bp
      
      p1 = seq(.001, .999, .001)
      p2 = seq(.001, .999, .001)
      p1p2 = expand.grid(p1=p1,p2=p2)
      p1p2["OR"] = with(p1p2,(p2/(1-p2))/(p1/(1-p1)))
      p1p2["RR"] = with(p1p2,p2/p1)
      p1p2["bias"] = p1p2$OR/p1p2$RR -1
      
      
      temp = p1p2[abs(p1p2$OR - myOR)  < .005,]
      
      with(temp,plot(supsmu(y=RR, x=p1), type = "l",ylab= "Risk Ratio", 
                     xlab="Baseline probability"
                     ,main = 
                       paste0("Exposed/baseline = ", ep,"/", bp, "; so RR=", round(myRR,3), "; OR=", round(myOR,3) )
                     , xlim=c(0,1.03), ylim=c(1- (myOR-1)/20,myOR)+ (myOR -1)/20, 
                     xaxp=c(0,1,5), yaxp = c(1,ceiling(myOR),5)))
      points(x=temp$p1[which.min(abs(myRR-temp$RR))], y=myRR, pch=20, col = "red")
      text(paste0("Bias (OR/RR -1) = ", 100*round(   ((myOR /myRR)-1)   ,3),"%"), 
           y=myRR,x=bp, pos=4)
    }  
    
    orandrr(input$bp,input$ep)
  })
  
  # function that outputs a plot of the odds ratio versus the risk ratio
  # plot 4
  output$orvsrrPlot <- renderPlot({
    orvsrr = function(ratio) {
      p1 = seq(.002, .998, .002)
      p2 = seq(.002, .998, .002)
      p1p2 = expand.grid(p1=p1,p2=p2)
      p1p2["OR"] = with(p1p2,(p2/(1-p2))/(p1/(1-p1)))
      p1p2["RR"] = with(p1p2,p2/p1)
      
      if (length(ratio) == 1 ) {
        odds = contourLines(p2,p1,matrix(p1p2$OR,499,499), levels = ratio)
        plot(supsmu(y = odds[[1]]$y, x = odds[[1]]$x), col = "red", type="l",
             xlab="Baseline Probability", ylab = "Exposed probability", 
             xlim = c(0,1), ylim = c(0,1),
             main = paste("Baseline and Exposed probability with constant OR and RR =",
                          round(ratio,3)))
        risk = contourLines(p2,p1,matrix(p1p2$RR,499,499), levels = ratio)
        lines(supsmu(y = risk[[1]]$y, x = risk[[1]]$x), col = "blue")
        legend(.8,0.3,legend=c("Odds ratio","Risk ratio"), lty = c(1,1), 
               col=c("red","blue"))
      }
      
    }
    
    orvsrr(input$ratio)
  })
  
  
  # function to calculate the odds ratio from four values: disExp, heaNexp, heaExp, and disNexp
  output$oddsRatio <- renderText({
    (as.numeric(input$disExp)*as.numeric(input$heaNexp))/(as.numeric(input$heaExp)*as.numeric(input$disNexp))
  })
  
  output$riskRatio <- renderText({
    (as.numeric(input$disExp)/(as.numeric(input$heaExp)+as.numeric(input$disExp)))/(as.numeric(input$disNexp)/(as.numeric(input$heaNexp)+as.numeric(input$disNexp)))
  })
  
})