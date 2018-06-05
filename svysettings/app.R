# Survey settings Shiny app
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Survey settings"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput(inputId = "su1",
                     label = "PSU variable name",
                     value = "",
               placeholder = "Enter the name of the variable that identifies the primary sampling units"),
         HTML("<font size=-1>Primary samping unit, PSU, cluster, variance estimation unit, replicate</font><p>"),
         HTML("<hr width=50%>"),
         
         textInput(inputId = "str1",
                   label = "Strata variable name",
                   value = "",
                   placeholder = "Enter the name of the variable that identifies the strata"),
         HTML("<font size=-1>Strata</font><p>"),
         HTML("<hr width=50%>"),
         
         textInput(inputId = "weight",
                   label = "Weight variable name",
                   value = "",
                   placeholder = "Enter the name of the variable that identifies the weights"),
         HTML("<font size=-1>Weight(s), probability weight(s), final weight(s), expansion weight(s)</font><p>"),
         HTML("<hr width=50%>")
      ),

      # Show a plot of the generated distribution
      mainPanel(
         htmlOutput("PSU"),
         htmlOutput("Strata1"),
         htmlOutput("Weight1"),
         HTML("<hr width=50%>"),
         HTML("Stata syntax (case sensitive!)"),
         htmlOutput("stata"),
         HTML("<hr width=50%>"),
         HTML("R syntax (case sensitive!)"),
         htmlOutput("lumley")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  psu <- reactive({ input$su1 })
  strata1 <- reactive({ input$str1 })
  weight1 <- reactive({ input$weight })
  
  output$PSU <- renderText({
    toprint <- strong("PSU: ")
    if ( is.na(psu()) | (psu()=="") ) toprint <- paste0(toprint,"<font color='#ABB2B9'>None (observations)</font>")
    else  toprint <- paste0(toprint,psu())
    toprint
  })
  
  output$Strata1 <- renderText({
    toprint <- strong("Strata: ")
    if ( is.na(strata1()) | (strata1()=="") ) toprint <- paste0(toprint,"<font color='#ABB2B9'>None (whole data set)</font>")
    else  toprint <- paste0(toprint,strata1())
    toprint
  })
  
  output$Weight1 <- renderText({
    toprint <- strong("Weights: ")
    if ( is.na(weight1()) | (weight1()=="") ) toprint <- paste0(toprint,"<font color='#ABB2B9'>None (unity)</font>")
    else  toprint <- paste0(toprint,weight1())
    toprint
  })
  
  output$stata <- renderText({
    print(psu())
    print(strata1())
    print(weight1())
    svyset <- "svyset"
    if ( !is.na(psu()) & (psu()!="") ) svyset <- paste(svyset,psu())
    if ( !is.na(weight1()) & (weight1()!="") ) svyset <- paste0(svyset," [pw=",weight1(),"]")
    if ( !is.na(strata1()) & (strata1()!="") ) svyset <- paste0(svyset,", strata(",strata1(),")")
    print(svyset)
    paste0("<pre>",svyset,"\nsvy : tabulate x y</pre>")
  })
  
  output$lumley <- renderText({
    print(psu())
    print(strata1())
    print(weight1())
    svyset <- "require(survey)\nmy.svy.design <- svydesign("
    if ( !is.na(psu()) & (psu()!="") ) svyset <- paste(svyset,"id = ~",psu(),", ")
    else svyset <- paste(svyset,"id = ~ 1, ")
    if ( !is.na(weight1()) & (weight1()!="") ) svyset <- paste0(svyset,"weight = ~",weight1(),", ")
    if ( !is.na(strata1()) & (strata1()!="") ) svyset <- paste0(svyset,"strata = ~",strata1(),", ")
    svyset <- paste0(svyset,"data = my.data)\n")
    print(svyset)
    paste0("<pre>",svyset,"svymean(~x,design=my.svy.design)</pre>")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

