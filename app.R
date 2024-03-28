source("functions.R")

# define UI
ui <- navbarPage(
  
  # title
  p(
    style = "color: white;", # I bet this line could be removed!
    strong("Building Intuition for Interaction Terms")
  ),
  
  # adding TeX capability, customizing colors, and creating CSS classes
  tags$head(
    tags$link(rel="stylesheet", 
              href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", 
              crossorigin="anonymous"),
    tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", 
                crossorigin="anonymous"),
    HTML(
      "
    <style>
    .navbar {background-color: #1d6e8b;}
    .navbar-default .navbar-nav > li > a {color: white;}
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover { background-color: #Bbbbf6; color: black;}
    .navbar-default .navbar-nav > li > a:hover {color: gray;}
    
    .rounded-box-solid {
          display: inline-block;
          border-radius: 10px;
          border: 2px solid #003865;
          padding: 8px;
    }
    
    .rounded-box-dashed {
          display: inline-block;
          border-radius: 10px;
          border: 2px dashed #003865;
          padding: 4px;
    }
    

    .custom-column p {
          max-width: 80%;
          justify-content: center;
          align-items: center;
    }
    
    </style>
    "
    )
  ),
  
  # opening tab
  tabPanel("Overview",
           
           verticalLayout(h3(strong("Welcome to Building Intuition for Interaction Terms!")),
                          
                          p("This activity will help you learn about",
                            strong("interaction terms"),
                            "in linear models. In the process, we'll also talk about centering predictors. The activity has two parts -- start in the",
                            strong("Part 1"),
                            "tab. As you go through the activity, discuss your thoughts and answers with your classmates nearby."))
           
        # leaving this just in case I want to revert the layout
           # fluidRow(
           #   column(width = 7,
           #          h3(strong("Welcome to Building Intuition for Interaction Terms!")),
           #          
           #          p("This activity is meant to help you learn about",
           #            strong("interaction terms"),
           #            "in linear models. In the process, we'll also talk about centering predictors. There are two parts to this activity -- start in the",
           #            strong("Part 1"),
           #            "tab. As you go through the activity, be sure to discuss your thoughts and answers with your classmates nearby."),
           #   ),
           #   
           #   column(width = 5, class = "custom-column",
           #          div(class = "rounded-box-dashed",
           #              
           #              p(strong("About:"),
           #                " Placeholder.")
           #          )
           #   )
           # )
  ),
  
  # tab for part 1
  tabPanel("Part 1",
           
           # enable showing/hiding of elements
           useShinyjs(),
           
           verticalLayout(
             div(class = "rounded-box-solid", # put background inside a box
                 p("The Lung Health Study (LHS) was a multicenter randomized clinical trial in the 1980s and 1990s, investigating whether smoking intervention and use of an inhaled bronchodilator (Atrovent) would decrease the rate of decline in lung function over the 5-year follow-up period. A total of 5,887 participants (smokers aged 35-59 years old) were recruited from 10 clinical centers in the US and Canada from 1986 to 1988. They were randomized into three groups for treatment and followed for several years. However, for this activity, we are looking exclusively at baseline measurements and demographics. The full, de-identified dataset can be downloaded in the Appendix tab."),
                 
                 p("We will examine the relationship between body mass index (BMI) and lung function, as measured by the ratio of FEV1 (forced expiratory volume in 1 second) to FVC (forced vital capacity)."),
             ),
             
             br(), # these are line breaks!
             
             p(strong("Question 1:"),
               "To examine the distribution of our lung function measure, a _____ should be used. To visualize the relationship between BMI and lung function, we can use a _____."),
             
             # putting multiple elements in the same row
             splitLayout(cellWidths = rep("25%", 4),
                         radioButtons("p1q1_1", label = "First blank:",
                                      choices = list("histogram",
                                                     "scatterplot",
                                                     "bar chart")),
                         radioButtons("p1q1_2", label = "Second blank:",
                                      choices = list("histogram",
                                                     "scatterplot",
                                                     "bar chart")),
                         actionButton("p1q1_3", label = "Submit"),
                         shinyjs::hidden(htmlOutput("p1q1_correct"))),
             
             br(),
             
             # text to appear till they answer the question
             p(id = "eda_placeholder",
               em("EDA plots will appear here after you submit the correct answer to Question 1.")),
             
             # hidden elements don't appear till a question is answered
             shinyjs::hidden(plotOutput("hist_bmi")),
             br(),
             shinyjs::hidden(plotOutput("hist_fev")),
             br(),
             shinyjs::hidden(plotOutput("scatter_eda")),
             br(),
             
             p(strong("Question 2:"),
               "What can we learn from these plots?"),
             
             textAreaInput("p1q2", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%"),
             
             br(),
             
             p("So, now we've seen the overall relationship between BMI and lung function. But maybe there's more to the story. Does sex affect the relationship between these variables? Here's a scatterplot to help investigate this question:"),
             
             p(id = "sex_placeholder",
               em("This plot will appear after you answer Question 1.")),
             
             shinyjs::hidden(plotOutput("sexplot")),
             
             br(),
             
             p(strong("Question 3:"),
               "Describe in words how the relationship between BMI and lung function differs for men and women, based on this scatterplot."),
             
             textAreaInput("p1q3", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%"),
             
             br(),
             
             p(strong("Question 4:"),
               "Now, let's fit a linear regression model with main effects for BMI and sex, and an interaction between the two. Which of the following equations represents our regression model?"),
             
             # a bit complicated because it uses TeX...
             # (there might be an easier way using withMathJax())
             selectizeInput("p1q4", label = NULL,
                            width = "55%",
                            choices = list("Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\varepsilon_i" = 1,
                                           "Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\beta_3 X_{3i} + \\varepsilon_i" = 2,
                                           "Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\beta_3 X_{3i} + \\beta_4 X_{1i} X_{2i} + \\varepsilon_i" = 3,
                                           "Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\beta_3 X_{1i} X_{2i} + \\varepsilon_i" = 4),
                            options = list(render = I("
      {
        item: function(item, escape) { 
                var html = katex.renderToString(item.label);
                return '<div>' + html + '</div>'; 
              },
        option: function(item, escape) { 
                  var html = katex.renderToString(item.label);
                  return '<div>' + html + '</div>'; 
                }
      }")
                            )
             ),
             
             splitLayout(cellWidths = c("20%", "40%"), # turns out these don't need to sum to 100
                         actionButton("p1q4_go",
                                      label = "Submit"),
                         shinyjs::hidden(htmlOutput("p1q4_correct"))),
             
             br(),
             br(),
             
             p("Here's the output after fitting this model in R:"),
             
             p(id = "model_placeholder",
               em("This plot will appear after you answer Question 4.")),
             
             shinyjs::hidden(verbatimTextOutput("model")),
             
             br(),
             
             p(strong("Question 5:"),
               "Interpret each estimated coefficient from the above output, including the model intercept."),
             
             textAreaInput("p1q5", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%"),
             
             br(),
             
             p("Notice that the interpretation of the intercept is meaningless right now, since a BMI of 0 is impossible! We can fix that by centering our data. To do this, we subtract some amount, c, from each subject's measurement of BMI."),
             
             p("The plot below shows estimates for the coefficients in the non-centered model, with a 95% confidence interval for each. The",
               strong("slider"),
               "lets you select the value of c; in other words, the slider controls how much is subtracted from BMI."),
             
             br(),
             
             p(strong("Question 6:"),
               "Shift the slider around and see how the plot changes. What value results in the smallest confidence intervals (CIs)? What values lead to the largest CIs? Why is this the case?"),
             
             sliderInput("p1q6", label = NULL,
                         min = 0,
                         max = 40,
                         value = 0),
             
             p(id = "beta_placeholder",
               em("This plot will appear after you answer Question 4.")),
             
             shinyjs::hidden(plotOutput("beta_centering")),
             
             textAreaInput("p1q6_text", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%")
           )
  ),
  
  # tab for part 2
  tabPanel("Part 2",
           verticalLayout(
             div(class = "rounded-box-solid",
                 p("In this part of the activity, we'll look at data from schools in Portugal. We have a dataset of 395 students at two different secondary schools in Portugal, including several potential predictors as well as our variable of interest: their final math score at the end of the academic year."),
                 
                 p("We want to evaluate the effect of age and urban/rural status on final math score. (Here, urban/rural refers to where a student lives, not where their school is located.) We also want to determine whether urban/rural status modifies the effect of age on final math score."),
             ),
             
             br(),
             
             p("First, some EDA:"),
             
             br(),
             
             plotOutput("hist_age"),
             
             br(),
             
             plotOutput("box_address"),
             
             br(),
             
             plotOutput("p2_scatter"),
             
             br(),
             
             p(strong("Question 1:"),
               "What can we learn from these plots?"),
             
             textAreaInput("p2q1", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%"),
             
             br(),
             
             p("Here's the equation for the interaction model we want to fit:"),
             
             withMathJax("$$Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\beta_3 X_{1i} X_{2i} + \\varepsilon_i$$"),
             
             p(strong("Question 2:"),
               "Using the model equation (and your intuition), find values for the intercept and coefficients to match the lines of best fit we already plotted. The plot below (on the right) will change to reflect the values you choose. (You can use decimals!)"),
             
             splitLayout(cellWidths = rep("25%", 4),
                         numericInput("p2q2_beta0", label = withMathJax("$$\\beta_0$$"),
                                      value = 0,
                                      min = -20,
                                      max = 20),
                         numericInput("p2q2_beta1", label = withMathJax("$$\\beta_1$$"),
                                      value = 0,
                                      min = -20,
                                      max = 20),
                         numericInput("p2q2_beta2", label = withMathJax("$$\\beta_2$$"),
                                      value = 0,
                                      min = -20,
                                      max = 20),
                         numericInput("p2q2_beta3", label = withMathJax("$$\\beta_3$$"),
                                      value = 0,
                                      min = -20,
                                      max = 20)),
             
             splitLayout(cellWidths = rep("25%", 4),
                         htmlOutput("beta0_correct"),
                         htmlOutput("beta1_correct"),
                         htmlOutput("beta2_correct"),
                         htmlOutput("beta3_correct")),
             
             br(),
             
             splitLayout(cellWidths = c("55%", "45%"),
                         plotOutput("p2_scatter_alt"),
                         plotOutput("beta_guesses")),
             
             br(),
             
             p("After finding the right values, here's the output after fitting this model in R:"),
             
             verbatimTextOutput("model_pt2"),
             
             br(),
             
             p(strong("Question 3:"),
               "Now consider the interpretations of our model coefficients. Select which term(s) have interpretation(s) subject to each limitation."),
             
             withMathJax(),
             splitLayout(cellWidths = c("33%", "33%", "34%"),
                         checkboxGroupInput("p2q3_rural",
                                            label = "Applies only to rural students:",
                                            choices = c("$$\\beta_0$$" = "beta_0",
                                                        "$$\\beta_1$$" = "beta_1",
                                                        "$$\\beta_2$$" = "beta_2",
                                                        "$$\\beta_3$$" = "beta_3")),
                         
                         checkboxGroupInput("p2q3_urban",
                                            label = "Applies only to urban students:",
                                            choices = c("$$\\beta_0$$" = "beta_0",
                                                        "$$\\beta_1$$" = "beta_1",
                                                        "$$\\beta_2$$" = "beta_2",
                                                        "$$\\beta_3$$" = "beta_3")),
                         
                         checkboxGroupInput("p2q3_age0",
                                            label = "Applies only to students of age 0:",
                                            choices = c("$$\\beta_0$$" = "beta_0",
                                                        "$$\\beta_1$$" = "beta_1",
                                                        "$$\\beta_2$$" = "beta_2",
                                                        "$$\\beta_3$$" = "beta_3"))),
             
             splitLayout(cellWidths = c("33%", "33%", "34%"),
                         actionButton("p2q3_rural_submit",
                                      label = "Submit"),
                         actionButton("p2q3_urban_submit",
                                      label = "Submit"),
                         actionButton("p2q3_age0_submit",
                                      label = "Submit")),
             splitLayout(cellWidths = c("33%", "33%", "34%"),
                         shinyjs::hidden(htmlOutput("p2q3_rural_correct")),
                         shinyjs::hidden(htmlOutput("p2q3_urban_correct")),
                         shinyjs::hidden(htmlOutput("p2q3_age0_correct"))),
             
             br(),
             
             p(strong("Question 4:"),
               "Finally, suppose we centered our age predictor at the mean age of our dataset. Which coefficient estimate(s) would change because of this?"),
             
             
             checkboxGroupInput("p2q4",
                                label = NULL,
                                choices = c("$$\\beta_0$$" = "beta_0",
                                            "$$\\beta_1$$" = "beta_1",
                                            "$$\\beta_2$$" = "beta_2",
                                            "$$\\beta_3$$" = "beta_3")),
             actionButton("p2q4_submit",
                          label = "Submit"),
             
             shinyjs::hidden(htmlOutput("p2q4_correct")),
             
             br(),
           )
  ),
  tabPanel("Appendix",
           h2("References"),
           
           p('Cortez, P., and Silva, A. (2008), "Using Data Mining to Predict Secondary School Student Performance," in ',
             em("Proceedings of 5th Annual Future Business Technology Conference, Porto, 2008",
                .noWS = "after"),
             ", ed. A. Brito and J. Teixeira, pp. 5-12. Ostend, Belgium: EUROSIS. Available at ",
             a(href = "https://hdl.handle.net/1822/8024", "https://hdl.handle.net/1822/8024",
               .noWS = "after"),
             "."),
           
           p("Anthonisen, N. R., Connett, J. E., Kiley, J. P., Altose, M. D., Bailey, W. C., Buist, A. S., Conway, W. A., Enright, P. L., Kanner, R. E., O'Hara, P., Owens, G. R., Scanlon, P. D., Tashkin, D. P., and Wise, R. A. (1994),",
             '"Effects of Smoking Intervention and the Use of an Inhaled Anticholinergic Bronchodilator on the Rate of Decline of FEV1", ',
             em("JAMA",
                .noWS = "after"),
             ", 272(19), 1497--1505."),
           
           h2("Technical Details"),
           
           p("This activity is a Shiny app created using R. Source code is available on GitHub (link forthcoming)."),
           
           h2("Acknowledgements"),
           
           p("This will be the last thing I add to the app!"),
           
           h2("TO-DO"),
           
           p("Potentially some rewording. Add color to the feedback that pops up in Part 1. Add dataset downloads here in the appendix. Standardize code (there's a lot of it!). If time, try to build an 'export' option for students to export their correct answers. Make front page less blank (add images?). Add copious comments to code."))
)


 
# define server
server <- function(input, output, session) {
  output$p1q1_correct <- renderText({
    if(input$p1q1_1 == "histogram" & input$p1q1_2 == "scatterplot") {
      "<b>Correct!</b> <br> (Click the button again to see the plots.)"
    } else {
      "<b>Not quite -- try again.</b>"
    }
  })
  
  output$p1q4_correct <- renderText({
    if(input$p1q4 == 4) {
      "<b>Correct!</b> <br> (Click the button again to see the model output.)"
    } else {
      "<b>Not quite -- try again.</b>"
    }
  })
  
  output$hist_bmi <- renderPlot({
    ggplot(data = lungs) +
      geom_histogram(aes(x = bmi), bins = 30) +
      labs(x = "BMI",
           y = "Count",
           title = "Distribution of BMI") +
      theme_minimal()
  })
  
  output$hist_fev <- renderPlot({
    ggplot(data = lungs) +
      geom_histogram(aes(x = FEVFVC02), bins = 30) +
      labs(x = "FEV1/FVC % at baseline",
           y = "Count",
           title = "Distribution of lung function at baseline",
           subtitle = "      (Higher values are better!)") +
      theme_minimal()
  })
  
  output$scatter_eda <- renderPlot({
    ggplot(data = lungs) +
      geom_point(aes(x = bmi, y = FEVFVC02)) +
      geom_smooth(aes(x = bmi, y = FEVFVC02),
                  se = FALSE,
                  method = "lm") +
      labs(x = "BMI",
           y = "FEV1/FVC % at baseline",
           title = "Relationship between BMI and lung function") +
      theme_minimal()
  })
  
  output$sexplot <- renderPlot({
    ggplot(data = lungs) +
      geom_point(aes(x = bmi, y = FEVFVC02),
                 alpha = .3) +
      geom_smooth(aes(x = bmi, y = FEVFVC02, color = sex),
                  se = FALSE,
                  method = "lm") +
      labs(x = "BMI",
           y = "FEV1/FVC % at baseline",
           color = "Sex") +
      theme_minimal()
  })
  
  output$model <- renderPrint({
    summary(lm(FEVFVC02 ~ bmi + sex + bmi:sex,
               data = lungs))
  })
  
  output$beta_centering <- renderPlot({
    beta_plot(input$p1q6)
  })
  
  output$hist_age <- renderPlot({
    ggplot(data = math) +
      geom_histogram(aes(x = age), bins = 8) +
      labs(x = "Age",
           y = "Count",
           title = "Distribution of age") +
      theme_minimal()
  })
  
  output$box_address <- renderPlot({
    ggplot(data = math) +
      geom_boxplot(aes(x = address,
                       y = G3),
                   fill = "#Bbbbf6") +
      labs(x = "Urban/rural status",
           y = "Final math score",
           title = "Distribution of final math score by urban/rural status") +
      theme_minimal()
  })
  
  output$p2_scatter_alt <- output$p2_scatter <- renderPlot({
    set.seed(20020521)
    ggplot(data = math) +
      geom_jitter(aes(x = age,
                      y = G3),
                  width = .5,
                  height = .5) +
      geom_smooth(aes(x = age,
                      y = G3,
                      color = address),
                  method = "lm",
                  se = FALSE) +
      labs(x = "Age",
           y = "Final math score",
           color = "Urban/rural",
           title = "Lines of best fit for rural and urban students") +
      scale_color_manual(values = c("Rural" = "red3",
                                    "Urban" = "blue3")) +
      theme_minimal()
  })
  
  output$beta_guesses <- renderPlot({
    set.seed(20020521)
    ggplot(data = math) +
      geom_jitter(aes(x = age,
                      y = G3),
                  width = .5,
                  height = .5) +
      geom_abline(intercept = input$p2q2_beta0,
                  slope = input$p2q2_beta2,
                  color = "red3",
                  linewidth = 1) + # match line width to geom_smooth() width
      geom_abline(intercept = input$p2q2_beta0 + input$p2q2_beta1,
                  slope = input$p2q2_beta2 + input$p2q2_beta3,
                  color = "blue3",
                  linewidth = 1) +
      labs(x = "Age",
           y = "Final math score",
           title = "Lines created with your 'guesses' for coefficients") +
      theme_minimal()
  })
  
  observeEvent(input$p1q1_3, {
    shinyjs::show(id = "p1q1_correct")
    
    if(input$p1q1_1 == "histogram" & input$p1q1_2 == "scatterplot") {
      shinyjs::hide(id = "eda_placeholder")
      shinyjs::hide(id = "sex_placeholder")
      shinyjs::show(id = "hist_bmi")
      shinyjs::show(id = "hist_fev")
      shinyjs::show(id = "scatter_eda")
      shinyjs::show(id = "sexplot")
    }
  })
  
  observeEvent(input$p1q4_go, {
    shinyjs::show(id = "p1q4_correct")
    
    if(input$p1q4 == 4) {
      shinyjs::hide(id = "model_placeholder")
      shinyjs::hide(id = "beta_placeholder")
      shinyjs::show(id = "model")
      shinyjs::show(id = "beta_centering")
    }
  })
  
  output$beta0_correct <- renderText({
    if(input$p2q2_beta0 == 0 | is.na(input$p2q2_beta0)) {
      ""
    } else if(input$p2q2_beta0 >= 14.5 & input$p2q2_beta0 <= 15.5) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Keep trying!</p>'
    }
  })
  
  output$beta1_correct <- renderText({
    if(input$p2q2_beta1 == 0 | is.na(input$p2q2_beta1)) {
      ""
    } else if(input$p2q2_beta1 >= 5.2 & input$p2q2_beta1 <= 6.2) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Keep trying!</p>'
    }
  })
  
  output$beta2_correct <- renderText({
    if(input$p2q2_beta2 == 0 | is.na(input$p2q2_beta2)) {
      ""
    } else if(input$p2q2_beta2 >= -.37 & input$p2q2_beta2 <= -.27) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Keep trying!</p>'
    }
  })
  
  output$beta3_correct <- renderText({
    if(input$p2q2_beta3 == 0 | is.na(input$p2q2_beta3)) {
      ""
    } else if(input$p2q2_beta3 >= -.33 & input$p2q2_beta3 <= -.23) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Keep trying!</p>'
    }
  })
  
  output$model_pt2 <- renderPrint({
    if(input$p2q2_beta0 >= 14.5 & input$p2q2_beta0 <= 15.5 &
       input$p2q2_beta1 >= 5.2 & input$p2q2_beta1 <= 6.2 &
       input$p2q2_beta2 >= -.37 & input$p2q2_beta2 <= -.27 &
       input$p2q2_beta3 >= -.33 & input$p2q2_beta3 <= -.23) {
      summary(lm(G3 ~ address + age + address:age,
                 data = math))
    } else {
      "Complete Question 2 to make this model output appear."
    }
  })
  
  output$p2q3_rural_correct <- renderText({
    if(is.null(input$p2q3_rural)) {
      ""
    } else if("beta_0" %in% input$p2q3_rural &
              "beta_2" %in% input$p2q3_rural &
              "beta_1" %notin% input$p2q3_rural &
              "beta_3" %notin% input$p2q3_rural) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Try again.</p>'
    }
  })
  
  output$p2q3_urban_correct <- renderText({
    if(is.null(input$p2q3_urban)) {
      ""
    } else if("beta_1" %in% input$p2q3_urban &
              "beta_3" %in% input$p2q3_urban &
              "beta_0" %notin% input$p2q3_urban &
              "beta_2" %notin% input$p2q3_urban) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Try again.</p>'
    }
  })
  
  output$p2q3_age0_correct <- renderText({
    if(is.null(input$p2q3_age0)) {
      ""
    } else if("beta_0" %in% input$p2q3_age0 &
              "beta_1" %in% input$p2q3_age0 &
              "beta_2" %notin% input$p2q3_age0 &
              "beta_3" %notin% input$p2q3_age0) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Try again.</p>'
    }
  })
  
  observeEvent(input$p2q3_rural_submit, {
    shinyjs::show(id = "p2q3_rural_correct")
  })
  observeEvent(input$p2q3_urban_submit, {
    shinyjs::show(id = "p2q3_urban_correct")
  })
  observeEvent(input$p2q3_age0_submit, {
    shinyjs::show(id = "p2q3_age0_correct")
  })
  
  output$p2q4_correct <- renderText({
    if(is.null(input$p2q4)) {
      ""
    } else if("beta_0" %in% input$p2q4 &
              "beta_1" %in% input$p2q4 &
              "beta_2" %notin% input$p2q4 &
              "beta_3" %notin% input$p2q4) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Try again.</p>'
    }
  })
  
  observeEvent(input$p2q4_submit, {
    shinyjs::show(id = "p2q4_correct")
  })
}

# run the application
shinyApp(ui = ui, server = server)
