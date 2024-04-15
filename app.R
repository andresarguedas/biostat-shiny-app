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
    
    body {
          padding-top: 70px;
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
                            "in linear models. In the process, we'll also talk about centering predictors. The activity has two parts. Click on the",
                            strong("Part 1"),
                            "tab above to get started. Once you complete Part 1, you'll move on to Part 2. Be sure to go through the activity in order -- some questions need to be answered correctly before you can move on to the next one. This activity should be completed in small groups, so discuss your answers with your classmates nearby."),
                          
                          p("Once you complete the activity, you can move on to the",
                            strong("Appendix"),
                            "tab. There, you can download a PDF containing the questions and the answers you wrote throughout the activity. You can also download the datasets if you wish."))
           
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
                 p("The Lung Health Study (LHS) was a multicenter randomized clinical trial in the 1980s and 1990s, investigating whether a smoking intervention and the use of an inhaled bronchodilator (Atrovent) would decrease the rate of decline in lung function over the five-year follow-up period. A total of 5,887 participants (smokers aged 35-59 years old) were recruited from 10 clinical centers in the US and Canada and were randomized into three treatment groups (smoking intervention plus bronchodilator, smoking intervention plus placebo, or no intervention). The full, de-identified dataset can be downloaded in the",
                 strong("Appendix"),
                 "tab."),
                 
                 p("In this activity, we will be looking exclusively at baseline measurements and demographics of the LHS participants. In particular, we will examine the relationship between body mass index (BMI) and lung function, as measured by the ratio of FEV1 (forced expiratory volume in 1 second) to FVC (forced vital capacity). Higher values of FEV1/FVC % indicate better functioning lungs."),
             ),
             
             br(), # these are line breaks!
             
             p(strong("Question 1:"),
               "To examine the distribution of the lung function measure (FEV1/FVC %), a _____ should be used. To visualize the relationship between BMI and lung function, a _____ should be used."),
             
             # putting multiple elements in the same row
             # NOTE: shinyjs::hidden() makes an element hidden until a specific action happens
             splitLayout(cellWidths = rep("25%", 4),
                         radioButtons("p1q1_1", label = "First blank:",
                                      choices = list("histogram",
                                                     "scatterplot",
                                                     "bar chart")),
                         radioButtons("p1q1_2", label = "Second blank:",
                                      choices = list("histogram",
                                                     "scatterplot",
                                                     "bar chart")),
                         actionButton("p1q1_submit", label = "Submit"),
                         shinyjs::hidden(htmlOutput("p1q1_correct"))),
             
             br(),
             
             # text to appear till they answer the question
             p(id = "eda_placeholder",
               em("EDA plots will appear here after you submit the correct answer to Question 1.")),
             
             # hidden elements don't appear till a question is answered
             shinyjs::hidden(plotOutput("hist_bmi")),
             br(),
             shinyjs::hidden(plotOutput("box_fev")),
             br(),
             shinyjs::hidden(plotOutput("scatter_eda")),
             br(),
             
             p(strong("Question 2:"),
               "What can we learn from these plots?"),
             
             textAreaInput("p1q2", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%"), # not too narrow, not too wide
             
             br(),
             
             p("So, now we've examined the overall relationship between BMI and lung function. But maybe there's more to the story. In particular, does biological sex affect the relationship between these variables? Here's the scatterplot again, but with separate lines of best fit for males and females, to help investigate this question:"),
             
             p(id = "sex_placeholder",
               em("This plot will appear after you answer Question 1.")),
             
             shinyjs::hidden(plotOutput("sexplot")),
             
             br(),
             
             p(strong("Question 3:"),
               "Based on this scatterplot, describe in words how the relationship between BMI and lung function differs for males and females."),
             
             textAreaInput("p1q3", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%"),
             
             br(),
             
             p("Now, let's fit a linear regression model with main effects for BMI and sex, and an interaction between the two."),
             
             br(),
             
             p(strong("Question 4:"),
               "Which of the following equations represents this interaction model?"),
             
             # a bit complicated because it uses TeX...
             # (there might be an easier way using withMathJax(), but I already did this)
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
             
             splitLayout(cellWidths = c("20%", "40%"), # turns out these don't need to sum to 100!
                         actionButton("p1q4_submit",
                                      label = "Submit"),
                         shinyjs::hidden(htmlOutput("p1q4_correct"))),
             
             br(),
             br(),
             
             p("Here's the output after fitting this model in R:"),
             
             p(id = "model_placeholder",
               em("This plot will appear after you answer Question 4.")),
             
             shinyjs::hidden(verbatimTextOutput("model_pt1")),
             
             br(),
             
             p(strong("Question 5:"),
               "Interpret each estimated coefficient from the above output, including the model intercept."),
             
             textAreaInput("p1q5", label = NULL,
                           placeholder = "Write your insights...",
                           width = "55%"),
             
             br(),
             
             p(strong("Question 6:"),
               "Which of these model terms have interpretations that are",
               em("not"),
               "meaningful for this data?"),
             
             checkboxGroupInput("p1q6",
                                label = NULL,
                                choices = c("$$\\beta_0$$" = "beta_0",
                                            "$$\\beta_1$$" = "beta_1",
                                            "$$\\beta_2$$" = "beta_2",
                                            "$$\\beta_3$$" = "beta_3")),
             actionButton("p1q6_submit",
                          label = "Submit"),
             
             shinyjs::hidden(htmlOutput("p1q6_correct")),
             
             br(),
             
             p(id = "centering_hider",
               em("The final questions will appear after you answer Question 6 correctly.")),
             
             shinyjs::hidden(p(
               id = "centering_hidden1",
               "That's right -- the interpretations of two of these model terms are meaningless right now, since a BMI of 0 is impossible! We can fix that by centering our BMI predictor. To do this, we subtract some amount, c, from each subject's measurement of BMI.")
             ),
             
             shinyjs::hidden(p(
               id = "centering_hidden2",
               "The plot below shows estimates for the coefficients in the non-centered model, with a 95% confidence interval for each. The",
               strong("slider"),
               "lets you select the value of c; in other words, the slider controls how much is subtracted from BMI.")
             ),
             
             shinyjs::hidden(p(
               id = "centering_hidden3",
               "Shift the slider around to several different values and see how the estimates and confidence intervals (CIs) change. When you change the slider value, for each term you'll see the original estimate (the point) and 95% CI (the bars), next to a new estimate and 95% CI after centering by the slider value. Then, answer the questions below the plot."
             )),
             
             br(),
             
             shinyjs::hidden(sliderInput("p1_slider", label = NULL,
                                         min = 0,
                                         max = 40,
                                         value = 0)
             ),
             
             
             shinyjs::hidden(plotOutput("beta_centering")),
             
             br(),
             
             shinyjs::hidden(p(
               id = "centering_hidden4",
               strong("Question 7:"),
               "Why do the estimates and CI widths change for only two of the terms?")
             ),
             
             shinyjs::hidden(textAreaInput("p1q7", label = NULL,
                                           placeholder = "Write your insights...",
                                           width = "55%")
             ),
             
             shinyjs::hidden(p(
               id = "centering_hidden5",
               strong("Question 8:"),
               "What value results in the smallest CIs, and why? What values lead to the largest CIs, and why?")
             ),
             
             shinyjs::hidden(textAreaInput("p1q8", label = NULL,
                                           placeholder = "Write your insights...",
                                           width = "55%")
             ),
             
             br(),
             
             shinyjs::hidden(p(
               id = "centering_hidden6",
               strong("Question 9:"),
               "Suppose we set c at the value of 20. Use the slider to find the estimated intercept and main effect for sex, and interpret the two of them in context.")
             ),
             
             shinyjs::hidden(textAreaInput("p1q9", label = NULL,
                                           placeholder = "Write your insights...",
                                           width = "55%")
             ),
             
             br(),
             
             shinyjs::hidden(p(
               id = "p1_end",
               "After completing these question, use the header of this page to move on to",
               strong("Part 2."))
             )
           )
  ),
  
  # tab for part 2
  tabPanel("Part 2",
           verticalLayout(
             div(class = "rounded-box-solid",
                 p("In this part of the activity, we will look at academic data from schools in Portugal. We have a dataset of 395 students at two different secondary schools in Portugal from the 2005-2006 school year. The data were collected by reviewing student records and presenting students with questionnaires, with the intent of showing how data mining can predict academic performance. The dataset includes several potential predictors as well as our variable of interest: their final math score at the end of the academic year. The full, de-identified dataset can be downloaded in the",
                 strong("Appendix"),
                 "tab."),
                 
                 p("We want to evaluate the effect of years in high school on final math score varies between students living in rural vs. urban areas. (Here, urban/rural refers to where a student lives, not where their school is located.)"),
             ),
             
             br(),
             
             p("Before fitting the model, let's do some EDA to explore the variables of interest:"),
             
             br(),
             
             plotOutput("hist_hs_yrs"),
             
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
             
             withMathJax("$$Y_i = \\beta_0 + \\beta_1 \\text{Urban}_i + \\beta_2 \\text{HS Years}_i + \\beta_3 \\text{Urban}_i \\times \\text{HS Years}_i + \\varepsilon_i$$"),
             
             p(strong("Question 2:"),
               "Using the model equation, find values for the regression coefficients to match the lines of best fit that were already plotted. To help you with this process, two scatterplots are presented below. The plot on the left shows the lines of best fit (as shown earlier), and the plot on the right shows the lines based on the values you choose. Your goal is to get the plot on the right to match the plot on the left. (You can use decimals!)"),
             
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
             splitLayout(cellWidths = c("33%", "33%", "33%"),
                         checkboxGroupInput("p2q3_rural",
                                            label = HTML("Interpretation is limited<br> to rural students:"),
                                            choices = c("$$\\beta_0$$" = "beta_0",
                                                        "$$\\beta_1$$" = "beta_1",
                                                        "$$\\beta_2$$" = "beta_2",
                                                        "$$\\beta_3$$" = "beta_3")),
                         
                         checkboxGroupInput("p2q3_urban",
                                            label = HTML("Interpretation is limited<br> to urban students:"),
                                            choices = c("$$\\beta_0$$" = "beta_0",
                                                        "$$\\beta_1$$" = "beta_1",
                                                        "$$\\beta_2$$" = "beta_2",
                                                        "$$\\beta_3$$" = "beta_3")),
                         
                         checkboxGroupInput("p2q3_yrs0",
                                            label = HTML("Interpretation is limited<br> to students with 0 years in high school:"),
                                            choices = c("$$\\beta_0$$" = "beta_0",
                                                        "$$\\beta_1$$" = "beta_1",
                                                        "$$\\beta_2$$" = "beta_2",
                                                        "$$\\beta_3$$" = "beta_3"))),
             
             splitLayout(cellWidths = c("33%", "33%", "33%"),
                         actionButton("p2q3_rural_submit",
                                      label = "Submit"),
                         actionButton("p2q3_urban_submit",
                                      label = "Submit"),
                         actionButton("p2q3_yrs0_submit",
                                      label = "Submit")),
             splitLayout(cellWidths = c("33%", "33%", "33%"),
                         shinyjs::hidden(htmlOutput("p2q3_rural_correct")),
                         shinyjs::hidden(htmlOutput("p2q3_urban_correct")),
                         shinyjs::hidden(htmlOutput("p2q3_yrs0_correct"))),
             
             br(),
             
             p(strong("Question 4:"),
               "Finally, suppose we centered our years in high school predictor at the mean value in our dataset. Which coefficient estimate(s) would change because of this?"),
             
             
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
  
  # tab for appendix
  tabPanel("Appendix",
           
           h2("Solutions"),
           
           downloadButton("report",
                          label = "Download solutions"),
           
           h2("Data"),
           
           splitLayout(cellWidths = c("25%", "25%"),
                       downloadButton("download_lungs",
                                      label = "Download lung data"),
                       downloadButton("download_math",
                                      label = "Download student data")),
           
           h2("References"),
           
           # code looks ugly bc of citation formatting
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
           
           p("This activity is a Shiny app created using R. Source code is available on ",
             a(href = "https://github.com/david-mcgowan/thesis", "GitHub",
               .noWS = "after"),
             "."),
           
           h2("Acknowledgements"),
           
           p("Thanks to Dr. Ashley Petersen for her advising throughout the development of this activity. Thanks also to Drs. Laura Le, Chelsey Legacy, and Ann Brearley for their assistance in the development process. Lastly, thanks to Emma Billmyer and Julia Kancans for their suggestions on data sources and repositories. This activity was created by David McGowan, with the help of all of these people.")),
  
  id = "navbar",
  position = c("fixed-top") # this fixes the header to the top of the page when scrolling
)


 
# define server
server <- function(input, output, session) {
  
  # JavaScript function to scroll to the top of the page
  scrollToTop <- function() {
    runjs("window.scrollTo(0,0);")
  }
  
  # function to bind tab switch event
  observeEvent(input$navbar, {
    scrollToTop() # scroll to the top when a tab is switched
  })
  
  # feedback for P1Q1
  output$p1q1_correct <- renderText({
    if(input$p1q1_1 == "histogram" & input$p1q1_2 == "scatterplot") {
      '<p style="color:green;"><b>Correct!</b></p> <br> <p>(If the plots do not appear, click Submit again.)</p>'
    } else {
      '<p style="color:red;">Not quite -- try again.</p>'
    }
  })
  
  # feedback for P1Q4
  output$p1q4_correct <- renderText({
    if(input$p1q4 == 4) {
      '<p style="color:green;"><b>Correct!</b></p> <br> <p>(If the model output does not appear, click Submit again.)</p>'
    } else {
      '<p style="color:red;">Not quite -- try again.</p>'
    }
  })
  
  # BMI histogram for Part 1
  output$hist_bmi <- renderPlot({
    ggplot(data = lungs) +
      geom_histogram(aes(x = bmi), bins = 30) +
      labs(x = "BMI",
           y = "Count",
           title = "Distribution of BMI") +
      theme_minimal()
  })
  
  # lung function histogram for Part 1
  output$box_fev <- renderPlot({
    ggplot(data = lungs) +
      geom_boxplot(aes(x = FEVFVC02,
                       y = sex,
                       fill = sex)) +
      scale_fill_manual(values = c("M" = "#D55E00",
                                   "F" = "#0072B2")) +
      labs(y = "Sex",
           x = "FEV1/FVC % at baseline",
           title = "Distribution of lung function at baseline by sex",
           subtitle = "      (Higher values are better!)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # scatterplot for Part 1 without separate lines
  output$scatter_eda <- renderPlot({
    ggplot(data = lungs) +
      geom_point(aes(x = bmi, y = FEVFVC02)) +
      geom_smooth(aes(x = bmi, y = FEVFVC02),
                  se = FALSE,
                  method = "lm") +
      labs(x = "BMI",
           y = "FEV1/FVC % at baseline",
           title = "Line of best fit between BMI and lung function") +
      theme_minimal()
  })
  
  # scatterplot for Part 1 divided by sex
  output$sexplot <- renderPlot({
    ggplot(data = lungs) +
      geom_point(aes(x = bmi, y = FEVFVC02),
                 alpha = .3) +
      geom_smooth(aes(x = bmi, y = FEVFVC02, color = sex),
                  se = FALSE,
                  method = "lm") +
      scale_color_manual(values = c("M" = "#D55E00",
                                    "F" = "#0072B2")) +
      labs(x = "BMI",
           y = "FEV1/FVC % at baseline",
           color = "Sex",
           title = "Lines of best fit for males and females") +
      theme_minimal()
  })
  
  # model output for Part 1
  output$model_pt1 <- renderPrint({
    summary(lm(FEVFVC02 ~ bmi + sex + bmi:sex,
               data = lungs))
  })
  
  # beta plot for Part 1 based on slider value
  output$beta_centering <- renderPlot({
    beta_plot(input$p1_slider)
  })
  
  # histogram for Part 2
  output$hist_hs_yrs <- renderPlot({
    ggplot(data = math) +
      geom_histogram(aes(x = hs_yrs), bins = 8) +
      labs(x = "Years in high school",
           y = "Count",
           title = "Distribution of years in high school") +
      theme_minimal()
  })
  
  # boxplot for Part 2
  output$box_address <- renderPlot({
    ggplot(data = math) +
      geom_boxplot(aes(y = address,
                       x = G3,
                       fill = address)) +
      scale_fill_manual(values = c("Rural" = "#D55E00",
                                   "Urban" = "#0072B2")) +
      labs(y = "Urban/rural status",
           x = "Final math score",
           title = "Distribution of final math score by urban/rural status") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # plot for Part 2 with lines of best fit
  output$p2_scatter_alt <- output$p2_scatter <- renderPlot({
    set.seed(20020521)
    ggplot(data = math) +
      geom_jitter(aes(x = hs_yrs,
                      y = G3),
                  width = .5,
                  height = .5) +
      geom_smooth(aes(x = hs_yrs,
                      y = G3,
                      color = address),
                  method = "lm",
                  se = FALSE) +
      labs(x = "Years in high school",
           y = "Final math score",
           color = "Urban/rural",
           title = "Lines of best fit for rural and urban students") +
      scale_color_manual(values = c("Rural" = "#D55E00",
                                    "Urban" = "#0072B2")) +
      scale_y_continuous(breaks = seq(0, 20, 1)) +
      theme_minimal()
  })
  
  # plot for Part 2 based on beta guesses
  output$beta_guesses <- renderPlot({
    set.seed(20020521) # to have same jitter every time
    ggplot(data = math) +
      geom_jitter(aes(x = hs_yrs,
                      y = G3),
                  width = .5,
                  height = .5) +
      geom_abline(intercept = input$p2q2_beta0,
                  slope = input$p2q2_beta2,
                  color = "#D55E00",
                  linewidth = 1) + # match line width to geom_smooth() width
      geom_abline(intercept = input$p2q2_beta0 + input$p2q2_beta1,
                  slope = input$p2q2_beta2 + input$p2q2_beta3,
                  color = "#0072B2",
                  linewidth = 1) +
      labs(x = "Years in high school",
           y = "Final math score",
           title = "Lines created with your 'guesses' for coefficients") +
      scale_y_continuous(breaks = seq(0, 20, 1)) +
      theme_minimal()
  })
  
  # when P1Q3 answered correctly and submitted, show these
  observeEvent(input$p1q1_submit, {
    shinyjs::show(id = "p1q1_correct")
    
    if(input$p1q1_1 == "histogram" & input$p1q1_2 == "scatterplot") {
      shinyjs::hide(id = "eda_placeholder")
      shinyjs::hide(id = "sex_placeholder")
      shinyjs::show(id = "hist_bmi")
      shinyjs::show(id = "box_fev")
      shinyjs::show(id = "scatter_eda")
      shinyjs::show(id = "sexplot")
    }
  })
  
  # when P1Q4 has been answered correctly, show these
  observeEvent(input$p1q4_submit, {
    shinyjs::show(id = "p1q4_correct")
    
    if(input$p1q4 == 4) {
      shinyjs::hide(id = "model_placeholder")
      shinyjs::hide(id = "beta_placeholder")
      shinyjs::show(id = "model_pt1")
    }
  })
  
  # feedback for P1Q6
  output$p1q6_correct <- renderText({
    # code to prevent error if student gets it wrong and changes answer
    if(is.null(input$p1q6)) {
      ""
    } else if("beta_0" %in% input$p1q6 &
              "beta_1" %notin% input$p1q6 &
              "beta_2" %in% input$p1q6 &
              "beta_3" %notin% input$p1q6) { # if correct answer
      '<p style="color:green;"><b>Correct!</b></p> <br> <p>(If the rest of the page does not appear, click Submit again.)</p>'
    } else { # if incorrect answer
      '<p style="color:red;">Try again.</p>'
    }
  })
  
  # when student submits answer for P1Q6, show feedback
  observeEvent(input$p1q6_submit, {
    shinyjs::show(id = "p1q6_correct")
    
    if(is.null(input$p1q6)) {
      ""
    } else if("beta_0" %in% input$p1q6 &
              "beta_1" %notin% input$p1q6 &
              "beta_2" %in% input$p1q6 &
              "beta_3" %notin% input$p1q6) { # if correct answer
      shinyjs::hide("centering_hider")
      shinyjs::show(id = "centering_hidden1")
      shinyjs::show(id = "centering_hidden2")
      shinyjs::show(id = "centering_hidden3")
      shinyjs::show(id = "p1_slider")
      shinyjs::show(id = "beta_centering")
      shinyjs::show(id = "centering_hidden4")
      shinyjs::show(id = "p1q7")
      shinyjs::show(id = "centering_hidden5")
      shinyjs::show(id = "p1q8")
      shinyjs::show(id = "centering_hidden6")
      shinyjs::show(id = "p1q9")
      shinyjs::show(id = "p1_end")
    }
  })
  
  # feedback for beta guesses
  output$beta0_correct <- renderText({
    # code to prevent error when they guess wrong and change their answer
    if(input$p2q2_beta0 == 0 | is.na(input$p2q2_beta0)) {
      ""
    } else if(input$p2q2_beta0 >= 9.97 & input$p2q2_beta0 <= 10.97) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Keep trying!</p>'
    }
  })
  
  output$beta1_correct <- renderText({
    if(input$p2q2_beta1 == 0 | is.na(input$p2q2_beta1)) {
      ""
    } else if(input$p2q2_beta1 >= 1.2 & input$p2q2_beta1 <= 2.2) {
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
  
  # show model output if they've guessed the correct betas
  output$model_pt2 <- renderPrint({
    if(is.na(input$p2q2_beta0) | is.na(input$p2q2_beta1) |
       is.na(input$p2q2_beta2) | is.na(input$p2q2_beta3)) {
      "Complete Question 2 to make this model output appear."
    } else if(input$p2q2_beta0 >= 9.97 & input$p2q2_beta0 <= 10.97 &
       input$p2q2_beta1 >= 1.2 & input$p2q2_beta1 <= 2.2 &
       input$p2q2_beta2 >= -.37 & input$p2q2_beta2 <= -.27 &
       input$p2q2_beta3 >= -.33 & input$p2q2_beta3 <= -.23) {
      summary(lm(G3 ~ address + hs_yrs + address:hs_yrs,
                 data = math))
    } else {
      "Complete Question 2 to make this model output appear."
    }
  })
  
  # feedback for P2Q3 (see output$p2q4_correct for comments on this code)
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
  
  output$p2q3_yrs0_correct <- renderText({
    if(is.null(input$p2q3_yrs0)) {
      ""
    } else if("beta_0" %in% input$p2q3_yrs0 &
              "beta_1" %in% input$p2q3_yrs0 &
              "beta_2" %notin% input$p2q3_yrs0 &
              "beta_3" %notin% input$p2q3_yrs0) {
      '<p style="color:green;"><b>Correct!</b></p>'
    } else {
      '<p style="color:red;">Try again.</p>'
    }
  })
  
  # show feedback when answers submitted for P2Q3
  observeEvent(input$p2q3_rural_submit, {
    shinyjs::show(id = "p2q3_rural_correct")
  })
  observeEvent(input$p2q3_urban_submit, {
    shinyjs::show(id = "p2q3_urban_correct")
  })
  observeEvent(input$p2q3_yrs0_submit, {
    shinyjs::show(id = "p2q3_yrs0_correct")
  })
  
  # feedback for P2Q4
  output$p2q4_correct <- renderText({
    # code to prevent error if student gets it wrong and changes answer
    if(is.null(input$p2q4)) {
      ""
    } else if("beta_0" %in% input$p2q4 &
              "beta_1" %in% input$p2q4 &
              "beta_2" %notin% input$p2q4 &
              "beta_3" %notin% input$p2q4) { # if correct answer
      '<p style="color:green;"><b>Correct!</b></p>'
    } else { # if incorrect answer
      '<p style="color:red;">Try again.</p>'
    }
  })
  
  # when student submits answer for P2Q4, show feedback
  observeEvent(input$p2q4_submit, {
    shinyjs::show(id = "p2q4_correct")
  })
  
  # server-side code to download lung dataset
  output$download_lungs <- downloadHandler(
    filename = "lhs.csv",
    content = function(file) {
      write.csv(lungs, file)
    }
  )
  
  # server-side code to download math dataset
  output$download_math <- downloadHandler(
    filename = "student-mat.csv",
    content = function(file) {
      write.csv(math, file)
    }
  )
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(p1q2 = input$p1q2,
                     p1q3 = input$p1q3,
                     p1q5 = input$p1q5,
                     p1q7 = input$p1q7,
                     p1q8 = input$p1q8,
                     p1q9 = input$p1q9,
                     p2q1 = input$p2q1,
                     lungs = lungs,
                     math = math)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

# run the application
shinyApp(ui = ui, server = server)
