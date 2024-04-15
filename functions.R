# building all packages from binary instead of source, for app deployment
options(repos = "https://cran.rstudio.com/", type = "binary")

# libraries for this app
library(shiny)
library(shinyjs)
library(tidyverse)
library(kableExtra)

# handy function
`%notin%` <- Negate(`%in%`)

# read in lung data
lungs <- read_csv("lhs.csv") %>%
  # relevel the treatment var
  mutate(alphagroup = as.factor(alphagroup),
         alphagroup = fct_relevel(alphagroup,
                                  c("SI-A", "SI-P"),
                                  after = Inf),
         sex = AGENDER) # considering this is from the 80s, I think we would call this variable sex nowadays

# read in math data
math <- read_csv("student-mat.csv")

# the following was used to adjust the address var categories and use age to create hs_yrs
# (it's since been added to the data file, so no need to run anymore)
  # mutate(address = case_when(address == "R" ~ "Rural",
  #                            address == "U" ~ "Urban")),
  #        hs_yrs = age - 14) # centering age, using 14 since min(age) = 15

# function to find confidence interval from lm output
get_CI <- function(model) {
  coefs <- summary(model)$coefficients[,1]
  SEs <- summary(model)$coefficients[,2]
  
  return(tibble(beta = coefs,
                se = SEs) %>%
           mutate(ci_low = beta - qnorm(.975) * se,
                  ci_high = beta + qnorm(.975) * se))
}

# function to plot coef estimates and CIs after centering by some amount `change`
beta_plot <- function(change) {
  # if change is 0 (at app startup)
  if(change == 0) {
    model <- lm(FEVFVC02 ~ bmi + sex + bmi * sex,
                data = lungs)
    
    df <- get_CI(model) %>%
      bind_cols(name = c("Intercept", "BMI", "Sex (M)", "BMI * Sex")) %>%
      mutate(name = as.factor(name),
             name = fct_relevel(name, "Intercept", "BMI",
                                "Sex (M)", "BMI * Sex"))
    
    # dummy df to artifically set the limits,
    # to match the plot created when c \neq 0
    dummy_df <- tibble(name = c(rep("Intercept", 2),
                                rep("BMI", 2),
                                rep("Sex (M)", 2),
                                rep("BMI * Sex", 2)),
                       beta = c(68, 62,
                                NA, NA,
                                2, -6.5,
                                NA, NA)) %>%
      mutate(name = as.factor(name),
             name = fct_relevel(name, "Intercept", "BMI",
                                "Sex (M)", "BMI * Sex"))
    
    plot <- df %>%
      ggplot() +
      geom_point(aes(x = "",
                     y = beta),
                 shape = 15) +
      geom_errorbar(aes(x = "",
                        ymin = ci_low,
                        ymax = ci_high)) +
      geom_blank(data = dummy_df,
                 aes(x = "",
                     y = beta)) +
      facet_wrap(~name,
                 scales = "free") +
      labs(x = "",
           y = "Coefficient estimate") +
      theme_minimal()
  } else {
    # for all other values of c, plot original model and adjusted model
    
    dataset <- lungs %>%
      mutate(bmi_adj = bmi - change)
    
    model <- lm(FEVFVC02 ~ bmi + sex + bmi * sex,
                data = lungs)
    
    model_adj <- lm(FEVFVC02 ~ bmi_adj + sex + bmi_adj * sex,
                    data = dataset)
    
    df <- get_CI(model) %>%
      bind_cols(name = c("Intercept", "BMI", "Sex (M)", "BMI * Sex")) %>%
      mutate(name = as.factor(name),
             name = fct_relevel(name, "Intercept", "BMI",
                                "Sex (M)", "BMI * Sex")) %>%
      mutate(model = "Original")
    
    df_adj <- get_CI(model_adj) %>%
      bind_cols(name = c("Intercept", "BMI", "Sex (M)", "BMI * Sex")) %>%
      mutate(name = as.factor(name),
             name = fct_relevel(name, "Intercept", "BMI",
                                "Sex (M)", "BMI * Sex")) %>%
      mutate(model = "Adjusted")
    
    full_df <- bind_rows(df,
                         df_adj) %>%
      mutate(model = as.factor(model),
             model = fct_relevel(model, "Original", "Adjusted"))
    
    # dummy df to artifically set the limits,
    # since each plot in the facet_wrap() needs different limits
    dummy_df <- tibble(model = rep(c("Original", "Adjusted"),
                                   4),
                       name = c(rep("Intercept", 2),
                                rep("BMI", 2),
                                rep("Sex (M)", 2),
                                rep("BMI * Sex", 2)),
                       beta = c(68, 62,
                                NA, NA,
                                2, -6.5,
                                NA, NA)) %>%
      mutate(model = as.factor(model),
             model = fct_relevel(model, "Original", "Adjusted"),
             name = as.factor(name),
             name = fct_relevel(name, "Intercept", "BMI",
                                "Sex (M)", "BMI * Sex"))
    
    plot <- ggplot(data = full_df) +
      geom_point(aes(x = model,
                     y = beta,
                     color = model,
                     shape = model)) +
      geom_errorbar(aes(x = model,
                        ymin = ci_low,
                        ymax = ci_high,
                        color = model)) +
      geom_blank(data = dummy_df,
                 aes(x = model,
                     y = beta)) +
      facet_wrap(~name,
                 scales = "free") +
      scale_color_manual(values = c("Original" = "black",
                                    "Adjusted" = "red3")) +
      scale_shape_manual(values = c("Original" = 15,
                                    "Adjusted" = 16)) +
      theme(legend.position = "right") +
      labs(x = "",
           y = "Coefficient estimate",
           color = "Model",
           shape = "Model") +
      theme_minimal()
  }
  
  return(plot)
}
