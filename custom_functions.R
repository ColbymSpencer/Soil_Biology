## Custom Functions

print_equation <- function(model, 
                           round_to = 3, beta = 'Beta_'){
  
  n <- names(coef(model)) #model var names
  n <- gsub(' ', '', n)
  nums <- round(coef(model), round_to) # effect values
  zipped <- c() #placeholder
  
  ## filled-in equation
  for (i in 1:length(nums)) {
    zipped[i] <- paste(nums[i], n[i], sep = '*')
  }
  
  eq <- paste(zipped , collapse = ' + ') # almost filled equation
  
  eq <- gsub('\\*\\(Intercept\\)','', eq) # remove the intercept variable name
  eq <- gsub(' \\+ -',' - ', eq) # reformat the negative numbers
  eq <- gsub(':','*', eq) # interactions
  filled_eq <- paste0('y_hat = ',eq)
  
  
  ## unfilled equation
  zipped <- c()
  betas <- paste0(beta, 0:(length(nums)-1))
  for (i in 1:length(nums)) {
    zipped[i] <- paste(betas[i], n[i], sep = '*')
  }
  eq <- paste(zipped , collapse = ' + ') # almost filled equation
  
  eq <- gsub('\\*\\(Intercept\\)','', eq) # remove the intercept variable name
  eq <- gsub(' \\+ -',' - ', eq) # reformat the negative numbers
  eq <- gsub(':','*', eq) # interactions
  unfilled_eq <- paste0('E(Y) = ',eq)
  
  return(c(filled_eq, unfilled_eq))
}

print_vars <- function(model) {
  clean_names <- gsub("[^A-Za-z0-9]", "_", names(coef(model)))
  vars_to_delare <- cat(paste(clean_names, collapse = "<-\n"))
  
  return(c(clean_names))
}

assumption_plots <- function(model) {
  op <- par(mfrow = c(1,3))
  plot(model, which = 1:3)
  par(op)
}


#packages
library(ggplot2)

# QQplot
# @param x : een lineair model
QQplot <- function(x){
  ggplot(data.frame(res = residuals(x)),
         aes(sample=res)) +
    stat_qq() + stat_qq_line()
}

# resplots
residualplot <- function(x){
  # Make a data frame with the correct values
  res <- residuals(x)
  rdf <- data.frame(
    res = c(res, sqrt(abs(res))),
    fit = rep(fitted(x), times = 2),
    lab = rep(c("Residuals","sqrt(abs(Residuals))"),
              each = length(res))
  )
  
  # Make the plot
  ggplot(rdf, aes(x=fit, y=res)) +
    geom_point() +
    geom_smooth(method="gam", formula = y ~ s(x)) +
    geom_smooth(method = "lm", col = alpha("red",0.5), se = FALSE) +
    facet_wrap(vars(lab), 
               ncol = 2,
               scale = "free_y", labeller = label_parsed) +
    labs(x = "Fitted values", y= "")
}