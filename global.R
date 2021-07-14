library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(readr)
library(ggpubr)
library(readxl)
library(hash)
library(tidyr)
library(tibble)


# Add sample or control column to stats input data frame if OMIQ workflow
add_sample_or_control_column <- function(df){
    ## Repeat "control" or "sample" for length of unique concentrations
    control_rep <- rep("control", length(unique(df$Concentration)))
    sample_rep <- rep("sample", length(unique(df$Concentration)))
    
    ## Determine # of rows to repeat pattern for
    unique_rows <- length(unique(df$Plate.Row))
    unique_pops <- length(unique(df$pop))
    
    ## Add sample or control column to stats file
    df <- df %>% add_column("Sample or Control" = rep(c(control_rep, sample_rep), unique_rows*unique_pops))
    return(df)
}

## Configure stats tables
configure_stats <- function(stats, pop){
  if(unique(stats$units) == "ug/test"){
    stats$Concentration <- stats$Concentration * 1000 # convert to ng/test
  }
  stats <- stats[stats$`Sample or Control` == "sample",] %>% arrange(Concentration)
  stats_pop <- stats[stats$pop == pop,]
  
  stats_pop <- stats_pop %>% select(Stability.Time.point, Concentration, `%+`, `MFI+`, `MFI-`, `rSD-`)
  colnames(stats_pop)[1] <- c("Condition")
  return(stats_pop)
  
}


# Calculate the % of 4C Reference MFI data from the uploaded stats
calculate_perct_4C_MFI <- function(df){

    df$`%+` <- as.numeric(ifelse(sapply(as_tibble(rep(NA, length(df$`%+`))), function(i) { df$`%+` == "n/a" | df$`%+` == "#N/A" }), NA, df$`%+`))
    df$`MFI+` <- as.numeric(ifelse(sapply(as_tibble(rep(NA, length(df$`MFI+`))), function(i) { df$`MFI+` == "n/a" | df$`MFI+` == "#N/A" }), NA, df$`MFI+`))
    df$`MFI-` <- as.numeric(ifelse(sapply(as_tibble(rep(NA, length(df$`MFI-`))), function(i) { df$`MFI-` == "n/a" | df$`MFI-` == "#N/A" }), NA, df$`MFI-`))
    df$`rSD-` <- as.numeric(ifelse(sapply(as_tibble(rep(NA, length(df$`rSD-`))), function(i) { df$`rSD-` == "n/a" | df$`rSD-` == "#N/A" }), NA, df$`rSD-`))
    
    
    df <- df %>% arrange(Concentration, Condition)
    
    calc_vect <- c() # Initialize % of 4C MFI data
    for(i in unique(df$Concentration)){
        for(row in c(1:nrow(df[df$Concentration == i,]))){

            ref_for_each_conc <- df$`MFI+`[df$Concentration == i & df$Condition == 0]
            MFI <- df$`MFI+`[df$Concentration == i][[row]]
            calc <- round((as.numeric(MFI)/as.numeric(ref_for_each_conc))*100,0)
            calc_vect <- append(calc_vect, calc)
            
        }
    }
    df <- bind_cols(df, "% 4C Reference MFI"=calc_vect)
    return(df)
}

# Convert the reference MFI table to a wide table, each column designated to each concentration
create_raw_reference_MFI_table_wide <- function(df){
    
    # Take only subset of raw stats table
    df_selected <- select(df, c(Condition, Concentration, `% 4C Reference MFI`))

    df_wide <- df_selected %>% pivot_wider(names_from = Concentration, values_from = `% 4C Reference MFI`)
    colnames(df_wide)[1] <- "Time"
    for(i in c(2:length(colnames(df_wide)))){
        colnames(df_wide)[i] <- paste0(colnames(df_wide)[i], " ng/test")
    }
    df_wide <- df_wide %>% arrange(Time)
    return(df_wide)
}

## Configure concentrations to output to UI and include in analysis

# Create concentration choice names for UI selection (based on file input)
concentration_choiceNames <- function(df){
    list_of_concentrations <- unique(sort(df$Concentration))
    choiceNames <- c()
    for(i in list_of_concentrations){
        name <- paste(i, "ng/test")
        choiceNames <- append(choiceNames, name)
    }
    return(choiceNames)
}

# Create concentration choice values for UI selection (based on choice names)
concentration_choiceValues <- function(choiceNames){
    choiceValues <- c(1:length(choiceNames)+1)
    return(choiceValues)
}

# Returns wide table with only included concentrations
concentrations_to_keep <- function(df, columns_to_include){
    
    list_of_included_columns <- c()
    # Loop through list of included columns (taking the choiceValues() each column is assigned to)
    for (i in columns_to_include) {
        list_of_included_columns <-
            c(list_of_included_columns, as.numeric(i))
    }
    # If no columns included, return empty vector
    if (is.null(df)) {
        df_selected <- c()
    }
    # Else, select all of included columns from df
    else{
        df_selected <- dplyr::select(df, all_of(list_of_included_columns))
    }
    
    # Return table with Time column and all columns included by user
    concentrations_to_keep_df <- cbind('Time' = df$Time, df_selected)
    
    return (concentrations_to_keep_df)
}

# Melt data to generate `keep()` reactive expression used on UI and in analysis
melt_reference_mfi_table <- function(df_full=NULL){
  # Melt Columns by Time
  dataMelt <- melt(df_full, "Time", variable='Concentrations')
  dataMelt <- cbind(dataMelt, 'Labels'=paste0(parse_number(as.character(dataMelt$Concentrations)), ' ng/test'))
  
  return(na.omit(dataMelt))
}

# Create % of 4C Reference MFI table show on 'Stats Table' tab of UI
create_reference_MFI_table_wide_with_keeps <- function(keep){
    
    keep <- na.omit(keep)
    df_selected <- select(keep, c(Time, Concentrations, value))
    
    df_wide <- df_selected %>% pivot_wider(names_from = Concentrations, values_from = value)
    
    df_wide <- df_wide %>% arrange(Time)
    return(df_wide)
}


## Model order, equation, and R^2 value

order_of_polynomial <- function(selected_order){
  order <- switch(selected_order, "Linear"=1, "2nd Order"=2, "3rd Order"=3)
  return(order)
}

get_model_coeff_pvalues <- function(df_melt, order){
  fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
  summary_regression <- summary(fit)
  p_values <- summary_regression$coefficients[,4]
  
  a_pvalue <- p_values[1]
  b_pvalue <- p_values[2]
  c_pvalue <- p_values[3]
  d_pvalue <- p_values[4]
  
  pvalue_df <- data.frame('a_pvalue'=a_pvalue,
                          'b_pvalue'=b_pvalue,
                          'c_pvalue'=c_pvalue,
                          'd_pvalue'=d_pvalue)
  
  return(pvalue_df)
}

R_sq <- function(df_melt, order){
    summary_regression <- summary(lm(value ~ poly(Time,order, raw=TRUE), data=df_melt))
    r_sq <- format(round(summary_regression$r.squared,2)) # R^2 value
    adj_r_sq <- format(round(summary_regression$adj.r.squared,2)) # Adjusted R^2 value

    return(as.numeric(r_sq))

}

best_fit_equation <- function(df_melt, order){
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    return(summary(fit))
}


## Confidence bands, regression plot and shelf-life(s)

find_confidence_bands <- function(df_melt, order, CI, threshold_y) {
    
    df_melt <- na.omit(df_melt)
    y <- na.omit(df_melt$value)
    x <- na.omit(df_melt$Time)
    n <- length(y) # Find length of y to use as sample size

    fit <- lm(y ~ poly(x,order,raw=TRUE),data=df_melt)

    summary_regression <- summary(fit)

    a <- as.numeric(format(round(summary_regression$coefficients[[1]],2))) # y-intercept
    b <- as.numeric(format(round(summary_regression$coefficients[[2]],2))) # 1st order coeff
    c <- ifelse(order > 1, as.numeric(format(round(summary_regression$coefficients[[3]],2))), 0) # 2nd order coeff
    d <- ifelse(order > 2, as.numeric(format(round(summary_regression$coefficients[[4]],2))), 0) # 3rd order coeff
    
    x_new <- seq(min(x), max(x), length.out = length(x))
    y_fit <- a + b*x_new + c*x_new^2 + d*x_new^3
    
    pre_predict <- predict(fit, data.frame(x_new))
    p1 <- ggplot(data=df_melt) +
        geom_point(data=df_melt, aes(x=x_new, y=y), col='blue')
    conf_df <- predict(fit, data.frame('x_new'=x_new), interval='confidence',level=CI, se.fit=TRUE)
    predict_df <- data.frame('x_new'=x_new, 'fit'=y_fit, 'lwr'=conf_df$fit[,2], 'upr'=conf_df$fit[,3], 'se_fit'=conf_df$se.fit)
    slope.upper <- predict_df$upr
    slope.lower <- predict_df$lwr

    fit_lower <- lm(slope.lower ~ poly(x,order, raw=TRUE))
    summary_regression_lower <- summary(fit_lower)

    f1 <- function(x) a + b*x + c*x^2 + d*x^3
    f2 <- function(x) threshold_y
    
    a_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[1]],2))) # y-intercept
    b_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[2]],2))) # 1st order coeff
    c_lower <- ifelse(order > 1, as.numeric(format(round(summary_regression_lower$coefficients[[3]],2))), 0) # 2nd order coeff
    d_lower <- ifelse(order > 2, as.numeric(format(round(summary_regression_lower$coefficients[[4]],2))), 0) # 3rd order coeff
    
    f1_lower <- function(x) a_lower + b_lower*x + c_lower*x^2 + d_lower*x^3
    f2_lower <- function(x) threshold_y

    bands <- data.frame(cbind(predict_df$x_new, predict_df$lwr, predict_df$fit, predict_df$upr))
    colnames(bands) <- c('X Values', 'Lower Confidence Band', 'Y Values', 'Upper Confidence Band')

    
    return(bands)
}

regression_plot_global <- function(text_size, data_point_size, eqn_size, df, confidence_bands, order, CI, eqn_location, eqn_location2){
  
  p <- ggplot(df, aes(x=Time, y=value, color=Concentrations)) + 
    geom_ribbon(data=df, 
                aes(x=Time, y=value, 
                    ymin=confidence_bands[[2]], 
                    ymax=confidence_bands[[4]]), 
                formula = y ~ poly(x,order, raw=TRUE), method="lm",col = "red", 
                level=as.numeric(CI), alpha=0.2) +
    geom_line(data=confidence_bands, 
              aes(x=`X Values`, y=`Y Values`), 
              formula = y ~ poly(x,order, raw=TRUE), method="lm", col = "red") +
    geom_point(size=data_point_size) + 
    labs(x = "Time (years)",
         y = "% of 4C Reference MFI") +
    theme_minimal() +
    scale_color_brewer(palette = 'Dark2', na.translate = F,
                       labels = unique(df$Labels)) +
    theme(text=element_text(size = text_size),
          legend.position = "bottom")
  return(suppressWarnings(p))
}

solve_for_shelf_life <- function(df_melt, threshold_y, order){
  df_melt <- na.omit(df_melt)
  
  x <- na.omit(df_melt$Time)
  y <- na.omit(df_melt$value)
  
  fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
  fit_residuals <- resid(fit)
  
  p <- ggplot(df_melt,aes(x=y, y=fit_residuals, label=Time, color=Concentrations)) + 
    geom_point(size=3) +
    # geom_area(color='blue') +
    geom_hline(aes(yintercept=0)) +
    ylim(0-max(abs(fit_residuals)), 0+max(abs(fit_residuals)))
  
  
  
  
  q <- ggplot(df_melt,aes(x=x, y=fit_residuals, label=Concentrations, color=Time)) + 
    geom_point(size=3) +
    # geom_area(color='blue') +
    geom_hline(aes(yintercept=0)) +
    ylim(0-max(abs(fit_residuals)), 0+max(abs(fit_residuals)))
  
  
  
  summary_regression <- summary(fit)
  
  a <- round(as.numeric(summary_regression$coefficients[[1]]),2) # y-intercept
  b <- round(as.numeric(summary_regression$coefficients[[2]]),2) # 1st order coeff
  c <- ifelse(order > 1, round(as.numeric(summary_regression$coefficients[[3]]),2), 0) # 2nd order coeff
  d <- ifelse(order > 2, round(as.numeric(summary_regression$coefficients[[4]]),2), 0) # 3rd order coeff
  
  r_sq <- format(round(summary_regression$r.squared,2)) # Adjusted R^2 value
  
  # Find intersect of two functions to determine shelf-life
  f1 <- function(x) a + b*x + c*x^2 + d*x^3
  f2 <- function(x) threshold_y
  
  shelf_life <- tryCatch(uniroot(function(x) f1(x)-f2(x),c(0,5), extendInt="yes")$root, error = function(c){
    cat("Never crosses MFI Threshold - no shelf-life can be found \n")
    shelf_life = NULL
    return(shelf_life)
  })
  
  return(shelf_life)
}

solve_for_lower_shelf_life <- function(df_melt, order, CI, threshold_y){
  df_melt <- na.omit(df_melt)
  
  x <- na.omit(df_melt$Time)
  y <- na.omit(df_melt$value)
  
  n <- length(y) # Find length of y to use as sample size
  
  fit <- lm(y ~ poly(x,order,raw=TRUE),data=df_melt)
  summary_regression <- summary(fit)
  a <- as.numeric(format(round(summary_regression$coefficients[[1]],2))) # y-intercept
  b <- as.numeric(format(round(summary_regression$coefficients[[2]],2))) # 1st order coeff
  c <- ifelse(order > 1, as.numeric(format(round(summary_regression$coefficients[[3]],2))), 0) # 2nd order coeff
  d <- ifelse(order > 2, as.numeric(format(round(summary_regression$coefficients[[4]],2))), 0) # 3rd order coeff
  
  x_new <- seq(min(x), max(x), length.out = length(x))
  y_fit <- a + b*x_new + c*x_new^2 + d*x_new^3
  
  # pre_predict <- predict(fit, data.frame(x_new))
  pre_predict <- predict(fit, data.frame('x_new'=x_new), interval='confidence',level=CI, se.fit=TRUE)
  pre_predict_df <- data.frame('x_new'=x_new, 'fit'=y_fit, 'lwr'=pre_predict$fit[,2], 'upr'=pre_predict$fit[,3])
  
  slope.upper <- pre_predict_df$upr
  slope.lower <- pre_predict_df$lwr
  
  fit_lower <- lm(slope.lower ~ poly(x,order, raw=TRUE))
  summary_regression_lower <- summary(fit_lower)
  
  f1 <- function(x) a + b*x + c*x^2 + d*x^3
  f2 <- function(x) threshold_y
  
  # shelf_life <- uniroot(function(x) f1(x)-f2(x),c(0,5), extendInt="yes")$root
  
  a_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[1]],2))) # y-intercept
  b_lower <- as.numeric(format(round(summary_regression_lower$coefficients[[2]],2))) # 1st order coeff
  c_lower <- ifelse(order > 1, as.numeric(format(round(summary_regression_lower$coefficients[[3]],2))), 0) # 2nd order coeff
  d_lower <- ifelse(order > 2, as.numeric(format(round(summary_regression_lower$coefficients[[4]],2))), 0) # 3rd order coeff
  
  f1_lower <- function(x) a_lower + b_lower*x + c_lower*x^2 + d_lower*x^3
  f2_lower <- function(x) threshold_y
  
  # UPDATE: ADDING TRYCATCH STATEMENT IF DOESNT CROSS MFI THRESHOLD
  shelf_life_lower <- tryCatch(uniroot(function(x) f1_lower(x)-f2_lower(x),c(0,5), extendInt="yes")$root, error = function(c){
    cat("Never crosses MFI Threshold - no shelf-life can be found \n")
    shelf_life_lower = NULL
    return(shelf_life_lower)
  })
  return(round(shelf_life_lower,2))
}

rounded_shelf_life <- function(shelf_life){
  
  ## If cannot find shelf-life, output message to UI
  if(is.null(shelf_life)){
    cat("Never crosses MFI Threshold - no shelf-life can be found \n")
    return(shelf_life)
    
  }
  
  ## Rounding rules:
  ## 1. If greater than 5 years, round down to 5 years to avoid extrapolation
  ## 2. If 1.5yrs, don't round down
  ## 3. Otherwise, round down to nearest half integer
  
  # If shelf-life is greater than 5 years, round down to 5 years to avoid extrapolation
  else if(shelf_life > 5){
    shelf_life <- 5 # max timepoint tested
  }
  # If shelf-life is equal to or below 1.5y, don't round down
  else if(shelf_life <= 1.5){
    shelf_life <- shelf_life
  }
  # Otherwise, round down to nearest half integer
  else{
    shelf_life <- floor(shelf_life / 0.5) * 0.5
  }
  
  return(shelf_life)
}


## Residuals Calculations & Plots from 'Residuals & Normality Checks' tab 

find_residuals <- function(df_melt, order){
  df_melt <- na.omit(df_melt)
  
  x <- na.omit(df_melt$Time)
  y <- na.omit(df_melt$value)
  n <- length(x)
  fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
  fit_residuals <- resid(fit)
  df <- df_melt %>% add_column("Residuals"=fit_residuals)
  return(df)
}

residual_vs_fit_plot <- function(df_melt, order, font_size, data_point_size){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fitted_y <- data.frame("y-fit"=fitted(fit))
    fit_residuals <- resid(fit)
    
    p <- ggplot(df_melt,aes(x=fitted_y$y.fit, y=fit_residuals, label=Time, 
                            text = sprintf("Time: %s yrs<br>Concentration: %s<br>Actual %% of 4C Ref. MFI: %.0f<br>Predicted %% of 4C Ref. MFI: %.0f<br>Residuals: %.2f", 
                                        Time, Concentrations, value, fitted_y$y.fit, fit_residuals))) + 
        geom_point(size=data_point_size, color = '#eb6864') +
        geom_hline(aes(yintercept=0)) +
        ylim(0-max(abs(fit_residuals)), 0+max(abs(fit_residuals))) +
        labs(title = 'Residuals vs. Fit Plot',
             x = 'Predicted % of 4C MFI', 
             y = 'Residuals') +
        theme(text=element_text(size = font_size))
    # p <- ggplotly(p, tooltip=c("text"))
    return(p)
}

residual_vs_fit_plot_w_tooltip <- function(p){
    p <- ggplotly(p, tooltip=c("text"))
    return(p)
}

residual_histogram <- function(df_melt, order, font_size){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fit_residuals <- resid(fit)
    
    p <- ggplot(df_melt,aes(x=fit_residuals, label=Time)) + 
        geom_histogram(binwidth=sd(fit_residuals), boundary=0, fill = '#eb6864') +
        labs(title = 'Histogram of Residuals',
             x = 'Residuals', 
             y = '# of Residuals') +
        theme(text=element_text(size = font_size))

    return(p)
}

normal_probability_plot <- function(df_melt, order, residuals, font_size, data_point_size){
    # And adding line with proper properties
    p <-
        ggplot(residuals, mapping = aes(sample = Residuals)) +
        stat_qq_point(size = data_point_size,color = "#eb6864") +
        stat_qq_line(color="black") +
        # geom_abline(aes(slope=1, intercept=0), color="black") +
        # geom_qq(color = "#eb6864") +
        # geom_qq_line(colour = "black") +
        labs(title = 'Normal Probability Plot of Residuals',
             x = 'Theoretical Quantiles',
             y = 'Residuals') +
        theme(text=element_text(size = font_size))
    return(p)
}

normal_probability_plot_w_tooltip <- function(p){
    p <- ggplotly(p)
    return(p)
}

anderson_darling_normality_test <- function(residuals){
    ad <- ad.test(residuals$Residuals)
    p_value <- ad$p.value
    
    # null hypothesis is that data DOES follow normal distribution
    # can reject null hypothesis if p-value < 0.05 --> meaning we can say with sufficient evidence that data does NOT follow normal distribution
    return(p_value)
}


## Stats Plots from 'Plots' tab

mfi_vs_concentration_plot <- function(df){
    
    p <- ggplot(df, aes(x=as.factor(Concentration), y=as.numeric(`MFI+`), group=Condition, color=as.factor(Condition))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Dark2", labels=unique(paste0(df$Condition, " years"))) +
        labs(title = 'MFI vs Concentration',
             x = 'Concentration (ng/test)', 
             y = 'MFI',
             color = "Time (years)")+
        theme(text=element_text(size = 11),
              legend.position = "bottom")
    
    return(p)
}

mfi_vs_time_plot <- function(df){

    p <- ggplot(df, aes(x=as.factor(Condition), y=as.numeric(`MFI+`), group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Dark2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = 'MFI vs Time',
             x = 'Time (years)', 
             y = 'MFI',
             color = "Concentration") +
        theme(text=element_text(size = 11),
              legend.position = "bottom")
    
    return(p)
}

stain_index <- function(df){
    si = (as.numeric(df$`MFI+`) - as.numeric(df$`MFI-`))/(2*as.numeric(df$`rSD-`))
    
    si <- as.numeric(ifelse(sapply(as_tibble(rep(NA, length(si))), function(i) { as.numeric(df$`rSD-`) == 0 | is.na(df$`rSD-`) }), NA, si))
    
    df <- cbind(df, "Stain Index"=si)
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`Stain Index`, group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Dark2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = 'Stain Index',
             x = 'Time (years)', 
             y = 'Stain Index',
             color = "Concentration") +
        theme(text=element_text(size = 11),
              legend.position = "bottom") +
        ylim(0,NA)
    
    return(p)
}

signal_to_noise <- function(df){
    sn = as.numeric(df$`MFI+`)/as.numeric(df$`MFI-`)
    df <- cbind(df, "Signal-to-Noise"=sn)
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`Signal-to-Noise`, group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Dark2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = 'Signal-to-Noise',
             x = 'Time (years)', 
             y = 'Signal-to-Noise',
             color = "Concentration") +
        theme(text=element_text(size = 11),
              legend.position = "bottom") +
        ylim(0,NA)
    
    return(p)
}

percent_positive <- function(df){
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=as.numeric(`%+`), group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Dark2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = '(%) Positive',
             x = 'Time (years)', 
             y = '(%) Positive',
             color = "Concentration") +
        theme(text=element_text(size = 11),
              legend.position = "bottom") +
        ylim(0,NA)
    
    return(p)
    
}
 
percent_of_4C_MFI <- function(df){
    p <- ggplot(df, aes(x=as.factor(Condition), y=as.numeric(`% 4C Reference MFI`), group=Concentration, color=as.factor(Concentration))) +
        geom_point(size=4) +
        geom_line(size=1) +
        scale_colour_brewer(palette="Dark2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = '% of 4C Reference MFI',
             x = 'Time (years)',
             y = '%',
             color = "Concentration") +
        theme(text=element_text(size = 11),
              legend.position = "bottom") +
        ylim(0,NA)

    return(p)
}


## Functions specific to downloadable outputs

# Configures color of shelf-life output text (used in downloadable PPT for Manual analysis only)
shelf_life_color <- function(shelf_life){
  if( shelf_life < 0 ){ # If shelf-life is negative, font --> yellow
    return("#fcba03")
  }
  return("#000000") # Else font --> black
  
}