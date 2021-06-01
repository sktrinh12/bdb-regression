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
# order <- 2

template_data <- tibble('Time'=c(0,0.5,1,1.5,2,3,4,5), 
                            'Conc_15_ng'=rep(NA, 8),
                            'Conc_30_ng'=rep(NA, 8), 
                            'Conc_60_ng'=rep(NA, 8),
                            'Conc_125_ng'=rep(NA, 8),
                            'Conc_250_ng'=rep(NA, 8),
                            'Conc_500_ng'=rep(NA, 8),
                            'Conc_1000_ng'=rep(NA, 8),
                            'Conc_2000_ng'=rep(NA, 8)
)

## Calculate the % of 4C Reference MFI data from the uploaded stats
calculate_perct_4C_MFI <- function(df){
    
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

## FOR UI USE ONLY
## Convert the reference MFI table to a wide table, each column designated to each concentration
## If 15 ng/test isn't included, will add to the table, but automatically toggle off of UI. 
create_reference_MFI_table_wide_UI_only <- function(df){
    
    # Take only subset of raw stats table
    df_selected <- select(df, c(Condition, Concentration, `% 4C Reference MFI`))

    df_wide <- df_selected %>% pivot_wider(names_from = Concentration, values_from = `% 4C Reference MFI`)
    colnames(df_wide)[1] <- "Time"
    for(i in c(2:length(colnames(df_wide)))){
        colnames(df_wide)[i] <- paste0(colnames(df_wide)[i], " ng/test")
    }

    if("15 ng/test" %in% colnames(df_wide)){
        return(df_wide)
    }
    else{
        df_wide <- add_column(df_wide, "15 ng/test" = c(NA), .after="Time")
    }
    return(df_wide)
}

create_raw_reference_MFI_table_wide <- function(df){
    
    # Take only subset of raw stats table
    df_selected <- select(df, c(Condition, Concentration, `% 4C Reference MFI`))

    df_wide <- df_selected %>% pivot_wider(names_from = Concentration, values_from = `% 4C Reference MFI`)
    colnames(df_wide)[1] <- "Time"
    for(i in c(2:length(colnames(df_wide)))){
        colnames(df_wide)[i] <- paste0(colnames(df_wide)[i], " ng/test")
    }

    return(df_wide)
}

create_modified_reference_MFI_table_wide <- function(df){
    df_selected <- select(df, c(Condition, Concentration, `% 4C Reference MFI`))

    df2 <- df_selected %>% pivot_wider(names_from = Concentration, values_from = `% 4C Reference MFI`)
    colnames(df2)[1] <- "Time"
    for(i in c(2:length(colnames(df2)))){
        # colnames(df2)[i] <- paste0("Conc_", colnames(df2)[i], "_ng")
        colnames(df2)[i] <- paste0(colnames(df2)[i], " ng/test")
    }
    
    # if("Conc_15_ng" %in% colnames(df2)){
    # if("15 ng/test" %in% colnames(df2)){
    #     return(df2)
    # }
    # else{
    #     df2 <- add_column(df2, "15 ng/test" = c(NA), .after="Time")
    # }
    return(df2)
}

concentrations_to_keep <- function(reference_MFI_data_wide, columns_to_include){
    df <- reference_MFI_data_wide
    
    concentrations_to_keep <- c()
    list_of_included_columns <- c()
    for (i in columns_to_include) {
        concentrations_to_keep <- cbind(concentrations_to_keep, df[[as.numeric(i)]])
        list_of_included_columns <- c(list_of_included_columns, as.numeric(i))
    }
    if(is.null(df)){
        df_selected <- c()
    }
    else{
        df_selected <- dplyr::select(df, all_of(list_of_included_columns))
    }

    concentrations_to_keep_df <- cbind('Time'=df$Time, df_selected)

    return (concentrations_to_keep_df)
}

concentrations_around_optimal <- function(optimal){
    key <- c('15 ng/test',
             '30 ng/test',
             '60 ng/test',
             '125 ng/test',
             '250 ng/test',
             '500 ng/test',
             '1000 ng/test',
             '2000 ng/test')
    
    value <- c(2:9)
    
    h <- hash(key,value)
    optimal_key <- as.character(paste0(optimal, ' ng/test'))
    optimal_value <- as.numeric(values(h)[optimal_key])
    # print(optimal_value)
    if(optimal_value == max(value)){
        conc_around_optimal <- c(optimal_value-2, optimal_value-1, optimal_value)
    } else if(optimal_value == min(value)){
        conc_around_optimal <- c(optimal_value, optimal_value+1)
    } else if(optimal_value == min(value)+1){
        conc_around_optimal <- c(optimal_value-1, optimal_value, optimal_value+1)
    } else{
        conc_around_optimal <- c(optimal_value-2, optimal_value-1, optimal_value, optimal_value+1)
    }
    
    return(conc_around_optimal)
}

melt_reference_mfi_table <- function(df_full=template_data){
    
    # Melt Columns by Time
    dataMelt <- melt(df_full, "Time", variable='Concentrations')

    dataMelt <- cbind(dataMelt, 'Labels'=paste0(parse_number(as.character(dataMelt$Concentrations)), ' ng/test'))
    # print('dataMelt: ')

    
    return (dataMelt)
}
# dotPlotData <- melt_reference_mfi_table(fullDataTable(reference_MFI_data_wide))


regressionDataTable <- function(df_full) {
    
    ############ User Inputs ############
    
    # COnfidence Interval
    CI_level <- 0.95
    
    # Threshold % of 4C MFI value to determine shelf-life
    threshold_y = 75
    
    # df_csv <- reference_MFI_data_wide
    # 
    # df_full <- cbind(df_csv, "Average"=rowMeans(df_csv[2:ncol(df_csv)]))
    
    
    df_regression <- data.frame("Time"=df_full$Time,"Average"=df_full$value, na.omit = TRUE)
    # print('df_regression: ')
    # print(df_regression)
    return(df_regression)
    
    
}

order_of_polynomial <- function(selected_order){
    if(selected_order == "Linear"){
        order <- 1
    }
    else if(selected_order == "2nd Order"){
        order <- 2
    }
    else if(selected_order == "3rd Order"){
        order <- 3
    }
    return(order)
}



R_sq <- function(df_melt, order){
    summary_regression <- summary(lm(value ~ poly(Time,order, raw=TRUE), data=df_melt))
    r_sq <- format(round(summary_regression$r.squared,2)) # R^2 value
    adj_r_sq <- format(round(summary_regression$adj.r.squared,2)) # Adjusted R^2 value

    return(as.numeric(r_sq))

}
adj_R_sq <- function(df_melt, order){
    summary_regression <- summary(lm(value ~ poly(Time,order, raw=TRUE), data=df_melt))
    # r_sq <- format(round(summary_regression$r.squared,2)) # R^2 value
    adj_r_sq <- format(round(summary_regression$adj.r.squared,2)) # Adjusted R^2 value
    
    return(as.numeric(adj_r_sq))
    
}

####################################################################3
####################################################################

best_fit_equation <- function(df_melt, order){
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    return(summary(fit))
}

########################################################################################################################
########################################################################################################################
polynomial_evaluation_of_linearity <- function(df_melt, order){
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    summary_regression <- summary(fit)
    p_values <- summary_regression$coefficients[,4]

    a_pvalue <- p_values[1]
    b_pvalue <- p_values[2]
    c_pvalue <- p_values[3]
    d_pvalue <- p_values[4]
    
    # print(paste('p-value of a: ', a_pvalue))
    # print(paste('p-value of b: ', b_pvalue))
    # print(paste('p-value of c: ', c_pvalue))
    # print(paste('p-value of d: ', d_pvalue))
    
    pvalue_df <- data.frame('a_pvalue'=a_pvalue,
                            'b_pvalue'=b_pvalue,
                            'c_pvalue'=c_pvalue,
                            'd_pvalue'=d_pvalue)

    # print(pvalue_df)
    return(pvalue_df)
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
    
    # print(paste("a: ", a))
    # print(paste("b: ", b))
    # print(paste("c: ", c))
    # print(paste("d: ", d))

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
    
    # print(paste("a_lower: ", a_lower))
    # print(paste("b_lower: ", b_lower))
    # print(paste("c_lower: ", c_lower))
    # print(paste("d_lower: ", d_lower))
    
    f1_lower <- function(x) a_lower + b_lower*x + c_lower*x^2 + d_lower*x^3
    f2_lower <- function(x) threshold_y
    
    # UPDATE: ADDING TRYCATCH STATEMENT TO CATCH getChannel() ERROR
    shelf_life_lower <- tryCatch(uniroot(function(x) f1_lower(x)-f2_lower(x),c(0,5), extendInt="yes")$root, error = function(c){
        cat("Never crosses MFI Threshold - no shelf-life can be found \n")
        shelf_life_lower = NULL
        return(shelf_life_lower)
    })
    # print(shelf_life_lower)
    return(shelf_life_lower)
}

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

residual_vs_fit_plot <- function(df_melt, order){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fit_residuals <- resid(fit)
    se <- sqrt(sum(fit_residuals^2)/(n-2))
    p <- ggplot(df_melt,aes(x=y, y=fit_residuals, label=Time)) + 
        geom_point(size=3, color = '#eb6864') +
        # geom_area(color='blue') +
        geom_hline(aes(yintercept=0)) +
        ylim(0-max(abs(fit_residuals)), 0+max(abs(fit_residuals))) +
        labs(title = 'Residuals vs. Fit Plot',
             x = 'Predicted % of 4C MFI', 
             y = 'Residuals')
    
    return(p)
}

residual_histogram <- function(df_melt, order){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fit_residuals <- resid(fit)
    
    p <- ggplot(df_melt,aes(x=fit_residuals, label=Time)) + 
        geom_histogram(binwidth=sd(fit_residuals), boundary=0, fill = '#eb6864') +
        labs(title = 'Histogram of Residuals',
             # subtitle = "Generally, 95% of residuals should not be larger than 2x the standard deviation of the residuals.",
             x = 'Residuals', 
             y = '# of Residuals')
    
    
    return(p)
}

find_residuals <- function(df_melt, order){
    df_melt <- na.omit(df_melt)
    
    x <- na.omit(df_melt$Time)
    y <- na.omit(df_melt$value)
    n <- length(x)
    fit <- lm(y ~ poly(x,order, raw=TRUE), data=df_melt)
    fit_residuals <- resid(fit)
    
    return(fit_residuals)
}

normal_probability_plot <- function(df_melt, order, residuals){
    
    # And adding line with proper properties
    p <- ggplot(mapping = aes(sample = residuals)) + 
        stat_qq_point(size = 3,color = "#eb6864") + 
        stat_qq_line(color="black") +
        labs(title = 'Normal Probability Plot of Residuals',
             x = 'Theoretical Quantiles', 
             y = 'Residuals') +
        theme(text=element_text(size = 11))
    print(p)
    return(p)
}

read_marker_data <- function(wave_data, sheet="Sheet1"){
    wave_df <- readxl::read_xlsx(wave_data, sheet)
    # print(wave_df)
    marker_info <- paste0(wave_df$`Target Species`,' ', wave_df$Specificity,' ', '(', wave_df$Clone, ')',' ', wave_df$Format)
    # print(marker_info)
    # print(wave_df$`Optimal (ng/test)`[wave_df$Specificity=='Integrin'])
    wave_df <- tibble(cbind(wave_df, 'Marker Description'=marker_info))
    return(wave_df)
}



anderson_darling_normality_test <- function(residuals){
    ad <- ad.test(residuals)
    # print(ad)
    # sqrt(ad$statistic^2)
    p_value <- ad$p.value
    
    # null hypothesis is that data DOES follow normal distribution
    # can reject null hypothesis if p-value < 0.05 --> meaning we can say with sufficient evidence that data does NOT follow normal distribution
    return(p_value)
}


results_summary <- function(data, order, CI){
     df <- tibble(
             '75% Threshold, No CI' = ifelse( typeof(solve_for_shelf_life(data, 75, order)*365) == "double", round(solve_for_shelf_life(data, 75, order)*365,0), "NA"),
             '80% Threshold, No CI' = ifelse( typeof(solve_for_shelf_life(data, 80, order)*365) == "double", round(solve_for_shelf_life(data, 80, order)*365,0), "NA"),
             '75% Threshold, Lwr 95% CI' = ifelse( typeof(solve_for_lower_shelf_life(data, order, CI, 75)*365) == "double", round(solve_for_lower_shelf_life(data, order, CI, 75)*365,0), "NA"),
             '80% Threshold, Lwr 95% CI' = ifelse( typeof(solve_for_lower_shelf_life(data, order, CI, 80)*365) == "double", round(solve_for_lower_shelf_life(data, order, CI, 80)*365,0), "NA"),
             'R-squared' = R_sq(data, order),
             'Adj. R-squared' = adj_R_sq(data, order),
             'Model p-value' = round(ifelse(order == 2, as.numeric(polynomial_evaluation_of_linearity(data, order)$c_pvalue), as.numeric(polynomial_evaluation_of_linearity(data, order)$b_pvalue)),3)
         )
     # print(df)
     return(df)
}

summarize_means <- function(df){

    linear_raw <- df[(df$`Model Order`=="Linear") & (df$`Concentrations Included` == "Raw"),]
    linear_optimal <- df[(df$`Model Order`=="Linear") & (df$`Concentrations Included` == "Optimal +1/-2"),]
    second_raw <- df[(df$`Model Order`=="2nd Order") & (df$`Concentrations Included` == "Raw"),]
    second_optimal <- df[(df$`Model Order`=="2nd Order") & (df$`Concentrations Included` == "Optimal +1/-2"),]
    raw <- df[df$`Concentrations Included` == "Raw",]
    optimal <- df[df$`Concentrations Included` == "Optimal +1/-2",]
    linear <- df[df$`Model Order`=="Linear",]
    second <- df[df$`Model Order`=="2nd Order",]
    
    linear_raw_means <- sapply(4:7, function(i){ mean(linear_raw[[i]]) } )
    linear_optimal_means <- sapply(4:7, function(i){ mean(linear_optimal[[i]]) } )
    second_raw_means <- sapply(4:7, function(i){ mean(second_raw[[i]]) } )
    second_optimal_means <- sapply(4:7, function(i){ mean(second_optimal[[i]]) } )
    raw_means <- sapply(4:7, function(i){ mean(raw[[i]]) } )
    optimal_means <- sapply(4:7, function(i){ mean(optimal[[i]]) } )
    linear_means <- sapply(4:7, function(i){ mean(linear[[i]]) } )
    second_means <- sapply(4:7, function(i){ mean(second[[i]]) } )

    tib <- as_tibble(
        rbind(
            linear_raw_means,
            linear_optimal_means,
            second_raw_means,
            second_optimal_means,
            linear_means,
            second_means,
            raw_means,
            optimal_means
        )
    )
    
    colnames(tib) <- c("75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI")
    
    tib <- tib %>% add_column(
        "Condition" = c(
            "Linear, Raw",
            "Linear, Optimal",
            "2nd Order, Raw",
            "2nd Order, Optimal",
            "Linear",
            "2nd Order",
            "Raw",
            "Optimal"
        ),
        .before = "75% Threshold, No CI"
    )
    return(tib)
}

mfi_vs_concentration_plot <- function(df){
    
    p <- ggplot(df, aes(x=as.factor(Concentration), y=`MFI+`, group=Condition, color=as.factor(Condition))) + 
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

    p <- ggplot(df, aes(x=as.factor(Condition), y=`MFI+`, group=Concentration, color=as.factor(Concentration))) + 
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
    si = (df$`MFI+` - df$`MFI-`)/(2*df$`rSD-`)
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
    sn = df$`MFI+`/df$`MFI-`
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
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`%+`, group=Concentration, color=as.factor(Concentration))) + 
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
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`%+`, group=Concentration, color=as.factor(Concentration))) + 
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

# 1. Put all raw data files in one folder, make sure name of file matches name in summary file
# 2. List all reagents in summary file
# 3. Loop through each raw data file, and calculate shelf-life for each of different combinations, capture R^2 values and p-value of model
# 4. 

# END GOAL: Determine the shelf-life for each combination and if it passes min. requirements
# Determine which combination of criteria give us a) most conservative estimates that still pass, b) most passes, c) best shelf-life estimates that still pass

loop_through_for_summary <- function(data_directory, wave_summary_file, sheet, wave_number, summary_output_file){
    tib <- tibble("Filename"=NA,
                  "Format"=NA,
                  "Min. Shelf-Life (days)"=NA,
                  "Age of 4C ref on test day (days)"=NA,
                  "Concentrations Included"=NA,
                  "Model Order"=NA,
                  "75% Threshold, No CI"=NA,
                  "80% Threshold, No CI"=NA,
                  "75% Threshold, Lwr 95% CI"=NA,
                  "80% Threshold, Lwr 95% CI"=NA
                  # "75%, No CI Pass/Fail"=NA,
                  # "80%, No CI Pass/Fail"=NA,
                  # "75%, 95% CI Pass/Fail"=NA,
                  # "80%, 95% CI Pass/Fail"=NA,
                  # "R-squared Pass/Fail"=NA,
                  # "Adj. R-squared Pass/Fail"=NA,
                  # "Model p-value Pass/Fail"=NA
                  )
    # print(tib)
    wave_summary <- readxl::read_xlsx(wave_summary_file, sheet=sheet)
    # print(wave_summary)
    files_to_analyze <- wave_summary$Filename
    # for(file in unique(files_to_analyze)){
    #     print(file)
    # }
    for(file in unique(files_to_analyze)){
        df <- read_csv(paste0(data_directory, "\\", paste0(file, ".csv")), col_types = cols())

        file <- paste0(file, ".csv")
        melted_df <- melt_reference_mfi_table(df)
        wave_df <- read_marker_data(wave_summary_file, sheet)
        optimal_value <- wave_df$`Optimal (ng/test)`[paste0(wave_df$`Filename`,".csv") == file]
        optimal_df <- melt_reference_mfi_table(concentrations_to_keep(df, concentrations_around_optimal(optimal_value)))
        
        format <- wave_df$`Format`[paste0(wave_df$`Filename`,".csv") == file]
        min_shelf_life <- wave_df$`Min. Shelf-Life (days)`[paste0(wave_df$`Filename`,".csv") == file]
        age_of_ref <- wave_df$`Age of 4C ref on test day (days)`[paste0(wave_df$`Filename`,".csv") == file]

        raw_linear_results <- results_summary(melted_df, 1, 0.95)
        optimal_linear_results <- results_summary(optimal_df, 1, 0.95)
        raw_second_order_results <- results_summary(melted_df, 2, 0.95)
        optimal_second_order_results <- results_summary(optimal_df, 2, 0.95)
        
        summary <- tibble(rbind(raw_linear_results,
                             optimal_linear_results,
                             raw_second_order_results,
                             optimal_second_order_results))
        row_names <- tibble('Filename'=rep(file,4),
                            "Format"=rep(format, 4),
                            "Min. Shelf-Life (days)"=rep(min_shelf_life,4),
                            "Age of 4C ref on test day (days)"=rep(age_of_ref,4),
                            'Concentrations Included'=rep(c('Raw','Optimal +1/-2'),2),
                            'Model Order'=c(rep('Linear',2),c(rep('Second Order',2))))
        tib_new <- bind_cols(row_names, summary)
        tib <- bind_rows(tib, tib_new)
    }
    tib <- tib[2:nrow(tib),]

    V1 <- as_tibble(ifelse(sapply(tibble(rep(NA, length(tib$Filename))), function(i) tib$`75% Threshold, No CI` < tib$`Min. Shelf-Life (days)` | is.na(tib$`75% Threshold, No CI`)),0, 1))
    V2 <- as_tibble(ifelse(sapply(tibble(rep(NA, length(tib$Filename))), function(i) tib$`80% Threshold, No CI` < tib$`Min. Shelf-Life (days)` | is.na(tib$`80% Threshold, No CI`)),0, 1))
    V3 <- as_tibble(ifelse(sapply(tibble(rep(NA, length(tib$Filename))), function(i) tib$`75% Threshold, Lwr 95% CI` < tib$`Min. Shelf-Life (days)` | is.na(tib$`75% Threshold, Lwr 95% CI`)),0, 1))
    V4 <- as_tibble(ifelse(sapply(tibble(rep(NA, length(tib$Filename))), function(i) tib$`80% Threshold, Lwr 95% CI` < tib$`Min. Shelf-Life (days)` | is.na(tib$`80% Threshold, Lwr 95% CI`)),0, 1))

    V5 <- as_tibble(ifelse(sapply(tibble(rep(NA, length(tib$Filename))), function(i) tib$`R-squared` >= 0.80),1, 0))
    V6 <- as_tibble(ifelse(sapply(tibble(rep(NA, length(tib$Filename))), function(i) tib$`Adj. R-squared` >= 0.80),1, 0))
    V7 <- as_tibble(ifelse(sapply(tibble(rep(NA, length(tib$Filename))), function(i) tib$`Model p-value` < 0.05),1, 0))
    tib <- bind_cols(tib,V1, V2, V3, V4, V5, V6, V7)
    names(tib)[14:20] <- c("75%, No CI Pass/Fail", "80%, No CI Pass/Fail", "75%, 95% CI Pass/Fail", "80%, 95% CI Pass/Fail", "R-squared Pass/Fail", "Adj. R-squared Pass/Fail", "Model p-value Pass/Fail")

    write_xlsx(tib, summary_output_file)
    
}


# add_countif_analysis <- function(wave_summary){
#     df <- read_xlsx("Wave3_summary_of_results.xlsx")
#     
#     df$`75%, No CI Pass/Fail` <- c(NA)
#     df$Filename[[1]]
#     df$`75%, No CI Pass/Fail`[[1]]
#     as_tibble(sapply(df$`75%, No CI Pass/Fail`,function(i) ifelse(df$`75% Threshold, No CI`[[i]] < df$`Min. Shelf-Life (days)`[[i]], "DARN", "YAY!!")))
#     df$`75%, No CI Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`75% Threshold, No CI` >= df$`Min. Shelf-Life (days)`),1, 0))
#     df$`80%, No CI Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`80% Threshold, No CI` >= df$`Min. Shelf-Life (days)`),1, 0))
#     df$`75%, 95% CI Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`75% Threshold, Lwr 95% CI` >= df$`Min. Shelf-Life (days)`),1, 0))
#     df$`80%, 95% CI Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`80% Threshold, Lwr 95% CI` >= df$`Min. Shelf-Life (days)`),1, 0))
#     df$`75%, 95% CI Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`75% Threshold, Lwr 95% CI` >= df$`Min. Shelf-Life (days)`),1, 0))
#     
#     df$`R-squared Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`R-squared` >= 0.80),1, 0))
#     df$`Adj. R-squared Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`Adj. R-squared` >= 0.80),1, 0))
#     df$`Model p-value Pass/Fail` <- as_tibble(ifelse(sapply(tibble(rep(NA, length(df$Filename))), function(i) df$`Model p-value` <= 0.05),1, 0))
# 
#     return(df)
# }
# summary(df[df$`Concentrations Included`=='Raw',11:17])

find_optimal_combination <- function(summary_of_results_xlsx, wave_number){
    df <- readxl::read_xlsx(summary_of_results_xlsx)
    subset_summary <- df[,14:20]
    colnames(subset_summary) <- c("75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI", "R-squared", "Adj. R-squared", "Model p-value")

    new_df <- bind_cols(df[,c(1,5:6)], subset_summary)

    initial_tibble <- tibble("Condition"= NA, "75% Threshold, No CI"=NA, "80% Threshold, No CI"=NA, "75% Threshold, Lwr 95% CI"=NA, "80% Threshold, Lwr 95% CI"=NA)
    
    linear <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear",][[i]])
    })
    second_order <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order",][[i]])
    })
    raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Raw",][[i]])
    })
    optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Optimal +1/-2",][[i]])
    })
    linear_raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Raw",][[i]])
    })
    linear_optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Optimal +1/-2",][[i]])
    })
    second_order_raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Raw",][[i]])
    })
    second_order_optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Optimal +1/-2",][[i]])
    })
   
    linear_tib <- tibble("Linear", linear[[1]], linear[[2]], linear[[3]], linear[[4]])
    second_order_tib <- tibble("Second Order", second_order[[1]], second_order[[2]], second_order[[3]], second_order[[4]])
    raw_tib <- tibble("Raw", raw[[1]], raw[[2]], raw[[3]], raw[[4]])
    optimal_tib <- tibble("Optimal +1/-2", optimal[[1]], optimal[[2]], optimal[[3]], optimal[[4]])
    linear_raw_tib <- tibble("Linear, Raw", linear_raw[[1]], linear_raw[[2]], linear_raw[[3]], linear_raw[[4]])
    linear_optimal_tib <- tibble("Linear, Optimal +1/-2", linear_optimal[[1]], linear_optimal[[2]], linear_optimal[[3]], linear_optimal[[4]])
    second_order_raw_tib <- tibble("Second Order, Raw", second_order_raw[[1]], second_order_raw[[2]], second_order_raw[[3]], second_order_raw[[4]])
    second_order_optimal_tib <- tibble("Second Order, Optimal +1/-2", second_order_optimal[[1]], second_order_optimal[[2]], second_order_optimal[[3]], second_order_optimal[[4]])
    dfs <- list(linear_tib, second_order_tib, raw_tib, optimal_tib, linear_raw_tib, linear_optimal_tib, second_order_raw_tib, second_order_optimal_tib)
    
    column_names <- c("Condition", "75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI")
    
    new_tibs <- lapply(dfs, function(x) {
        names(x) <- column_names
        x
    })
   
    mean_summary <- bind_rows(initial_tibble, new_tibs)
    mean_summary <- mean_summary[2:nrow(mean_summary),]
    write_xlsx(mean_summary, paste0(wave_number, "_all_mean_results.xlsx"))

}


find_optimal_combination_p_val <- function(summary_of_results_xlsx, wave_number){
    df <- readxl::read_xlsx(summary_of_results_xlsx)
    subset_summary <- df[,14:20]
    colnames(subset_summary) <- c("75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI", "R-squared", "Adj. R-squared", "Model p-value")
    
    new_df <- bind_cols(df[,c(1,5:6)], subset_summary)
    initial_tibble <- tibble("Condition"= NA, "75% Threshold, No CI"=NA, "80% Threshold, No CI"=NA, "75% Threshold, Lwr 95% CI"=NA, "80% Threshold, Lwr 95% CI"=NA)
    
    linear <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Model p-value` == 1,][[i]])
    })
    second_order <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Model p-value` == 1,][[i]])
    })
    raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1,][[i]])
    })
    optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1,][[i]])
    })
    linear_raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1,][[i]])
    })
    linear_optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1,][[i]])
    })
    second_order_raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1,][[i]])
    })
    second_order_optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1,][[i]])
    })
    
    linear_tib <- tibble("Linear", linear[[1]], linear[[2]], linear[[3]], linear[[4]])
    second_order_tib <- tibble("Second Order", second_order[[1]], second_order[[2]], second_order[[3]], second_order[[4]])
    raw_tib <- tibble("Raw", raw[[1]], raw[[2]], raw[[3]], raw[[4]])
    optimal_tib <- tibble("Optimal +1/-2", optimal[[1]], optimal[[2]], optimal[[3]], optimal[[4]])
    linear_raw_tib <- tibble("Linear, Raw", linear_raw[[1]], linear_raw[[2]], linear_raw[[3]], linear_raw[[4]])
    linear_optimal_tib <- tibble("Linear, Optimal +1/-2", linear_optimal[[1]], linear_optimal[[2]], linear_optimal[[3]], linear_optimal[[4]])
    second_order_raw_tib <- tibble("Second Order, Raw", second_order_raw[[1]], second_order_raw[[2]], second_order_raw[[3]], second_order_raw[[4]])
    second_order_optimal_tib <- tibble("Second Order, Optimal +1/-2", second_order_optimal[[1]], second_order_optimal[[2]], second_order_optimal[[3]], second_order_optimal[[4]])
    
    dfs <- list(linear_tib, second_order_tib, raw_tib, optimal_tib, linear_raw_tib, linear_optimal_tib, second_order_raw_tib, second_order_optimal_tib)
    column_names <- c("Condition", "75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI")
    
    new_tibs <- lapply(dfs, function(x) {
        names(x) <- column_names
        x
    })

    mean_summary <- bind_rows(initial_tibble, new_tibs)
    mean_summary <- mean_summary[2:nrow(mean_summary),]
    write_xlsx(mean_summary, paste0(wave_number, "_p_val_mean_results.xlsx"))
    
}

##################################################################################################3

find_optimal_combination_p_val_r_sqd <- function(summary_of_results_xlsx, wave_number){
    df <- readxl::read_xlsx(summary_of_results_xlsx)
    subset_summary <- df[,14:20]
    colnames(subset_summary) <- c("75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI", "R-squared", "Adj. R-squared", "Model p-value")
    
    new_df <- bind_cols(df[,c(1,5:6)], subset_summary)
    initial_tibble <- tibble("Condition"= NA, "75% Threshold, No CI"=NA, "80% Threshold, No CI"=NA, "75% Threshold, Lwr 95% CI"=NA, "80% Threshold, Lwr 95% CI"=NA)
    
    linear <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    second_order <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    linear_raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    linear_optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    second_order_raw <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    second_order_optimal <- sapply(4:7, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1 & new_df$`R-squared` == 1,][[i]])
    })
    

    linear_tib <- tibble("Linear", linear[[1]], linear[[2]], linear[[3]], linear[[4]])
    second_order_tib <- tibble("Second Order", second_order[[1]], second_order[[2]], second_order[[3]], second_order[[4]])
    raw_tib <- tibble("Raw", raw[[1]], raw[[2]], raw[[3]], raw[[4]])
    optimal_tib <- tibble("Optimal +1/-2", optimal[[1]], optimal[[2]], optimal[[3]], optimal[[4]])
    linear_raw_tib <- tibble("Linear, Raw", linear_raw[[1]], linear_raw[[2]], linear_raw[[3]], linear_raw[[4]])
    linear_optimal_tib <- tibble("Linear, Optimal +1/-2", linear_optimal[[1]], linear_optimal[[2]], linear_optimal[[3]], linear_optimal[[4]])
    second_order_raw_tib <- tibble("Second Order, Raw", second_order_raw[[1]], second_order_raw[[2]], second_order_raw[[3]], second_order_raw[[4]])
    second_order_optimal_tib <- tibble("Second Order, Optimal +1/-2", second_order_optimal[[1]], second_order_optimal[[2]], second_order_optimal[[3]], second_order_optimal[[4]])
    
    dfs <- list(linear_tib, second_order_tib, raw_tib, optimal_tib, linear_raw_tib, linear_optimal_tib, second_order_raw_tib, second_order_optimal_tib)
    column_names <- c("Condition", "75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI")
    
    new_tibs <- lapply(dfs, function(x) {
        names(x) <- column_names
        x
    })
    
    mean_summary <- bind_rows(initial_tibble, new_tibs)
    mean_summary <- mean_summary[2:nrow(mean_summary),]
    write_xlsx(mean_summary, paste0(wave_number, "_p_val_r_sqd_mean_results.xlsx"))
    
}



find_optimal_combination_high_r_sqd <- function(summary_of_results_xlsx, wave_number){
    df <- readxl::read_xlsx(summary_of_results_xlsx)
    subset_summary <- df[,14:20]
    colnames(subset_summary) <- c("75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI", "R-squared", "Adj. R-squared", "Model p-value")
    
    new_df <- bind_cols(df[,c(1,5:6)], subset_summary)
    initial_tibble <- tibble("Condition"= NA, "75% Threshold, No CI"=NA, "80% Threshold, No CI"=NA, "75% Threshold, Lwr 95% CI"=NA, "80% Threshold, Lwr 95% CI"=NA)
    
    linear <- sapply(8:9, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Model p-value` == 1,][[i]])
    })
    second_order <- sapply(8:9, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Model p-value` == 1,][[i]])
    })
    raw <- sapply(8:9, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1,][[i]])
    })
    optimal <- sapply(8:9, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1,][[i]])
    })
    linear_raw <- sapply(8:9, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1,][[i]])
    })
    linear_optimal <- sapply(8:9, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1,][[i]])
    })
    second_order_raw <- sapply(8:9, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Raw" & new_df$`Model p-value` == 1,][[i]])
    })
    second_order_optimal <- sapply(8:9, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Optimal +1/-2" & new_df$`Model p-value` == 1,][[i]])
    })
   
    
    
    linear_tib <- tibble("Linear", linear[[1]], linear[[2]])
    second_order_tib <- tibble("Second Order", second_order[[1]], second_order[[2]])
    raw_tib <- tibble("Raw", raw[[1]], raw[[2]])
    optimal_tib <- tibble("Optimal +1/-2", optimal[[1]], optimal[[2]])
    linear_raw_tib <- tibble("Linear, Raw", linear_raw[[1]], linear_raw[[2]])
    linear_optimal_tib <- tibble("Linear, Optimal +1/-2", linear_optimal[[1]], linear_optimal[[2]])
    second_order_raw_tib <- tibble("Second Order, Raw", second_order_raw[[1]], second_order_raw[[2]])
    second_order_optimal_tib <- tibble("Second Order, Optimal +1/-2", second_order_optimal[[1]], second_order_optimal[[2]])
    
    dfs <- list(linear_tib, second_order_tib, raw_tib, optimal_tib, linear_raw_tib, linear_optimal_tib, second_order_raw_tib, second_order_optimal_tib)
    column_names <- c("Condition", "R-Squared", "Adj. R-squared")
    
    new_tibs <- lapply(dfs, function(x) {
        names(x) <- column_names
        x
    })
    
    mean_summary <- bind_rows(initial_tibble, new_tibs)
    mean_summary <- mean_summary[2:nrow(mean_summary),]
    write_xlsx(mean_summary, paste0(wave_number, "_high_r_sqd_mean_results.xlsx"))
    
}

find_times_with_valid_pvalue <- function(summary_of_results_xlsx, wave_number){
    df <- readxl::read_xlsx(summary_of_results_xlsx)
    subset_summary <- df[,14:20]
    colnames(subset_summary) <- c("75% Threshold, No CI", "80% Threshold, No CI", "75% Threshold, Lwr 95% CI", "80% Threshold, Lwr 95% CI", "R-squared", "Adj. R-squared", "Model p-value")
    
    new_df <- bind_cols(df[,c(1,5:6)], subset_summary)
    initial_tibble <- tibble("Condition"= NA, "75% Threshold, No CI"=NA, "80% Threshold, No CI"=NA, "75% Threshold, Lwr 95% CI"=NA, "80% Threshold, Lwr 95% CI"=NA)
    
    linear <- sapply(10, function(i){
        mean(new_df[new_df$`Model Order`=="Linear",][[i]])
    })
    second_order <- sapply(10, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order",][[i]])
    })
    raw <- sapply(10, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Raw",][[i]])
    })
    optimal <- sapply(10, function(i){
        mean(new_df[new_df$`Concentrations Included`=="Optimal +1/-2",][[i]])
    })
    linear_raw <- sapply(10, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Raw",][[i]])
    })
    linear_optimal <- sapply(10, function(i){
        mean(new_df[new_df$`Model Order`=="Linear" & new_df$`Concentrations Included`=="Optimal +1/-2",][[i]])
    })
    second_order_raw <- sapply(10, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Raw",][[i]])
    })
    second_order_optimal <- sapply(10, function(i){
        mean(new_df[new_df$`Model Order`=="Second Order" & new_df$`Concentrations Included`=="Optimal +1/-2",][[i]])
    })
    
    
    
    linear_tib <- tibble("Linear", linear[[1]])
    second_order_tib <- tibble("Second Order", second_order[[1]])
    raw_tib <- tibble("Raw", raw[[1]])
    optimal_tib <- tibble("Optimal +1/-2", optimal[[1]])
    linear_raw_tib <- tibble("Linear, Raw", linear_raw[[1]])
    linear_optimal_tib <- tibble("Linear, Optimal +1/-2", linear_optimal[[1]])
    second_order_raw_tib <- tibble("Second Order, Raw", second_order_raw[[1]])
    second_order_optimal_tib <- tibble("Second Order, Optimal +1/-2", second_order_optimal[[1]])
    
    dfs <- list(linear_tib, second_order_tib, raw_tib, optimal_tib, linear_raw_tib, linear_optimal_tib, second_order_raw_tib, second_order_optimal_tib)
    column_names <- c("Condition", "Model p-value")
    
    new_tibs <- lapply(dfs, function(x) {
        names(x) <- column_names
        x
    })
    
    mean_summary <- bind_rows(initial_tibble, new_tibs)
    mean_summary <- mean_summary[2:nrow(mean_summary),]
    write_xlsx(mean_summary, paste0(wave_number, "_valid_p_value_results.xlsx"))
    
}

wave_overview_file <- "allwaves_summary.xlsx"
summary_output_file <- "allwaves_summary_of_results.xlsx"
summary_xlsx_file <- summary_output_file
wave_number <- "AllWaves"
# loop_through_for_summary("C:\\Users\\10294643\\Desktop\\Rotation 2 - San Diego\\Stability\\Wave 3\\MFI Data - fixed", wave_overview_file, "All", wave_number, summary_output_file)
# 
# # # list.files("C:\\Users\\10294643\\Desktop\\Rotation 2 - San Diego\\Stability\\Wave 5\\MFI Data", "Wave5")
# find_optimal_combination(summary_xlsx_file, wave_number)
# find_optimal_combination_p_val(summary_xlsx_file, wave_number)
# find_optimal_combination_p_val_r_sqd(summary_xlsx_file, wave_number)
# find_optimal_combination_high_r_sqd(summary_xlsx_file, wave_number)
# find_times_with_valid_pvalue(summary_xlsx_file, wave_number)
