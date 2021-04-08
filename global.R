library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(readr)
library(ggpubr)
library(readxl)
library(hash)

# order <- 2

template_data <- data.frame('Time'=c(0,0.5,1,1.5,2,3,4,5), 
                            'Conc_30_ng'=rep(NA, 8), 
                            'Conc_60_ng'=rep(NA, 8),
                            'Conc_125_ng'=rep(NA, 8),
                            'Conc_250_ng'=rep(NA, 8),
                            'Conc_500_ng'=rep(NA, 8),
                            'Conc_1000_ng'=rep(NA, 8),
                            'Conc_2000_ng'=rep(NA, 8),
                            row.names=NULL
)


conc_to_exclude <- function(df_from_GUI, cols_to_avg){
    df_csv <- df_from_GUI
    
    keep_matrix_conc <- c()
    list_cols <- c()
    for (i in cols_to_avg) {
        keep_matrix_conc <- cbind(keep_matrix_conc, df_csv[[as.numeric(i)]])
        list_cols <- c(list_cols, as.numeric(i))
    }
    
    if(is.null(df_csv)){
        df_selected <- c()
    }
    else{
        df_selected <- dplyr::select(df_csv, c(list_cols))
    }

    keep_conc <- cbind('Time'=df_csv$Time, df_selected)

    

    return (keep_conc)
}

concentrations_around_optimal <- function(optimal){
    key <- c('30 ng/test',
             '60 ng/test',
             '125 ng/test',
             '250 ng/test',
             '500 ng/test',
             '1000 ng/test',
             '2000 ng/test')
    
    value <- c(2:8)
    
    h <- hash(key,value)
    optimal_key <- as.character(paste0(optimal, ' ng/test'))
    optimal_value <- as.numeric(values(h)[optimal_key])
    
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

meltedDataTable <- function(df_full=template_data){
    
    # Melt Columns by Time
    dataMelt <- melt(df_full, "Time", variable='Concentrations')

    dataMelt <- cbind(dataMelt, 'Labels'=paste0(parse_number(as.character(dataMelt$Concentrations)), ' ng/test'))
    # print('dataMelt: ')

    
    return (dataMelt)
}
# dotPlotData <- meltedDataTable(fullDataTable(df_from_GUI))


regressionDataTable <- function(df_full) {
    
    ############ User Inputs ############
    
    # COnfidence Interval
    CI_level <- 0.95
    
    # Threshold % of 4C MFI value to determine shelf-life
    threshold_y = 75
    
    # df_csv <- df_from_GUI
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
    
    print(paste('p-value of a: ', a_pvalue))
    print(paste('p-value of b: ', b_pvalue))
    print(paste('p-value of c: ', c_pvalue))
    print(paste('p-value of d: ', d_pvalue))
    
    pvalue_df <- data.frame('a_pvalue'=a_pvalue,
                            'b_pvalue'=b_pvalue,
                            'c_pvalue'=c_pvalue,
                            'd_pvalue'=d_pvalue)

    print(pvalue_df)
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
    
    print(paste("a: ", a))
    print(paste("b: ", b))
    print(paste("c: ", c))
    print(paste("d: ", d))

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
    
    print(paste("a_lower: ", a_lower))
    print(paste("b_lower: ", b_lower))
    print(paste("c_lower: ", c_lower))
    print(paste("d_lower: ", d_lower))
    
    f1_lower <- function(x) a_lower + b_lower*x + c_lower*x^2 + d_lower*x^3
    f2_lower <- function(x) threshold_y
    
    # UPDATE: ADDING TRYCATCH STATEMENT TO CATCH getChannel() ERROR
    shelf_life_lower <- tryCatch(uniroot(function(x) f1_lower(x)-f2_lower(x),c(0,5), extendInt="yes")$root, error = function(c){
        cat("Never crosses MFI Threshold - no shelf-life can be found \n")
        shelf_life_lower = NULL
        return(shelf_life_lower)
    })
    print(shelf_life_lower)
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
             subtitle = "Check for lack of patterns in residuals. If any apparent patterns are visible in residual scatterplot, model requires adjustments.",
             x = 'Predicted % of 4C MFI', 
             y = 'Residuals')
    
    return(ggplotly(p))
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
             subtitle = "Generally, 95% of residuals should not be larger than 2x the standard deviation of the residuals.",
             x = 'Residuals', 
             y = '# of Residuals')
    
    
    return(ggplotly(p))
}

read_marker_data <- function(wave_data){
    wave_df <- read_xlsx(wave_data)
    # print(wave_df)
    marker_info <- paste0(wave_df$`Target Species`,' ', wave_df$Specificity,' ', '(', wave_df$Clone, ')',' ', wave_df$Format)
    # print(marker_info)
    # print(wave_df$`Optimal (ng/test)`[wave_df$Specificity=='Integrin'])
    wave_df <- tibble(cbind(wave_df, 'Marker Description'=marker_info))
    return(wave_df)
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
     print(df)
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
    print(dim(df))
    return(tib)
}

mfi_vs_concentration_plot <- function(df){
    
    p <- ggplot(df, aes(x=as.factor(Concentration), y=`MFI+`, group=Condition, color=as.factor(Condition))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Set2", labels=unique(paste0(df$Condition, " years"))) +
        labs(title = 'MFI vs Concentration',
             x = 'Concentration (ng/test)', 
             y = 'MFI',
             color = "Time (years)")+
        theme(text=element_text(size = 16))
    
    return(p)
}

mfi_vs_time_plot <- function(df){
    print(df)
    p <- ggplot(df, aes(x=as.factor(Condition), y=`MFI+`, group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Set2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = 'MFI vs Time',
             x = 'Time (years)', 
             y = 'MFI',
             color = "Concentration") +
        theme(text=element_text(size = 16))
    
    return(p)
}

stain_index <- function(df){
    si = (df$`MFI+` - df$`MFI-`)/(2*df$`rSD-`)
    df <- cbind(df, "Stain Index"=si)
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`Stain Index`, group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Set2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = 'Stain Index',
             x = 'Time (years)', 
             y = 'Stain Index',
             color = "Concentration") +
        theme(text=element_text(size = 16)) +
        ylim(0,NA)
    
    return(p)
}

signal_to_noise <- function(df){
    sn = df$`MFI+`/df$`MFI-`
    df <- cbind(df, "Signal-to-Noise"=sn)
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`Signal-to-Noise`, group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Set2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = 'Signal-to-Noise',
             x = 'Time (years)', 
             y = 'Signal-to-Noise',
             color = "Concentration") +
        theme(text=element_text(size = 16)) +
        ylim(0,NA)
    
    return(p)
}

percent_positive <- function(df){
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`%+`, group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Set2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = '(%) Positive',
             x = 'Time (years)', 
             y = '(%) Positive',
             color = "Concentration") +
        theme(text=element_text(size = 16)) +
        ylim(0,NA)
    
    return(p)
    
}
 
percent_of_4C_MFI <- function(df){
    
    p <- ggplot(df, aes(x=as.factor(Condition), y=`%+`, group=Concentration, color=as.factor(Concentration))) + 
        geom_point(size=4) + 
        geom_line(size=1) + 
        scale_colour_brewer(palette="Set2", labels=unique(paste0(df$Concentration, " ng/test"))) +
        labs(title = '(%) Positive',
             x = 'Time (years)', 
             y = '(%) Positive',
             color = "Concentration") +
        theme(text=element_text(size = 16)) +
        ylim(0,NA)
    
    return(p)
}

# 1. Put all raw data files in one folder, make sure name of file matches name in summary file
# 2. List all reagents in summary file
# 3. Loop through each raw data file, and calculate shelf-life for each of different combinations, capture R^2 values and p-value of model
# 4. 

# END GOAL: Determine the shelf-life for each combination and if it passes min. requirements
# Determine which combination of criteria give us a) most conservative estimates that still pass, b) most passes, c) best shelf-life estimates that still pass

loop_through_for_summary <- function(data_directory){
    
    for(file in list.files(data_directory)){
        df <- read_csv(paste0(data_directory, "\\", file))
        print(df)
        
    }
    
}
# loop_through_for_summary("C:\\Users\\10294643\\Desktop\\Rotation 2 - San Diego\\Stability\\Wave 5\\MFI Data")
# list.files("C:\\Users\\10294643\\Desktop\\Rotation 2 - San Diego\\Stability\\Wave 5\\MFI Data")
