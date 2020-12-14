library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(readr)


############ User Inputs ############

# COnfidence Interval
# CI_level <- 0.95

# Threshold % of 4C MFI value to determine shelf-life
# threshold_y = 75

# Concentrations to Average
# 
# rowsToAverage <- cbind(df_csv[])
# 
# for () {
#     
# }
#     
# averagedRows <- rowMeans(df_csv[col1:col2])
# df_from_GUI = read.csv('stability_stats.csv')

# labels_column <- function(df_from_GUI){
#     df_csv <- df_from_GUI
#     labels_list <- colnames(df_csv)
#     print(labels_list)
#     return (labels_list)
# }


fullDataTable <- function(df_from_GUI, cols_to_avg) {
    
    df_csv <- df_from_GUI

    averagesMatrix <- c()
    list_cols <- c()
    for (i in cols_to_avg) {
        averagesMatrix <- cbind(averagesMatrix, df_csv[[as.numeric(i)]])
        list_cols <- c(list_cols, as.numeric(i))
    }

    if(is.null(df_csv)){
        df_selected <- c()
    }
    else{
        df_selected <- dplyr::select(df_csv, c(list_cols))
    }
    df_full <- cbind('Time'=df_csv$Time, df_selected, "Average"=rowMeans(averagesMatrix, na.rm=TRUE))

    
    keep_conc <- melted_data_table()[ vals$keeprows, , drop = FALSE]
    
    return (df_full)
}

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
    print(df_selected)
    
    keep_conc <- cbind('Time'=df_csv$Time, df_selected)
    print('keep_conc: ')
    print(keep_conc)
    return (keep_conc)
}


meltedDataTable <- function(df_full=rep(NA, 64)){
    
    # Melt Columns by Time
    dataMelt <- melt(df_full, "Time", variable='Concentrations')

    dataMelt <- cbind(dataMelt, 'Labels'=paste0(parse_number(as.character(dataMelt$Concentrations)), ' ng/test'))
    print('dataMelt: ')
    print(dataMelt)

    
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
    print('df_regression: ')
    print(df_regression)
    return(df_regression)
    
    
}
# summarizeAllData <- function(df_full, threshold_y){
#     fit <
# }
# regressionData <- regressionDataTable(fullDataTable(df_from_GUI))


summarizeData <- function(df_melt, threshold_y){
    # Summary Data
    fit <- lm(value ~ Time, data=df_melt)
    print(fit)
    summary_regression <- summary(fit)
    print(summary_regression)
    m <- as.numeric(format(round(summary_regression$coefficients[[2]],2))) # Slope
    b <- as.numeric(format(round(summary_regression$coefficients[[1]],2))) # y-intercept
    r_sq <- format(round(summary_regression$r.squared,2)) # Adjusted R^2 value
    
    lm_eqn <- paste('y = ',m,'x + ',b)
    # paste('R-sq = ', r_sq)
    # expression(R^2 == 0.85)
    
    
    # y = mx + b
    shelf_life <- (threshold_y - b) / m 
    
    return (round(shelf_life,2))
}

reg_conf_intervals <- function(x, y, CI, threshold_y) {
    y <- na.omit(y)
    x <- na.omit(x)
    n <- length(y) # Find length of y to use as sample size
    print(n)
    print(data.frame('Time'=x,'value'=y))
    lm.model <- lm(y ~ x) # Fit linear model
    print(lm.model)
    
    # Extract fitted coefficients from model object
    b <- lm.model$coefficients[1]
    m <- lm.model$coefficients[2]
    
    # Find SSE and MSE
    sse <- sum((y - lm.model$fitted.values)^2)
    mse <- sse / (n - 2)
    t.val <- qt(CI, n - 2) # Calculate critical t-value
    
    # Fit linear model with extracted coefficients
    x_new <- 0:max(x, na.rm=TRUE)
    y.fit <- m * x + b
    print(data.frame(x, y.fit))
    plot(x, y, xlim=c(0,5), ylim =c(0,100))
    lines(y.fit ~ x)
    
    
    # Find the standard error of the regression line
    se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
    
    # Fit a new linear model that extends past the given data points (for plotting)
    x_new2 <- 0:max(x)
    y.fit2 <- m * x_new2 + b
    print(data.frame(x_new2, y.fit2))
    
    # Warnings of mismatched lengths are suppressed
    slope.upper <- suppressWarnings(y.fit + t.val * se)
    slope.lower <- suppressWarnings(y.fit - t.val * se)
    
    lm_lower.model <- lm(slope.lower ~ x)
    # Extract fitted coefficients from model object
    b_lower <- lm_lower.model$coefficients[1]
    m_lower <- lm_lower.model$coefficients[2]
    
    print(y.fit - t.val * se)
    print(length(slope.lower))
    print(m_lower)
    print(b_lower)
    print(lm_lower.model)
    shelf_life <- (threshold_y - b) / m
    shelf_life_lower <- (threshold_y - b_lower) / m_lower
    print(paste("shelf-life: ", shelf_life))
    print(paste("shelf-life of lower: ", shelf_life_lower))
    
    
    # Collect the computed confidence bands into a data.frame and name the columns
    bands <- data.frame(cbind(x, slope.lower, y.fit, slope.upper))
    colnames(bands) <- c('X Values', 'Lower Confidence Band', 'Y Values', 'Upper Confidence Band')
    print(bands)
    
    # Plot the fitted linear regression line and the computed confidence bands
    plot(x, y, cex = 1.75, pch = 21, bg = 'gray', xlim=c(0,5), ylim =c(60,100))
    lines(y.fit ~ x, col = 'black', lwd = 2)
    lines(bands[[2]] ~ bands[[1]], col = 'blue', lty = 2, lwd = 2)
    lines(bands[[4]] ~ bands[[1]], col = 'blue', lty = 2, lwd = 2)
    
    return(round(shelf_life_lower, 2))
}



createPlot <- function(dataMelt, df_regression, CI_level){
    outputPlot <- ggplot(dataMelt, aes(x=Time, y=value, color=Concentrations)) +
        
        
        # Line plot based on average of certain columns
        geom_smooth(data=df_regression, aes(x=Time, y=Average), formula = y ~ x, method="lm", col="red", level=CI_level) +
        geom_point(size=4) +
        labs(x = "Time (y)",
             y = "% of 4C Reference MFI") +
        theme_minimal() +
        scale_color_brewer(palette = 'Reds', labels = c(
            "30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test', 'Average'
        )) 
    p <- ggplotly(outputPlot)
    return (outputPlot)
}
# createPlot(dotPlotData, regressionData, 0.99)

