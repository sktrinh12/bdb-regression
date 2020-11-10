fullDataTable <- function(df_from_GUI) {
    
    df_csv <- df_from_GUI
    print(df_csv)
    
    # averagesMatrix <- c()
    # 
    # for (i in cols_to_avg) {
    #     print(i)
    #     averagesMatrix <- cbind(averagesMatrix, df_csv[[as.numeric(i)]])
    # }
    # print(averagesMatrix)
    
    df_full <- cbind(df_csv, "Average"=rowMeans(cbind(df_csv[[2]], df_csv[[3]], df_csv[[4]], df_csv[[5]], df_csv[[6]], df_csv[[7]], df_csv[[8]])))
    
    return (df_full)
}
df_full <- fullDataTable(read.csv('stability_stats.csv'))


meltedDataTable <- function(df_full){
    
    # Melt Columns by Time
    dataMelt <- melt(df_full, "Time", variable='Concentrations')
    
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
    
    
    df_regression <- data.frame("Time"=df_full$Time,"Average"=df_full$Average)
    
    return(df_regression)
    
    
}

df_regression <- regressionDataTable(fullDataTable(read.csv('stability_stats.csv')))

reg.conf.intervals <- function(x, y, CI, threshold_y) {
    n <- length(y) # Find length of y to use as sample size
    print(n)
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
    x_new <- 0:max(x)
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
    
    return(bands)
}

conf.intervals <- reg.conf.intervals(df_regression$Time, df_regression$Average, 0.95, 75)

