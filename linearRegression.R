library(ggplot2)
library(reshape2)

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
df_from_GUI = read.csv('stability_stats.csv')




fullDataTable <- function(df_from_GUI, cols_to_avg) {
    
    df_csv <- df_from_GUI
    
    
    averagesMatrix <- c()
    
    for (i in cols_to_avg) {
        print(i)
        averagesMatrix <- cbind(averagesMatrix, df_csv[[as.numeric(i)]])
    }
    print(averagesMatrix)
    
    df_full <- cbind(df_csv, "Average"=rowMeans(averagesMatrix))
    
    return (df_full)
}
# 
# df_csv <- df_from_GUI


# 
# df_full <- cbind(df_csv, "Average"=rowMeans(df_csv[2:ncol(df_csv)]))
# 
# 
# df_regression <- data.frame("Time"=df_full$Time,"Average"=df_full$Average)
# df_regression

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

# regressionData <- regressionDataTable(fullDataTable(df_from_GUI))


summarizeData <- function(df_regression, threshold_y){
    # Summary Data
    fit <- lm(Average ~ Time, data=df_regression)
    summary_regression <- summary(fit)
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



createPlot <- function(dataMelt, df_regression, CI_level){
    outputPlot <- ggplot(dataMelt, aes(x=Time, y=value, color=Concentrations)) +
        
        
        # Line plot based on average of certain columns
        geom_smooth(data=df_regression, aes(x=Time, y=Average), formula = y ~ x, method="lm", col="blue", level=CI_level) +
        geom_point(size=4) +
        labs(title = "Regression for Stability",
             x = "Time (y)",
             y = "% of 4C Reference MFI") +
        theme_minimal() +
        scale_color_brewer(palette = 'Blues', labels = c(
            "30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test', 'Average'
        )) 
    
    return (outputPlot)
}
# createPlot(dotPlotData, regressionData, 0.99)

                