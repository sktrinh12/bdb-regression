library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)
library(readr)
library(ggpubr)

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


meltedDataTable <- function(df_full=template_data){
    
    # Melt Columns by Time
    dataMelt <- melt(df_full, "Time", variable='Concentrations')

    dataMelt <- cbind(dataMelt, 'Labels'=paste0(parse_number(as.character(dataMelt$Concentrations)), ' ng/test'))
    # print('dataMelt: ')
    # print(dataMelt)

    
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

####################################################################3
####################################################################

best_fit_equation <- function(df_melt, order){
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    print(fit)
    return(summary(fit))
}
# 
# r_sq <- function(best_fit_eqn){
#     round(summary(best_fit_eqn)$r.squared,2)
# }
# adj_r_sq <- function(best_fit_eqn){
#     round(summary(best_fit_eqn)$adj.r.squared,2)
# }
# coeff <- function(best_fit_eqn, order){
#     a <- round(as.numeric(summary(best_fit_eqn)$coefficients[[1]]),2) # y-intercept
#     b <- round(as.numeric(summary(best_fit_eqn)$coefficients[[2]]),2) # 1st order coeff
#     c <- ifelse(order > 1, round(as.numeric(summary(best_fit_eqn)$coefficients[[3]]),2), 0) # 2nd order coeff
#     d <- ifelse(order > 2, round(as.numeric(summary(best_fit_eqn)$coefficients[[4]]),2), 0) # 3rd order coeff
#     coeff_df <- data.frame('variable'=c('a','b','c','d'),'coeff'=c(a,b,c,d))
#     print(coeff_df)
#     return(coeff_df)
# }

########################################################################################################################
########################################################################################################################
polynomial_evaluation_of_linearity <- function(df_melt, order){
    print('are we in?')
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    print('what about here?')
    summary_regression <- summary(fit)
    p_values <- summary_regression$coefficients[,4]
    print('--------P-vALUES-----------')
    print(p_values)
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
    # if(order == 2){
    #     if(c_pvalue > 0.05){
    #         return('2nd Order Statistically Significant! Use 2nd Order Model')
    #     }
    #     else{
    #         return('2nd Order is not statisticially significant...use linear model')
    #     }
    # }
    # if(order == 3){
    #     if(d_pvalue < 0.05){
    #         return('3rd Order Statistically Significant! Use 3rd Order Model')
    #     }
    #     else{
    #         return('3rd Order not statistically significant...try second order or linear model')
    #     }
    # }
    return(pvalue_df)
}

solve_for_shelf_life <- function(df_melt, threshold_y, order){
    
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    summary_regression <- summary(fit)
    
    a <- round(as.numeric(summary_regression$coefficients[[1]]),2) # y-intercept
    b <- round(as.numeric(summary_regression$coefficients[[2]]),2) # 1st order coeff
    c <- ifelse(order > 1, round(as.numeric(summary_regression$coefficients[[3]]),2), 0) # 2nd order coeff
    d <- ifelse(order > 2, round(as.numeric(summary_regression$coefficients[[4]]),2), 0) # 3rd order coeff
    
    r_sq <- format(round(summary_regression$r.squared,2)) # Adjusted R^2 value
    
    # Find intersect of two functions to determine shelf-life
    f1 <- function(x) a + b*x + c*x^2 + d*x^3
    f2 <- function(x) threshold_y
    
    print('-----------------ERROR FOR SHELF LIFE??------------')
    shelf_life <- tryCatch(uniroot(function(x) f1(x)-f2(x),c(0,5), extendInt="yes")$root, error = function(c){
        cat("Never crosses MFI Threshold - no shelf-life can be found \n")
        shelf_life = NULL
        return(shelf_life)
    })
    
    return(shelf_life)
}

solve_for_lower_shelf_life <- function(df_melt, order, CI, threshold_y){
    # fit <- lm(value ~ poly(Time,order, raw=TRUE), data = df_melt)
    # print(fm)
    # plot(cars, xlab = "Speed", ylab = "Distance")
    # # spd <- seq(min(Time)-1, max(Time)+1, length.out = length(df_melt$))
    # print(spd)
    # writexl::write_xlsx(data.frame(spd),'cars3.xlsx')
    # print(fm)
    # nD <- data.frame(speed = spd)
    # pfm <- predict(fm, nD)
    # lines(spd, pfm)
    # pf2 <- predict(update(fm, ~ stats::poly(speed, 2, raw=TRUE)), nD, interval = 'confidence', level=0.95, se.fit=TRUE)
    # lines(spd,pf2[,2], col='red')
    # lines(spd,pf2[,3], col='red')
    # print(pf2)
    # df <- data.frame('fit'=pf2$fit[,1], 'lwr'=pf2$fit[,2], 'upr'=pf2$fit[,3], 'se_fit'=pf2$se.fit)
    # df
    # writexl::write_xlsx(df, 'cars5.xlsx')
    # writexl::write_xlsx(cars, 'cars2.xlsx')
    # fm_lwr <- lm(pf2[,2] ~ poly(spd, 2), data=cars)
    # fm_lwr
    # summary(fm_lwr)
    
    y <- na.omit(df_melt$value)
    x <- na.omit(df_melt$Time)

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
    
    # Find SSE and MSE
    # sse <- sum((y - fit$fitted.values)^2)
    # mse <- sse / (n - 2)
    # 
    # alpha <- 1 - CI
    # t.val <- qt(1-alpha/2, fit$df.residual) # Calculate critical t-value
    # print(paste('t.val: ', t.val))
    
    # Fit linear model with extracted coefficients
    # x_new <- 0:max(x, na.rm=TRUE)
    # y.fit <- a + b*x_new + c*x_new^2 + d*x_new^3
    # 
    # # Find the standard error of the regression line
    # se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
    
    # Fit a new linear model that extends past the given data points (for plotting)
    # x_new2 <- 0:max(x)
    # y.fit2 <- a + b*x_new2 + c*x_new2^2 + d*x_new2^3
    
    # Warnings of mismatched lengths are suppressed
    # slope.upper <- suppressWarnings(y.fit + t.val * se)
    # slope.lower <- suppressWarnings(y.fit - t.val * se)
    x_new <- seq(min(x), max(x), length.out = length(x))
    y_fit <- a + b*x_new + c*x_new^2 + d*x_new^3

    ################################
    # fm <- lm(weight ~ poly(height, 2), data = women)
    # plot(women, xlab = "Height (in)", ylab = "Weight (lb)")
    # ht <- seq(57, 73, length.out = 200)
    # nD <- data.frame(height = ht)
    # pfm <- predict(fm, nD)
    # lines(ht, pfm)
    # pf2 <- predict(update(fm, ~ stats::poly(height, 2)), nD)


    # pre_predict <- predict(fit, data.frame(x_new))
    pre_predict <- predict(fit, data.frame('x_new'=x_new), interval='confidence',level=CI, se.fit=TRUE)
    print('----------- PRE_PREDICT ----------')
    pre_predict_df <- data.frame('x_new'=x_new, 'fit'=y_fit, 'lwr'=pre_predict$fit[,2], 'upr'=pre_predict$fit[,3])

    # conf_df <- predict(fit, data.frame('x_new'=x_new), interval='confidence',level=CI, se.fit=TRUE)
    # predict_df <- data.frame('Time_new'=x_new, 'value_fit'=conf_df$fit[,1], 'lwr'=conf_df$fit[,2], 'upr'=conf_df$fit[,3], 'se_fit'=conf_df$se.fit)
    slope.upper <- pre_predict_df$upr
    slope.lower <- pre_predict_df$lwr

    fit_lower <- lm(slope.lower ~ poly(x,order, raw=TRUE))
    # print(fit_lower)
    summary_regression_lower <- summary(fit_lower)
    # print(summary_regression_lower)
    
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
    
    print('-----------------ERROR FOR LOWER BOUND??------------')
    # UPDATE: ADDING TRYCATCH STATEMENT TO CATCH getChannel() ERROR
    shelf_life_lower <- tryCatch(uniroot(function(x) f1_lower(x)-f2_lower(x),c(0,5), extendInt="yes")$root, error = function(c){
        cat("Never crosses MFI Threshold - no shelf-life can be found \n")
        shelf_life_lower = NULL
        return(shelf_life_lower)
    })
    
    return(shelf_life_lower)
}

find_confidence_bands <- function(df_melt, order, CI, threshold_y) {
    y <- na.omit(df_melt$value)
    x <- na.omit(df_melt$Time)
    n <- length(y) # Find length of y to use as sample size

    fit <- lm(y ~ poly(x,order,raw=TRUE),data=df_melt)
    summary_regression <- summary(fit)

    a <- as.numeric(format(round(summary_regression$coefficients[[1]],2))) # y-intercept
    b <- as.numeric(format(round(summary_regression$coefficients[[2]],2))) # 1st order coeff
    c <- ifelse(order > 1, as.numeric(format(round(summary_regression$coefficients[[3]],2))), 0) # 2nd order coeff
    d <- ifelse(order > 2, as.numeric(format(round(summary_regression$coefficients[[4]],2))), 0) # 3rd order coeff
    
    # # Find SSE and MSE
    # sse <- sum((y - fit$fitted.values)^2)
    # mse <- sse / (n - 2)
    # t.val <- qt(CI, fit$df.residual) # Calculate critical t-value
    # 
    # # Fit linear model with extracted coefficients
    # x_new <- 0:max(x, na.rm=TRUE)
    # y.fit <- a + b*x + c*x^2 + d*x^3
    # 
    # # Find the standard error of the regression line
    # se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
    # 
    # # Fit a new linear model that extends past the given data points (for plotting)
    # x_new2 <- 0:max(x)
    # y.fit2 <- a + b*x_new2 + c*x_new2^2 + d*x_new2^3

    x_new <- seq(min(x), max(x), length.out = length(x))
    y_fit <- a + b*x_new + c*x_new^2 + d*x_new^3
    
    pre_predict <- predict(fit, data.frame(x_new))
    p1 <- ggplot(data=df_melt) +
        geom_point(data=df_melt, aes(x=x_new, y=y), col='blue')
    conf_df <- predict(fit, data.frame('x_new'=x_new), interval='confidence',level=CI, se.fit=TRUE)
    predict_df <- data.frame('x_new'=x_new, 'fit'=y_fit, 'lwr'=conf_df$fit[,2], 'upr'=conf_df$fit[,3], 'se_fit'=conf_df$se.fit)
    slope.upper <- predict_df$upr
    slope.lower <- predict_df$lwr
    # Warnings of mismatched lengths are suppressed
    # slope.upper <- suppressWarnings(y.fit + t.val * se)
    # slope.lower <- suppressWarnings(y.fit - t.val * se)
    
    fit_lower <- lm(slope.lower ~ poly(x,order, raw=TRUE))
    # print(fit_lower)
    summary_regression_lower <- summary(fit_lower)
    # print(summary_regression_lower)
    # Extract fitted coefficients from model object
    # b_lower <- lm_lower.model$coefficients[1]
    # m_lower <- lm_lower.model$coefficients[2]
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
    
    # shelf_life_lower <- uniroot(function(x) f1_lower(x)-f2_lower(x),c(0,5), extendInt="yes")$root
    # 
    # print(paste("shelf-life: ", shelf_life))
    # print(paste("shelf-life of lower: ", shelf_life_lower))
    
    # Collect the computed confidence bands into a data.frame and name the columns
    # bands <- data.frame(cbind(x, slope.lower, y.fit, slope.upper))
    bands <- data.frame(cbind(predict_df$x_new, predict_df$lwr, predict_df$fit, predict_df$upr))
    colnames(bands) <- c('X Values', 'Lower Confidence Band', 'Y Values', 'Upper Confidence Band')

    
    return(bands)
}


 