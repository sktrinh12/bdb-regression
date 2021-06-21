library(qqplotr)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(nortest)
library(ggpubr)

# ORDER = 1 # For linear model
# CI = 0.95
# THRESHOLD_MFI = 75
# DATA_PT_SIZE = 10
# FONT_SIZE = 22

## Step 1: Upload raw stats ##
read_stats <- function(stats_file, pop){
    stats <- read_csv(stats_file, col_types = cols())
    stats <- stats[!is.na(stats$SI),] %>% arrange(Concentration)
    stats_pop <- stats[stats$pop == pop,]
    
    stats_pop <- stats_pop %>% select(Stability.Time.point, ug.test, `%+`, `MFI+`, `MFI-`, `rSD-`)
    colnames(stats_pop)[1] <- c("Condition")

    return(stats_pop)
    
}
rounded_shelf_life <- function(shelf_life){
    ## Rounding rules:
    ## 1. Round down to nearest half integer
    ## 2. If a half or whole integer, still round down to next half integer
    ## 3. If 1.5yrs, don't round down
    
    # If shelf-life is greater than 5 years, round down to 5 years to avoid extrapolation
    if(shelf_life > 5){
        shelf_life <- 5 # max timepoint tested
    }
    # If shelf-life is equal to or below 1.5y, don't round down
    else if(shelf_life <= 1.5){
        shelf_life <- shelf_life
    }
    # If shelf-life is a whole # or half integer (ex. 4.0 or 3.5), round down to next nearest half integer
    else if(shelf_life %% 0.5 == 0){
        shelf_life <- floor((shelf_life - 0.1) / 0.5) * 0.5
    }
    # Otherwise, round down to nearest half integer
    else{
        shelf_life <- floor(shelf_life / 0.5) * 0.5
    }
    
    return(round(shelf_life,1))
}

## Step 2: Calculate % of 4C Reference MFI ##
calculate_perct_4C_MFI <- function(df){
    
    calc_vect <- c() # Initialize % of 4C MFI data
    for(i in unique(df$Concentration)){
        for(row in c(1:nrow(df[df$Concentration == i,]))){
            ref_for_each_conc <- df$`MFI+`[df$Concentration == i & df$Condition == 0]
            MFI <- df$`MFI+`[df$Concentration == i][[row]]
            calc <- round((MFI/ref_for_each_conc)*100,0)
            calc_vect <- append(calc_vect, calc)
        }
        
    }
    df <- bind_cols(df, "% 4C Reference MFI"=calc_vect)
    df2 <- df %>% pivot_wider(names_from = Concentration, values_from = `MFI+`)
    
    return(df)
}

## Step 3a: Transform % of 4C Reference MFI data to wide ##
create_raw_reference_MFI_table_wide <- function(df){
    df_selected <- select(df, c(Condition, Concentration, `% 4C Reference MFI`))
    
    df2 <- df_selected %>% pivot_wider(names_from = Concentration, values_from = `% 4C Reference MFI`)
    colnames(df2)[1] <- "Time"
    for(i in c(2:length(colnames(df2)))){
        colnames(df2)[i] <- paste0(colnames(df2)[i], " ng/test")
    }
    
    return(df2)
}


## Step 3b: Transform % of 4C Reference MFI data to long ##
meltedDataTable <- function(df){
    
    # Melt Columns by Time
    dataMelt <- melt(df, "Time", variable='Concentrations')
    
    # dataMelt <- cbind(dataMelt, 'Labels'=paste0(parse_number(as.character(dataMelt$Concentrations)), ' ng/test'))
    dataMelt <- cbind(dataMelt, 'Labels'=as.character(dataMelt$Concentrations))
    
    # print('dataMelt: ')
    
    
    return (na.omit(dataMelt))
}

## Step 4: Create linear regression model ##
best_fit_equation <- function(df_melt, order){
    fit <- lm(value ~ poly(Time,order, raw=TRUE), data=df_melt)
    return(summary(fit))
}

## Step 5: Add lower 95% Confidence Intervals ##
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

## Step 6: Calculate predicted shelf-life ##

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
##########################
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
    return(shelf_life_lower)
}

## Step 7: Calculate model p-value ##
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

## Step 8: Calculated R^2 value ##
R_sq <- function(df_melt, order){
    summary_regression <- summary(lm(value ~ poly(Time,order, raw=TRUE), data=df_melt))
    r_sq <- format(round(summary_regression$r.squared,2)) # R^2 value
    adj_r_sq <- format(round(summary_regression$adj.r.squared,2)) # Adjusted R^2 value
    
    return(as.numeric(r_sq))
    
}

# Step 9: Create plot of regression model ##
plot_regression <- function(df, order, CI, confidence_bands){

    regression_plot <- ggplot(df, aes(x=Time, y=value, color=Concentrations)) + 
        geom_ribbon(data=df, aes(x=Time, y=value, ymin=confidence_bands[[2]], ymax=confidence_bands[[4]]), formula = y ~ poly(x,order, raw=TRUE), method="lm", col = "red", level=as.numeric(CI), alpha=0.2) +
        geom_line(data=confidence_bands, aes(x=`X Values`, y=`Y Values`), formula = y ~ poly(x,order, raw=TRUE), method="lm", col = "red") +
        geom_point(size=DATA_PT_SIZE) + 
        labs(x = "Time (years)",
             y = "% of 4C Reference MFI") +
        theme_minimal() +
        scale_color_brewer(palette = 'Reds', na.translate = F,
                           labels = unique(df$Labels)
                           # labels = c("30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test')
        ) +
        stat_regline_equation(data=df, aes(x=Time, y=value,label=paste(..eq.label.., ..rr.label.., sep = "~~~~~~")), formula = y ~ poly(x,order,raw=TRUE), method="lm", col="red",
                              label.x.npc="center",
                              label.y.npc="top",
                              
                              size=DATA_PT_SIZE) +
        theme(text=element_text(size = FONT_SIZE),
              legend.position = "bottom")
    return(regression_plot)
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

## Step 10: Create residual vs fit plot ##
residual_vs_fit_plot <- function(df_melt, order, residuals){
    

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
        geom_point(size=3, color = '#eb6864') +
        geom_hline(aes(yintercept=0)) +
        ylim(0-max(abs(fit_residuals)), 0+max(abs(fit_residuals))) +
        labs(title = 'Residuals vs. Fit Plot',
             x = 'Predicted % of 4C MFI', 
             y = 'Residuals') +
        theme(text=element_text(size = FONT_SIZE))

    
    
    # p <- ggplot(df_melt,aes(x=y, y=residuals, label=Time)) + 
    #     geom_point(size=DATA_PT_SIZE, color = '#eb6864') +
    #     # geom_area(color='blue') +
    #     geom_hline(aes(yintercept=0)) +
    #     ylim(0-max(abs(residuals)), 0+max(abs(residuals))) +
    #     labs(title = 'Residuals vs. Fit Plot',
    #          x = 'Predicted % of 4C MFI', 
    #          y = 'Residuals')  +
    #     theme(text=element_text(size = FONT_SIZE))
    

    return(p)
}

## Step 11: Create normal probability plot of residuals ##
normal_probability_plot <- function(df_melt, order, residuals){
    
    # And adding line with proper properties
    p <- ggplot(mapping = aes(sample = residuals)) + 
        qqplotr::stat_qq_point(size = DATA_PT_SIZE,color = "#eb6864") + 
        qqplotr::stat_qq_line(color="black") +
        xlab("Theoretical Quantiles") + ylab("Residuals") +
        theme(text=element_text(size = FONT_SIZE))
    return(p)
}

## Step 12: Create histogram of residuals ##
residual_histogram <- function(df_melt, order, residuals){

    p <- ggplot(df_melt,aes(x=residuals, label=Time)) + 
        geom_histogram(binwidth=sd(residuals), boundary=0, fill = '#eb6864', color="black") +
        labs(title = 'Histogram of Residuals',
             # subtitle = "Generally, 95% of residuals should not be larger than 2x the standard deviation of the residuals.",
             x = 'Residuals', 
             y = '# of Residuals') +
        theme(text=element_text(size = FONT_SIZE))
    
    return(p)
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
# 
# # ######################################################
# # ## 
# stats_file <- read_csv("all_stats_new_wellids.csv")
# 
# ## Step 1: Upload raw stats ##
# df <- read_stats("all_stats_new_wellids.csv", "Lymph")
# 
# ## Step 2: Calculate % of 4C Reference MFI ##
# df <- calculate_perct_4C_MFI(df)
# 
# ## Step 3a: Transform % of 4C Reference MFI data to wide ##
# df <- create_raw_reference_MFI_table_wide(df)
# 
# ## Step 3b: Transform % of 4C Reference MFI data to long ##
# df_melt <- meltedDataTable(df)
# 
# ## Step 4: Create linear regression model ##
# fit_summary <- best_fit_equation(df_melt, ORDER)
# 
# ## Step 5: Add lower 95% Confidence Intervals ##
# bands <- find_confidence_bands(df_melt, ORDER, CI, THRESHOLD_MFI)
# 
# ## Step 6: Calculate predicted shelf-life ##
# lower_shelf_life <- solve_for_lower_shelf_life(df_melt, ORDER, CI, THRESHOLD_MFI)
# shelf_life <- solve_for_shelf_life(df_melt, THRESHOLD_MFI, ORDER)
# print(lower_shelf_life)
# ## Step 7: Calculate model p-value ##
# p_value <- polynomial_evaluation_of_linearity(df_melt, ORDER)
# 
# ## Step 8: Calculated R^2 value ##
# r_sq <- R_sq(df_melt, ORDER)
# 
# # Step 9: Create plot of regression model ##
# regress_plot <- plot_regression(df_melt, ORDER, CI, bands)
# 
# residuals <- find_residuals(df_melt, ORDER)
# ## Step 10: Create residual vs fit plot ##
# resid_plot <- residual_vs_fit_plot(df_melt, ORDER, residuals)
# 
# ## Step 11: Create normal probability plot of residuals ##
# normal_prob_plot <- normal_probability_plot(df_melt, ORDER, residuals)
# 
# ## Step 12: Create histogram of residuals ##
# resid_histogram <- residual_histogram(df_melt, ORDER, residuals)
# 
# normality_p_value <- anderson_darling_normality_test(residuals)
# 
# ## Step 12: Create report ##
# regression_report <- function(){
#     ## Step 1: Upload raw stats ##
#     df <- read_stats("all_stats_new_wellids.csv", "Lymph")
#     
#     ## Step 2: Calculate % of 4C Reference MFI ##
#     df <- calculate_perct_4C_MFI(df)
#     
#     ## Step 3a: Transform % of 4C Reference MFI data to wide ##
#     df <- create_raw_reference_MFI_table_wide(df)
#     
#     ## Step 3b: Transform % of 4C Reference MFI data to long ##
#     df_melt <- meltedDataTable(df)
#     
#     ## Step 4: Create linear regression model ##
#     fit_summary <- best_fit_equation(df_melt, ORDER)
#     
#     ## Step 5: Add lower 95% Confidence Intervals ##
#     bands <- find_confidence_bands(df_melt, ORDER, CI, THRESHOLD_MFI)
#     
#     ## Step 6: Calculate predicted shelf-life ##
#     lower_shelf_life <- solve_for_lower_shelf_life(df_melt, ORDER, CI, THRESHOLD_MFI)
#     shelf_life <- solve_for_shelf_life(df_melt, THRESHOLD_MFI, ORDER)
#     print(lower_shelf_life)
#     ## Step 7: Calculate model p-value ##
#     p_value <- polynomial_evaluation_of_linearity(df_melt, ORDER)
#     
#     ## Step 8: Calculated R^2 value ##
#     r_sq <- R_sq(df_melt, ORDER)
#     
#     # Step 9: Create plot of regression model ##
#     regress_plot <- plot_regression(df_melt, ORDER, CI, bands)
#     
#     residuals <- find_residuals(df_melt, ORDER)
#     ## Step 10: Create residual vs fit plot ##
#     resid_plot <- residual_vs_fit_plot(df_melt, ORDER, residuals)
#     
#     ## Step 11: Create normal probability plot of residuals ##
#     normal_prob_plot <- normal_probability_plot(df_melt, ORDER, residuals)
#     
#     ## Step 12: Create histogram of residuals ##
#     resid_histogram <- residual_histogram(df_melt, ORDER, residuals)
#     
#     normality_p_value <- anderson_darling_normality_test(residuals)
# }
# 
# shelf_life_df <- tibble(
#     "Shelf-Life (days)" = round(lower_shelf_life*365,0),
#     "Shelf-Life (years)" = round(lower_shelf_life,2),
#     "R-squared" = round(r_sq, 2),
#     "Model p-value" = format(round(p_value$b_pvalue, 3), nsmall = 3)
# )
# 
# 
# 
# perc_4C_mfi_table_png <- function(){
#     tab = ggpubr::ggtexttable(df, rows = NULL,
#                               theme=ggpubr::ttheme("classic",
#                                                    base_size=28,
#                                                    padding = unit(c(16, 5), "mm"),
#                                                    colnames.style = ggpubr::colnames_style(fill = "lightgray",
#                                                                                            size = 28,
#                                                                                            linecolor = "black")))
#     tab = ggpubr::tab_add_title(tab,text = "% of 0 Reference MFI", face = "bold", size = 28, hjust = -2)
# 
# 
#     for (r in 3:(NROW(df)+2)){ # add bold first column and gray bg
#         tab = ggpubr::table_cell_font(tab, row=r, column=1, face="bold", size=28)
#         tab = ggpubr::table_cell_bg(tab, row=r, column=1, fill="lightgray")
#     }
# 
#     png("StatsTable.png", width=1600, height=800, units="px")
#     print(tab)
# }
# 
# shelf_life_estimates_table_png <- function(p_value, shelf_life){
# 
#     shelf_life_df_tab = ggpubr::ggtexttable(shelf_life_df, rows = NULL,
#                               theme=ggpubr::ttheme("classic",
#                                                    base_size=28,
#                                                    padding = unit(c(16, 5), "mm"),
#                                                    colnames.style = ggpubr::colnames_style(fill = "lightgray",
#                                                                                            size = 28,
#                                                                                            linecolor = "black")))
#     shelf_life_df_tab = ggpubr::tab_add_title(shelf_life_df_tab,text = "Shelf-Life Estimates", face = "bold", size = 28, hjust = -1.2)
# 
#     if(p_value < 0.05){
#         shelf_life_df_tab <- table_cell_bg(
#             shelf_life_df_tab,
#             row = 3,
#             column = 4,
#             linewidth = 5,
#             fill = "#f69494",
#             color = "#ee0000"
#         )
#     }
#     if(shelf_life < 0){
#         shelf_life_df_tab <- table_cell_bg(
#             shelf_life_df_tab,
#             row = 3,
#             column = c(1:2),
#             linewidth = 5,
#             fill = "#f69494",
#             color = "#ee0000"
#         )
#     }
#     if(unique(stats_file$Fluorochrome) == "Ab-E" & shelf_life < 1){
#         shelf_life_df_tab <- table_cell_bg(
#             shelf_life_df_tab,
#             row = 3,
#             column = c(1:2),
#             linewidth = 5,
#             fill = "#f69494",
#             color = "#ee0000"
#         )
#     }
#     else if(unique(stats_file$Fluorochrome) == "Purified" & shelf_life < 2){
#         shelf_life_df_tab <- table_cell_bg(
#             shelf_life_df_tab,
#             row = 3,
#             column = c(1:2),
#             linewidth = 5,
#             fill = "#f69494",
#             color = "#ee0000"
#         )
#     }
#     else if(shelf_life < 1.5){
#         shelf_life_df_tab <- table_cell_bg(
#             shelf_life_df_tab,
#             row = 3,
#             column = c(1:2),
#             linewidth = 5,
#             fill = "#f69494",
#             color = "#ee0000"
#         )
#     }
#     if(r_sq < 0.80){
#         shelf_life_df_tab <- table_cell_bg(
#             shelf_life_df_tab,
#             row = 3,
#             column = 3,
#             linewidth = 5,
#             fill = "#ffec8b",
#             color = "#ffd700"
#         )
#     }
# 
#     png("ShelfLifeEstimates.png", width=1200, height=800, units="px")
#     print(shelf_life_df_tab)
# }
# 
# anderson_darling_p_value_png <- function(p_value){
# 
#     ad_p_value_df = ggpubr::ggtexttable(tibble("Anderson-Darling\nNormality Test p-value" = round(normality_p_value, 3)), rows = NULL,
#                                             theme=ggpubr::ttheme("classic",
#                                                                  base_size=28,
#                                                                  padding = unit(c(16, 5), "mm"),
#                                                                  colnames.style = ggpubr::colnames_style(fill = "lightgray",
#                                                                                                          size = 28,
#                                                                                                          linecolor = "black")))
#     # ad_p_value_df = ggpubr::tab_add_title(ad_p_value_df,text = "Anderson-Darling Normality Test", face = "bold", size = 28)
# 
#     if(p_value >= 0.05){
#         ad_p_value_df <- table_cell_bg(
#             ad_p_value_df,
#             row = 2,
#             column = 1,
#             linewidth = 5,
#             fill = "#f69494",
#             color = "#ee0000"
#         )
#     }
# 
#     png("anderson_darling_p_value.png", width=1200, height=800, units="px")
#     print(ad_p_value_df)
# }
# 
# regression_pdf <- function(){
# 
#     # Create regression plot png
#     png("regression_plot.png", width = 1200, height = 800, units = "px")
#     print(regress_plot)
#     dev.off()
# 
#     # Create % of 4C Reference MFI Table png
#     perc_4C_mfi_table_png()
#     dev.off()
# 
#     # Create shelf-life estimates png
#     shelf_life_estimates_table_png(p_value$b_pvalue, lower_shelf_life)
#     dev.off()
# 
# 
# 
#     png_plots = list()
#     for (i in c("regression_plot.png","StatsTable.png", "ShelfLifeEstimates.png")) {
#         # pname = pnames[i]
#         png_plots[[i]] = grid::rasterGrob(png::readPNG(i))
#     }
#     # figure = ggpubr::ggarrange(
#     #     plotlist=png_plots,
#     #     ncol=2, nrow=2
#     # )
#     figure = ggarrange(
#         png_plots[[1]], ggarrange(png_plots[[2]], png_plots[[3]], ncol = 2),
#         nrow = 2
#     )
# 
#     return(figure)
# }
# quality_checks_pdf <- function(){
# 
#     # Create residuals vs fit plot png
#     png("residuals_vs_fit_plot.png", width = 1200, height = 800, units = "px")
#     print(resid_plot)
#     dev.off()
# 
#     # Create histogram of residuals png
#     png("residual_histogram.png", width = 1200, height = 800, units = "px")
#     print(resid_histogram)
#     dev.off()
# 
#     # Create normal probability plot of residuals png
#     png("normal_prob_plot_of_residuals.png", width = 1200, height = 800, units = "px")
#     print(normal_prob_plot)
#     dev.off()
# 
#     # Generate p-value for Anderson-Darling Normality Test
#     # png("anderson_darling_p_value.png", width = 1200, height = 800, units = "px")
#     # print(normality_p_value)
#     anderson_darling_p_value_png(normality_p_value)
#     dev.off()
# 
#     # Loop through each png and print to pdf
#     png_plots = list()
#     for (i in c("residuals_vs_fit_plot.png", "residual_histogram.png", "normal_prob_plot_of_residuals.png", "anderson_darling_p_value.png")) {
#         # pname = pnames[i]
#         png_plots[[i]] = grid::rasterGrob(png::readPNG(i))
#     }
#     figure = ggpubr::ggarrange(
#         plotlist=png_plots,
#         ncol=2, nrow=2
#     )
#     # figure = ggarrange(
#     #     png_plots[[1]], ggarrange(png_plots[[2]], stable.p, ncol = 2),
#     #     nrow = 2
#     # )
# 
#     return(figure)
# }
# 
# pdf("example_regression_report.pdf", title="Regression for Stability", width = 16, height = 10, onefile = TRUE)
# print(regression_pdf())
# print(quality_checks_pdf())
# 
# dev.off()
