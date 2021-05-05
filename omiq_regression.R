ORDER = 1 # For linear model
CI = 0.95
THRESHOLD_MFI = 75

## Step 1: Upload raw stats ##
read_stats <- function(stats_file, pop){
    stats <- read_csv(stats_file)
    stats <- stats[!is.na(stats$SI),] %>% arrange(ug.test)
    stats_pop <- stats[stats$pop == pop,]
    
    stats_pop <- stats_pop %>% select(Stability.Time.point, ug.test, `%+`, `MFI+`, `MFI-`, `rSD-`)
    colnames(stats_pop)[1:2] <- c("Condition", "Concentration")

    return(stats_pop)
    
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
    print(shelf_life_lower)
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
    
    print(df)
    regression_plot <- ggplot(df, aes(x=Time, y=value, color=Concentrations)) + 
        geom_ribbon(data=df, aes(x=Time, y=value, ymin=confidence_bands[[2]], ymax=confidence_bands[[4]]), formula = y ~ poly(x,order, raw=TRUE), method="lm", col = "red", level=as.numeric(CI), alpha=0.2) +
        geom_line(data=confidence_bands, aes(x=`X Values`, y=`Y Values`), formula = y ~ poly(x,order, raw=TRUE), method="lm", col = "red") +
        geom_point(size=5) + 
        labs(x = "Time (years)",
             y = "% of 4C Reference MFI") +
        theme_minimal() +
        scale_color_brewer(palette = 'RdYlGn', na.translate = F,
                           labels = unique(df$Labels)
                           # labels = c("30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test')
        ) +
        stat_regline_equation(data=df, aes(x=Time, y=value,label=paste(..eq.label.., ..rr.label.., sep = "~~~~~~")), formula = y ~ poly(x,order,raw=TRUE), method="lm", col="red",
                              label.x.npc="center",
                              label.y.npc="top",
                              
                              size=5) +
        theme(text=element_text(size = 11),
              legend.position = "bottom")
    show(regression_plot)
    return(regression_plot)
}

## Step 10: Create residual vs fit plot ##
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
    
    show(p)
    return(p)
}

## Step 11: Create report ##

######################################################
## Step 1: Upload raw stats ##
df <- read_stats("all_stats.csv", "Lymph")

## Step 2: Calculate % of 4C Reference MFI ##
df <- calculate_perct_4C_MFI(df)

## Step 3a: Transform % of 4C Reference MFI data to wide ##
df <- create_raw_reference_MFI_table_wide(df)
df <- create_raw_reference_MFI_table_wide(readxl::read_xlsx("hGranzyme A CB9 R718- 1'5y - test size.xlsx"))
## Step 3b: Transform % of 4C Reference MFI data to long ##
df_melt <- meltedDataTable(df)

## Step 4: Create linear regression model ##
fit_summary <- best_fit_equation(df_melt, ORDER)

## Step 5: Add lower 95% Confidence Intervals ##
bands <- find_confidence_bands(df_melt, ORDER, CI, THRESHOLD_MFI)

## Step 6: Calculate predicted shelf-life ##
lower_shelf_life <- solve_for_lower_shelf_life(df_melt, ORDER, CI, THRESHOLD_MFI)
shelf_life <- solve_for_shelf_life(df_melt, THRESHOLD_MFI, ORDER)
print(shelf_life)

## Step 7: Calculate model p-value ##
p_value <- polynomial_evaluation_of_linearity(df_melt, ORDER)
print(p_value)

## Step 8: Calculated R^2 value ##
r_sq <- R_sq(df_melt, ORDER)
print(r_sq)
# Step 9: Create plot of regression model ##
regress_plot <- plot_regression(df_melt, ORDER, CI, bands)

## Step 10: Create residual vs fit plot ##
resid_plot <- residual_vs_fit_plot(df_melt, ORDER)
flextable(data=tibble("No CI"=round(shelf_life,1),
                      "CI"=round(lower_shelf_life,1),
                      "p-value"=format(round(p_value$b_pvalue, digits=2), nsmall = 2) ,
                      "r_sq"=r_sq
                      ))
pdf("example_regression_report.pdf", width = 16, height = 10)
library(rstatix)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
df_mass <- create_raw_reference_MFI_table_wide(readxl::read_xlsx("hIL-4 MP4-25D2 R718 - mass size.xlsx"))
df_full <- create_raw_reference_MFI_table_wide(readxl::read_xlsx("hIL-4 MP4-25D2 R718 - 1'5y_full_titr.xlsx"))
df_short <- create_raw_reference_MFI_table_wide(readxl::read_xlsx("hIL-4 MP4-25D2 R718 - 1'5y_short_titr.xlsx"))
df_test <- create_raw_reference_MFI_table_wide(readxl::read_xlsx("hIL-4 MP4-25D2 R718 - test size.xlsx"))
df_melt_mass <- meltedDataTable(df_mass) %>% add_column("Dataset"="Mass Size")
df_melt_full <- meltedDataTable(df_full) %>% add_column("Dataset"="1.5y + 5y full titration")
df_melt_short <- meltedDataTable(df_short) %>% add_column("Dataset"="1.5y + 5y short titration")
df_melt_test <- meltedDataTable(df_test) %>% add_column("Dataset"="Test Size")

df_test_mass <- as_tibble(rbind(df_melt_mass, df_melt_test)) %>% convert_as_factor(Dataset)
df_all <- as_tibble(rbind(df_melt_mass, df_melt_test, df_melt_full, df_melt_short)) %>% convert_as_factor(Dataset)
df_test_short <- as_tibble(rbind(df_melt_short, df_melt_test)) %>% convert_as_factor(Dataset)
df_test_full <- as_tibble(rbind(df_melt_full, df_melt_test)) %>% convert_as_factor(Dataset)

df_all[df_all$Time == 0.5 & df_all$Dataset == "1.5y + 5y full titration",]
# av_test_short <- aov(value ~ Dataset, data=df_test_short)
# av_test_full <- aov(value ~ Dataset, data=df_test_full)

# summary(av_test_short)
# summary(av_test_full)

# df_test_short %>%
#     group_by(Dataset, Time) %>%
#     get_summary_stats(value, type = "mean_sd")

bxp <- ggboxplot(
    df_all, x = "Time", y = "value",
    color = "Dataset", palette = "jco",
    xlab = "Time (years)", ylab = "% of 4C Reference MFI"
    # facet.by = "diet", short.panel.labs = FALSE
)
bxp
df_all %>% group_by(Time) %>% identify_outliers(value)


ggline(df_all, x = "Dataset", y = "value", 
       add = c("mean_se", "jitter"), 
       color = "Dataset", palette = "jco",
       # order = c("ctrl", "trt1", "trt2"),
       ylab = "Weight", xlab = "Treatment")
# Compute the analysis of variance
res.aov <- aov(value ~ Dataset, data = df_all)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)

res.aov <- anova_test(
    data = df_all, dv = value, wid = Concentrations,
    within = c(Dataset, Time)
)
df_all[20:30,]

get_anova_table(res.aov)

df_test_short
na.omit(df_melt_short) %>%
    group_by(Dataset, Time) %>%
    identify_outliers(value) %>%
    # shapiro_test(value)
identify_outliers(df_all, variable = "value")

df_all %>%
    group_by(Dataset) %>%
    identify_outliers(Time)

df_melt_mass
res.aov <- anova_test(
    data = df_test_short, dv = value, wid = Dataset,
    within = c(Dataset, Time)
)
get_anova_table(res.aov)

## Step 1: Test for normal distribution ##
normal_distribution_test <- function(){
    set.seed(100)
    sample_n(df_melt_mass, nrow(df_mass))
    
    ggqqplot(df_melt_full$value)
    shapiro.test(df_melt_full$value)
}
## Step 2: Test that datasets have a common variance ##


