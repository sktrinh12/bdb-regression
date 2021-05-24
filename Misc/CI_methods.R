reg.conf.intervals <- function(x, y) {
    x <- na.omit(x)
    y <- na.omit(y)
    n <- length(y) # Find length of y to use as sample size
    lm.model <- lm(y ~ poly(x, 1, raw=TRUE)) # Fit linear model
    print(lm.model)
    
    # Extract fitted coefficients from model object
    b0 <- lm.model$coefficients[1]
    b1 <- lm.model$coefficients[2]
    print(lm.model$fitted.values)
    # Find SSE and MSE
    sse <- sum((y - lm.model$fitted.values)^2)
    mse <- sse / (n - 2)
    
    t.val <- qt(0.975, n - 2) # Calculate critical t-value
    
    # Fit linear model with extracted coefficients
    x_new <- 1:max(x)
    print(length(x))
    y.fit <- b1 * x_new + b0
    
    # Find the standard error of the regression line
    se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
    
    # Fit a new linear model that extends past the given data points (for plotting)
    x_new2 <- 1:max(x)
    y.fit2 <- b1 * x_new2 + b0
    print(confint(lm.model, level=0.95))
    b1_lwr <- confint(lm.model, level=0.95)[2,1]
    b0_lwr <- confint(lm.model, level=0.95)[1,1]
    y_lwr <- b1_lwr * x_new + b0_lwr
    
    b1_upr <- confint(lm.model, level=0.95)[2,2]
    b0_upr <- confint(lm.model, level=0.95)[1,2]
    y_upr <- b1_upr * x_new + b0_upr
    
    
    ###############################################
    print('---------PREDICT----------')
    conf.dist <- predict(lm.model, newdata=data.frame(x), interval='confidence',alpha=0.95)
    df.new <- data.frame(cbind("speed"=cars$speed, "dist"=cars$dist, conf.dist))
    print(df.new)
    pl <- ggplot(cars) + geom_point(aes(x=speed, y=dist), size=2, colour="#993399") + 
        xlab("Speed (mph)") + ylab("Stopping distance (ft)")  
    print(pl)
    pl <- pl +   
        # geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
        geom_ribbon(data=df.new, aes(x=speed, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="#339900") +  
        geom_line(data=df.new, aes(x=speed, y=fit), colour="#339900", size=1)
    print(pl)
    ###############################################
    
    
    lm_summ <- summary(lm.model)
    
    CI_b1 <- c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2],
               "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])
    print(CI_b1)
    CI_b0 <- c("lower" = lm_summ$coef[1,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[1, 2],
               "upper" = lm_summ$coef[1,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[1, 2])
    print(CI_b0)
    
    y_lwr_ci <- CI_b1[1] * x_new + CI_b0[1]
    y_upr_ci <- CI_b1[2] * x_new + CI_b0[2]
    
    # Warnings of mismatched lengths are suppressed
    slope.upper <- suppressWarnings(y.fit2 + t.val * se)
    slope.lower <- suppressWarnings(y.fit2 - t.val * se)
    
    # Collect the computed confidence bands into a data.frame and name the colums
    bands <- data.frame(cbind(slope.lower, slope.upper))
    colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
    
    # # Plot the fitted linear regression line and the computed confidence bands
    # plot(x, y, cex = 1.75, pch = 21, bg = 'gray')
    # lines(y.fit2, col = 'black', lwd = 2)
    # lines(bands[1], col = 'blue', lty = 2, lwd = 2)
    # lines(bands[2], col = 'blue', lty = 2, lwd = 2)
    # lines(y_lwr, col = 'red', lty = 2, lwd = 2)
    # lines(y_upr, col = 'red', lty = 2, lwd = 2)
    # lines(y_lwr_ci, col = 'green', lty = 2, lwd = 2)
    # lines(y_upr_ci, col = 'green', lty = 2, lwd = 2)
    return(bands)
}

conf.intervals <- reg.conf.intervals(cars$speed, cars$dist)

cars
lines(y_lwr, col = 'red', lty = 2, lwd = 2)

predict_method <- function(cars, order){
    y <- na.omit(cars$dist)
    x <- na.omit(cars$speed)
    lm.model <- lm(y ~ poly(x, order, raw=TRUE))
    summary(lm.model)$coefficients
    print(summary(lm.model))
    print(cars)
    ###############################################
    
    print('---------PREDICT----------')
    conf.dist <- predict(lm.model, newdata=data.frame(x,y), interval='confidence',alpha=0.95, se.fit=TRUE)
    conf <- data.frame(conf.dist$fit)
    print(conf)
    df.new <- data.frame(cbind("speed"=cars$speed, "dist"=cars$dist, 'fit'=conf$fit, 'lwr'=conf$lwr, 'upr'=conf$upr))
    print(df.new)
    p1 <- ggplot(cars) + geom_point(aes(x=speed, y=dist), size=2, colour="#993399") + 
        xlab("Speed (mph)") + ylab("Stopping distance (ft)")  
    print(p1)
    p1 <- p1 +
        # geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") +
        geom_ribbon(data=df.new, aes(x=speed, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="#339900") +
        # geom_line(data=df.new, aes(x=speed, y=fit), colour="#339900", size=1) +
        geom_smooth(data=df.new, aes(x=speed, y=dist), formula=y~ poly(x, order, raw=TRUE), method='lm', se=FALSE) +
        ylim(0,120)
    print(p1)
    ###############################################
}

predict_method(cars,1)
predict_method(cars,2)
predict_method(cars,3)

confint_method <- function(cars, order){
    
    y <- na.omit(cars$dist)
    x <- na.omit(cars$speed)
    lm.model <- lm(y ~ poly(x, order, raw=TRUE))
    ###############################################
    x_new <- 1:length(x)
    a <- summary(lm.model)$coefficients[[1]] # y-intercept
    b <- summary(lm.model)$coefficients[[2]] # 1st order coeff
    c <- ifelse(order > 1, summary(lm.model)$coefficients[[3]], 0) # 2nd order coeff
    d <- ifelse(order > 2, summary(lm.model)$coefficients[[4]], 0) # 3rd order coeff
    
    y.fit <- a + b*x_new + c*x_new^2 + d*x_new^3
    print(y.fit)
    ###############################################
    conf.int <- confint(lm.model, level=0.95)
    a_lwr <- conf.int[1,1]
    b_lwr <- conf.int[2,1]
    c_lwr <- ifelse(order > 1, conf.int[3,1], 0)
    d_lwr <- ifelse(order > 2, conf.int[4,1], 0)
    
    y_lwr <- a_lwr + b_lwr*x_new + c_lwr*x_new^2 + d_lwr*x_new^3
    
    a_upr <- conf.int[1,2]
    b_upr <- conf.int[2,2]
    c_upr <- ifelse(order > 1, conf.int[3,2], 0)
    d_upr <- ifelse(order > 1, conf.int[4,2], 0)
    
    y_upr <- a_upr + b_upr*x_new + c_upr*x_new^2 + d_upr*x_new^3

    df.new <- data.frame('speed'=cars$speed, 'dist'=cars$dist, 'fit'=y.fit,'lwr'=y_lwr, 'upr'=y_upr)
    print('----------CONFINT--------------')
    print(df.new)
    p2 <- ggplot(cars) + geom_point(aes(x=speed, y=dist), size=2, colour="#993399") + 
        xlab("Speed (mph)") + ylab("Stopping distance (ft)")  
    print(p2)
    p2 <- p2 +   
        # geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") + 
        geom_ribbon(data=df.new, aes(x=speed, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="#339900") +  
        # geom_line(data=df.new, aes(x=speed, y=fit), colour="#339900", size=1) +
        geom_smooth(data=df.new, aes(x=speed, y=dist), method='lm', se=FALSE) +
        ylim(0,120)
    print(p2)
}

manual_method <- function(cars, order){
    y <- na.omit(cars$dist)
    x <- na.omit(cars$speed)
    n <- length(y)
    lm.model <- lm(y ~ poly(x, order, raw=TRUE))
    print(lm.model)
    ##########################################
    
    x_new <- 1:max(x)
    a <- summary(lm.model)$coefficients[[1]] # y-intercept
    b <- summary(lm.model)$coefficients[[2]] # 1st order coeff
    c <- ifelse(order > 1, summary(lm.model)$coefficients[[3]], 0) # 2nd order coeff
    d <- ifelse(order > 2, summary(lm.model)$coefficients[[4]], 0) # 3rd order coeff
    ############################################
    # Find SSE and MSE
    sse <- sum((y - lm.model$fitted.values)^2)
    mse <- sse / (n - 2)
    
    t.val <- qt(0.975, n - 2) # Calculate critical t-value
    
    # Fit linear model with extracted coefficients
    y.fit <- a + b*x_new + c*x_new^2 + d*x_new^3
    print(y.fit)
    # Find the standard error of the regression line
    se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
    print(length(se))
    # Fit a new linear model that extends past the given data points (for plotting)
    x_new2 <- 1:length(x)
    y.fit2 <- a + b*x_new2 + c*x_new2^2 + d*x_new2^3
    # Warnings of mismatched lengths are suppressed
    slope.upper <- suppressWarnings(y.fit2 + t.val * se)
    slope.lower <- suppressWarnings(y.fit2 - t.val * se)
    print(length(slope.upper))
    print(length(slope.lower))
    df <- data.frame('x'=x_new2, 'y.fit'=y.fit2, 'lwr'=slope.lower, 'upr'=slope.upper)
    print(df)
    df.new <- data.frame(cbind("speed"=cars$speed, "dist"=cars$dist, 'fit'=y.fit,'lwr'=slope.lower, 'upr'=slope.upper))
    
    ###############################################
    print('---------MANUAL----------')
    # print(df.new)
    p <- ggplot(data=df) +
        geom_point(data=df, aes(x=x, y=y.fit), col='green') +
        geom_point(data=df, aes(x=x, y=lwr), col='red') +
        geom_point(data=df, aes(x=x, y=upr), col='red') +
        ylim(0,120) +
        xlim(5,25)
    print(p)
    p3 <- ggplot(cars) + geom_point(aes(x=speed, y=dist), size=2, colour="#993399") +
        xlab("Speed (mph)") + ylab("Stopping distance (ft)")
    print(p3)
    p3 <- p3 +
        # geom_ribbon(data=df.new, aes(x=speed, ymin=lwr.pred, ymax=upr.pred), alpha=0.1, inherit.aes=F, fill="blue") +
        geom_ribbon(data=df, aes(x=x_new2, y=y.fit2, ymin=lwr, ymax=upr), formula = y ~ poly(x,order, raw=TRUE), method="lm", col = "red", alpha=0.2, inherit.aes=F, fill="#339900") +
        geom_point(data=df, aes(x=x_new2, y=lwr)) +
        geom_point(data=df, aes(x=x_new2, y=upr)) +
        
        # geom_line(data=df.new, aes(x=speed, y=fit), colour="#339900", size=1) +
        geom_smooth(data=df.new, aes(x=speed, y=dist), method='lm', se=FALSE) +
        ylim(0,120) +
        xlim(5,25)
    print(p3)

}

predict_method(cars,1)
# confint_method(cars,1)
manual_method(cars,1)

require(graphics)
print(women)
## using poly: this did not work in R < 1.5.0
fm <- lm(dist ~ poly(speed, 2, raw=TRUE), data = cars)
print(fm)
plot(cars, xlab = "Speed", ylab = "Distance")
spd <- seq(3, 26, length.out = length(cars$speed))
print(spd)
writexl::write_xlsx(data.frame(spd),'cars3.xlsx')
print(fm)
nD <- data.frame(speed = spd)
pfm <- predict(fm, nD)
lines(spd, pfm)
pf2 <- predict(update(fm, ~ stats::poly(speed, 2, raw=TRUE)), nD, interval = 'confidence', level=0.95, se.fit=TRUE)
lines(spd,pf2[,2], col='red')
lines(spd,pf2[,3], col='red')
print(pf2)
df <- data.frame('fit'=pf2$fit[,1], 'lwr'=pf2$fit[,2], 'upr'=pf2$fit[,3], 'se_fit'=pf2$se.fit)
df
writexl::write_xlsx(df, 'cars5.xlsx')
writexl::write_xlsx(cars, 'cars2.xlsx')
fm_lwr <- lm(pf2[,2] ~ poly(spd, 2), data=cars)
fm_lwr
summary(fm_lwr)
stopifnot(all.equal(pfm, pf2)) ## was off (rel.diff. 0.0766) in R <= 3.5.0

## see also example(cars)
example(cars)

## see bs and ns for spline examples.
