library(qqplotr)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(reshape2)
library(nortest)
library(ggpubr)
library(grid)

ORDER = 1 # For linear model
CI = 0.95
THRESHOLD_MFI = 75
DATA_PT_SIZE = 10
FONT_SIZE = 22
EQN_SIZE = 10

source('omiq_regression.R')
source('global.R')

## Step 1: Upload raw stats ##
read_stats <- function(stats_file, pop){
    stats <- read_csv(stats_file, col_types = cols())
    stats <- stats[!is.na(stats$SI),] %>% arrange(Concentration)
    stats_pop <- stats[stats$pop == pop,]
    
    stats_pop <- stats_pop %>% select(Stability.Time.point, Concentration, `%+`, `MFI+`, `MFI-`, `rSD-`)
    colnames(stats_pop)[1] <- c("Condition")
    
    return(stats_pop)
    
}

get_stats_table_for_mfi_table <- function(stats_file, cell_pop){
    
    ## Step 1: Upload raw stats ##
    df <- read_stats(stats_file, cell_pop)
    
    ## Step 2: Calculate % of 4C Reference MFI ##
    df <- calculate_perct_4C_MFI(df)
    
    ## Step 3a: Transform % of 4C Reference MFI data to wide ##
    df <- create_raw_reference_MFI_table_wide(df)
    
    return(df)
}

create_shelf_life_summary_table <- function(lower_shelf_life, r_sq, p_value){
    
    rounded_lower_shelf_life <- rounded_shelf_life(lower_shelf_life)
    
    df <- tibble(
        "Raw Shelf-Life" = c(paste0(round(lower_shelf_life,1), " yrs (", round(lower_shelf_life*365,0), " days)")),
        "Rounded Shelf-Life" = c(paste0(rounded_lower_shelf_life, " yrs (", round(rounded_lower_shelf_life*365,0), " days)")),
        "R-squared" = round(r_sq, 2),
        "Model p-value"=format(round(p_value$b_pvalue, 3), nsmall = 3)
    )
    return(df)
}

perc_4C_mfi_table_png <- function(df){
    tab = ggpubr::ggtexttable(df, rows = NULL,
                              theme=ggpubr::ttheme("classic",
                                                   base_size=28,
                                                   padding = unit(c(16, 5), "mm"),
                                                   colnames.style = ggpubr::colnames_style(fill = "lightgray",
                                                                                           size = 28,
                                                                                           linecolor = "black")))
    tab = ggpubr::tab_add_title(tab,text = "% of 0 Reference MFI", face = "bold", size = 28, hjust = -2)
    
    
    for (r in 3:(NROW(df)+2)){ # add bold first column and gray bg
        tab = ggpubr::table_cell_font(tab, row=r, column=1, face="bold", size=28)
        tab = ggpubr::table_cell_bg(tab, row=r, column=1, fill="lightgray")
    }
    
    png("StatsTable.png", width=1600, height=800, units="px")
    print(tab)
    dev.off()
}

shelf_life_estimates_table_png <- function(stats_file, shelf_life_df, p_value, shelf_life, r_sq){
    
    shelf_life_df_tab = ggpubr::ggtexttable(shelf_life_df, rows = NULL,
                                            theme=ggpubr::ttheme("classic",
                                                                 base_size=28,
                                                                 padding = unit(c(16, 5), "mm"),
                                                                 colnames.style = ggpubr::colnames_style(fill = "lightgray",
                                                                                                         size = 28,
                                                                                                         linecolor = "black")))
    shelf_life_df_tab = ggpubr::tab_add_title(shelf_life_df_tab,text = "Shelf-Life Estimates", face = "bold", size = 28, hjust = -1.2)
    
    if(p_value >= 0.05){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = 4,
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    if(p_value < 0.05){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = 4,
            linewidth = 5,
            fill = "#a5eba5",
            color = "#2DC62D"
        )
        
    }
    if(shelf_life < 0){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#ffec8b",
            color = "#ffd700"
        )
    }
    if(unique(stats_file$Fluorochrome) == "Ab-E" & shelf_life > 0 & shelf_life < 1){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    else if(unique(stats_file$Fluorochrome) == "Purified" & shelf_life > 0 & shelf_life < 2){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    else if(shelf_life < 1.5 & shelf_life > 0){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = c(1:2),
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    if(r_sq < 0.80){
        shelf_life_df_tab <- table_cell_bg(
            shelf_life_df_tab,
            row = 3,
            column = 3,
            linewidth = 5,
            fill = "#ffec8b",
            color = "#ffd700"
        )
    }
    
    png("ShelfLifeEstimates.png", width=1200, height=800, units="px")
    print(shelf_life_df_tab)
}

anderson_darling_p_value_png <- function(p_value){
    
    ad_p_value_df = ggpubr::ggtexttable(tibble("Anderson-Darling\nNormality Test p-value" = round(p_value, 3)), rows = NULL,
                                        theme=ggpubr::ttheme("classic",
                                                             base_size=28,
                                                             padding = unit(c(16, 5), "mm"),
                                                             colnames.style = ggpubr::colnames_style(fill = "lightgray",
                                                                                                     size = 28,
                                                                                                     linecolor = "black")))
    
    # Reject null hypothesis - not normally distributed - flag as red
    if(p_value < 0.05){
        ad_p_value_df <- table_cell_bg(
            ad_p_value_df,
            row = 2,
            column = 1,
            linewidth = 5,
            fill = "#f69494",
            color = "#ee0000"
        )
    }
    # normally distributed - color green
    else{
        ad_p_value_df <- table_cell_bg(
            ad_p_value_df,
            row = 2,
            column = 1,
            linewidth = 5,
            fill = "#a5eba5",
            color = "#2DC62D"
        )
    }
    
    png("anderson_darling_p_value.png", width=1200, height=800, units="px")
    print(ad_p_value_df)
}

regression_pdf <- function(regress_plot, reference_mfi_table, shelf_life_summary_table, cell_pop, marker_name, optimal){
    
    # Create regression plot png
    png("regression_plot.png", width = 1200, height = 800, units = "px")
    print(regress_plot)
    dev.off()
    
    # Create % of 4C Reference MFI Table png
    reference_mfi_table
    dev.off()
    
    # Create shelf-life estimates png
    shelf_life_summary_table
    dev.off()
    
    
    
    png_plots = list()
    for (i in c("regression_plot.png","StatsTable.png", "ShelfLifeEstimates.png")) {
        png_plots[[i]] = grid::rasterGrob(png::readPNG(i))
    }
    
    figure = ggarrange(
        png_plots[[1]], ggarrange(png_plots[[2]], png_plots[[3]], ncol = 2),
        labels = c(paste0(cell_pop,"\n",marker_name, "\n", optimal)),
        nrow = 2
    )
    
    return(figure)
}
quality_checks_pdf <- function(resid_plot, resid_histogram, normal_prob_plot, normality_table){
    
    # Create residuals vs fit plot png
    png("residuals_vs_fit_plot.png", width = 1200, height = 800, units = "px")
    print(resid_plot)
    dev.off()
    
    # Create histogram of residuals png
    png("residual_histogram.png", width = 1200, height = 800, units = "px")
    print(resid_histogram)
    dev.off()
    
    # Create normal probability plot of residuals png
    png("normal_prob_plot_of_residuals.png", width = 1200, height = 800, units = "px")
    print(normal_prob_plot)
    dev.off()
    
    # Generate p-value for Anderson-Darling Normality Test
    png("anderson_darling_p_value.png", width = 1200, height = 800, units = "px")
    print(normality_table)
    dev.off()
    
    # Loop through each png and print to pdf
    png_plots = list()
    for (i in c("residuals_vs_fit_plot.png", "residual_histogram.png", "normal_prob_plot_of_residuals.png", "anderson_darling_p_value.png")) {
        png_plots[[i]] = grid::rasterGrob(png::readPNG(i))
    }
    figure = ggpubr::ggarrange(
        plotlist=png_plots,
        ncol=2, nrow=2
    )
    
    return(figure)
}


build_regression_report <- function(stats_file, cell_pop, marker_name, optimal){
    
    ## Step 1: Upload raw stats ##
    df <- get_stats_table_for_mfi_table(stats_file, cell_pop)
    
    ## Step 3b: Transform % of 4C Reference MFI data to long ##
    df_melt <- melt_reference_mfi_table(df)
    
    ## Step 4: Create linear regression model ##
    fit_summary <- best_fit_equation(df_melt, ORDER)
    
    ## Step 5: Add lower 95% Confidence Intervals ##
    bands <- find_confidence_bands(df_melt, ORDER, CI, THRESHOLD_MFI)
    
    ## Step 6: Calculate predicted shelf-life ##
    lower_shelf_life <- solve_for_lower_shelf_life(df_melt, ORDER, CI, THRESHOLD_MFI)
    
    shelf_life <- solve_for_shelf_life(df_melt, THRESHOLD_MFI, ORDER)
    
    ## Step 7: Calculate model p-value ##
    p_value <- polynomial_evaluation_of_linearity(df_melt, ORDER)
    
    ## Step 8: Calculated R^2 value ##
    r_sq <- R_sq(df_melt, ORDER)
    
    # Step 9: Create plot of regression model ##
    my_grob = grobTree(textGrob("This text stays in place!", x=0.1,  y=0.95, hjust=0,
                                gp=gpar(col="blue", fontsize=15, fontface="italic")))
    
    regress_plot <- regression_plot_global(FONT_SIZE, DATA_PT_SIZE, EQN_SIZE, df_melt, bands, ORDER, CI, 0.1, 0.2)  +
        coord_cartesian(ylim=c(0, NA)) + 
        scale_y_continuous(breaks=seq(0, 120, 20)) +
        stat_regline_equation(data=df_melt,
                              aes(x=Time, y=value,
                                  label=paste(..eq.label..)),
                              formula = y ~ poly(x,ORDER,raw=TRUE), method="lm", col="red",
                              label.x=0,label.y=10,size=12) +
        stat_regline_equation(data=df_melt,
                              aes(x=Time, y=value,
                                  label=paste(..rr.label..)),
                              formula = y ~ poly(x,ORDER,raw=TRUE), method="lm", col="red",
                              label.x=0,label.y=5,size=12)
        # annotation_custom(my_grob) +
        # stat_cor(label.x = 1, label.y = 20, size=12) +
        # stat_regline_equation(label.x = 1, label.y = 10, size=24)
   
   
    residuals <- find_residuals(df_melt, ORDER)
    
    ## Step 10: Create residual vs fit plot ##
    resid_plot <- residual_vs_fit_plot(df_melt, ORDER, FONT_SIZE, DATA_PT_SIZE)
    
    ## Step 11: Create normal probability plot of residuals ##
    normal_prob_plot <- normal_probability_plot(df_melt, ORDER, residuals, FONT_SIZE, DATA_PT_SIZE)
    
    ## Step 12: Create histogram of residuals ##
    resid_histogram <- residual_histogram(df_melt, ORDER, FONT_SIZE)
    
    normality_p_value <- anderson_darling_normality_test(residuals)
    
    shelf_life_df <- create_shelf_life_summary_table(lower_shelf_life, r_sq, p_value)
    
    shelf_life_summary_png <- shelf_life_estimates_table_png(read_csv(stats_file, col_types = cols()), shelf_life_df, p_value$b_pvalue, lower_shelf_life, r_sq)
    
    reference_mfi_table_png <- perc_4C_mfi_table_png(df)
    
    normality_pvalue_png <- anderson_darling_p_value_png(normality_p_value)
    
    pdf(paste0("regression_report_",cell_pop, ".pdf"), title="Regression for Stability", width = 16, height = 10, onefile = TRUE)
    print(regression_pdf(regress_plot, reference_mfi_table_png, shelf_life_summary_png, cell_pop, marker_name, optimal))
    print(quality_checks_pdf(resid_plot, resid_histogram, normal_prob_plot, normality_pvalue_png))
    
    dev.off()
}
# build_regression_report("all_stats_new_wellids.csv", "Lymph")

build_regression_report_per_cell_pop <- function(data_path, stats_file){
    stats_df <- read_csv(file.path(data_path, stats_file), col_types = cols())
    marker_name <- paste(unique(stats_df$Target.Species), 
                         unique(stats_df$Specificity..CD.), 
                         unique(stats_df$Clone), 
                         unique(stats_df$Fluorochrome))
    
    cell_pop_list <- unique(stats_df$pop)
    
    for(pop in cell_pop_list){
        print(paste0("Building regression report for ", pop))
        optimal <- unique(stats_df$Optimal[stats_df$pop == pop])
        build_regression_report(stats_file, pop, marker_name, optimal)
    }
}
# build_regression_report_per_cell_pop("stability_all_stats.csv")

