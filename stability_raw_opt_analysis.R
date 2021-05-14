# Check analysis and 
# 1) pull in formats and those min shelf life requirements
# 2) investigate why some are blank
# 3) check that it matches our analysis

library(dplyr)
library(tidyr)
library(tibble)

find_difference_raw_optimal_SL <- function(wave_no, sheet_name){
    
    wave_summary <- readxl::read_xlsx("All_Wave_Results.xlsx", sheet = sheet_name)

    # Filter only on linear models and ones with stat. sign. model coeff. p-value
    df <- wave_summary[wave_summary$`Model Order`=="Linear" & wave_summary$`Model p-value` < 0.05,]
    
    # Filter only columns of interest
    df_subset <- df[c("Filename", "Format", "Min. Shelf-Life (days)", "Concentrations Included", "75% Threshold, Lwr 95% CI")]
    
    # Add appropriate columns
    # df_wide <- df_subset %>% pivot_wider(Filename, names_from = `Concentrations Included`, values_from = c(`75% Threshold, Lwr 95% CI`, `Format`, `Min. Shelf-Life (days)`))
    df_wide <- df_subset %>% pivot_wider(c(Filename, `Format`, `Min. Shelf-Life (days)`), names_from = `Concentrations Included`, values_from = `75% Threshold, Lwr 95% CI`)
    df_wide <- df_wide %>% add_column("Raw (yrs)" = df_wide$Raw / 365) %>% add_column("Optimal (yrs)" = df_wide$`Optimal +1/-2` / 365) %>% add_column("Min. Shelf-Life (yrs)" = df_wide$`Min. Shelf-Life (days)` / 365, .after="Format")
    df_wide <- df_wide %>% add_column("Difference (yrs)" = df_wide$`Raw (yrs)` - df_wide$`Optimal (yrs)`) %>% add_column("Wave #" = rep(paste("Wave", as.character(wave_no)), nrow(df_wide)), .before="Filename")
    
    return(df_wide)
    # # Write to individual excel file
    # writexl::write_xlsx(df_wide, paste0("Wave_", wave_no, "_wide_analysis.xlsx"))

}
wave2 <- find_difference_raw_optimal_SL(2,"Wave 2 Summary")
wave3 <- find_difference_raw_optimal_SL(3,"Wave 3 Summary - Exclude Reruns")
wave5 <- find_difference_raw_optimal_SL(5,"Wave 5 Summary - Mass Size")
wave6 <- find_difference_raw_optimal_SL(6,"Wave 6 Summary - Mass Size")
wave8 <- find_difference_raw_optimal_SL(8,"Wave 8 Summary")

wave_num_list <- list(wave2, wave3, wave5, wave6, wave8)
df_all <- tibble("Wave #"=NA, "Filename"=NA, "Format"=NA, "Min. Shelf-Life (yrs)"=NA,"Raw (yrs)"=NA, "Optimal (yrs)"=NA, "Difference (yrs)"=NA)
for(i in wave_num_list){
    # df <- readxl::read_xlsx(paste0("Wave_", as.character(num), "_wide_analysis.xlsx"))
    # df <- paste0("wave",as.character(num))
    # print(df)
    print(i)
    # df <- i %>% add_column("Wave #" = rep(paste("Wave", as.character(parsen)), nrow(df)), .before="Filename")
    df_subset_yrs <- i[c("Wave #", "Filename", "Format", "Min. Shelf-Life (yrs)","Raw (yrs)", "Optimal (yrs)", "Difference (yrs)")]
    df_all <- add_row(df_all, df_subset_yrs)
    print(df_all)
}

writexl::write_xlsx(df_all, "All_waves_diff_raw_optimal.xlsx")
