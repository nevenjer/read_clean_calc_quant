plot_point_nor_dis <- function(df,
                               var = "Price",
                               start_date = NULL,
                               end_date = NULL,
                               point_date = NULL,
                               main_color = "lightblue",
                               stat_color = "grey60",
                               point_color = "orange") {
    
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    
    # -----------------------------
    # 1) Filter ช่วงวันที่
    # -----------------------------
    if (!is.null(start_date)) df <- df %>% filter(Date >= as.Date(start_date))
    if (!is.null(end_date))   df <- df %>% filter(Date <= as.Date(end_date))
    
    # -----------------------------
    # 2) ตัวแปรที่ต้องการ
    # -----------------------------
    x <- df[[var]]
    
    # -----------------------------
    # 3) จุดราคาตามวันที่ที่กำหนด
    # -----------------------------
    if (!is.null(point_date)) {
        latest_value <- df %>% filter(Date == as.Date(point_date)) %>% pull(var)
        if (length(latest_value) == 0) latest_value <- NA
    } else {
        latest_value <- tail(x, 1)
    }
    
    # -----------------------------
    # 4) สถิติ
    # -----------------------------
    stats <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    names(stats) <- c("Min", "Q1", "Median", "Q3", "Max")
    mean_val <- mean(x, na.rm = TRUE)
    
    # -----------------------------
    # 5) คำนวณ density ก่อน (สำคัญมาก)
    # -----------------------------
    d <- density(x, na.rm = TRUE)
    
    # หา y ของแต่ละสถิติ
    y_vals <- approx(d$x, d$y, xout = c(stats, mean_val))$y
    
    stat_df <- tibble(
        value = c(stats, mean_val),
        label = c("Min", "Q1", "Median", "Q3", "Max", "Mean"),
        y     = y_vals,
        type  = c("solid", "solid", "dashed", "solid", "solid", "solid")  # dashed เฉพาะ Median
    )
    
    # -----------------------------
    # 6) Plot Minimal Style
    # -----------------------------
    p <- ggplot(df, aes(x = !!sym(var))) +
        
        # Distribution
        geom_density(fill = main_color, alpha = 0.1, color = main_color, linewidth = 0.5) +
        
        # เส้นสถิติ
        geom_vline(data = stat_df,
                   aes(xintercept = value, linetype = type),
                   color = stat_color, linewidth = 0.5) +
        
        # Label ของเส้น (อยู่ตรงกลางเส้นจริง)
        geom_text(data = stat_df,
                  aes(x = value, y = y, label = label),
                  color = stat_color,
                  angle = 90,
                  vjust = -0.4,
                  size = 4) +
        
        # จุดราคาล่าสุด
        geom_point(aes(x = latest_value, y = 0),
                   color = point_color, size = 3.5) +
        
        # ชื่อกราฟ
        labs(
            title = paste("Minimal Distribution of", var),
            subtitle = paste(
                "Latest =", format(latest_value, big.mark = ",", nsmall = 2),
                "| Range:", start_date, "to", end_date
            ),
            x = var,
            y = "Density"
        ) +
        
        theme_minimal(base_size = 14) +
        theme(
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold"),
            legend.position = "none"
        ) +
        
        scale_y_continuous(labels = scales::comma) +
        scale_linetype_identity()
    
    return(p)
}
