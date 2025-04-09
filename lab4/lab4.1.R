library(stringi)
library(rvest)

time_period <- 2014:2021
analyzed_nations <- c("Colombia", "China", "Russia", "Mexico", "Brazil")
chart_colors <- c("red", "blue", "green", "purple", "orange")

fetch_nation_stats <- function(yr){
  message(sprintf("Получение статистики за %d год ... ", yr))
  page_url <- sprintf("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=%d", yr)
  webpage <- read_html(page_url)
  stats_table <- html_nodes(webpage, "table#t2")[[1]]
  stats_df <- as.data.frame(html_table(stats_table, na.strings="-"))
  stats_df <- stats_df[-1]
  return(stats_df)
}

raw_stats <- lapply(time_period, fetch_nation_stats)
names(raw_stats) <- time_period
filtered_stats <- lapply(raw_stats, subset, Country %in% analyzed_nations)

metrics_analysis <- lapply(2:ncol(filtered_stats[[1]]), function(col_idx){
  
  result_df = data.frame()
  
  yearly_data <- lapply(time_period, function(yr){
    
    metric_row <- t(filtered_stats[[as.character(yr)]][col_idx])
    rownames(metric_row) = yr
    colnames(metric_row) = t(filtered_stats[[as.character(yr)]][1])
    metric_row
  })
      
  result_df <- do.call(rbind, yearly_data)
  result_df
})

metric_names = c('Индекс качества жизни (чем выше, тем лучше)',
                 'Индекс покупательной способности (чем выше, тем лучше)',
                 'Индекс безопасности (чем выше, тем лучше)',
                 'Индекс медицинского обслуживания (чем выше, тем лучше)',
                 'Индекс прожиточного минимума (чем ниже, тем лучше)',
                 'Отношение цены на жильё к доходу (чем ниже, тем лучше)',
                 'Индекс времени движения на дороге (чем ниже, тем лучше)',
                 'Индекс загрязнения (чем ниже, тем лучше)',
                 'Климатический индекс (чем выше, тем лучше)'
)

visualize_metrics <- function(metric_data, title){
  
  y_min <- min(as.matrix(metric_data), na.rm = TRUE)
  y_max <- max(as.matrix(metric_data), na.rm = TRUE)
  print(title)
  print(metric_data)
  
  matplot(
    time_period,
    metric_data,
    type="b",
    pch=16,
    lty=1,
    lwd=1.8,
    cex=0.8,
    col=chart_colors,
    ylim=c(y_min - 10, y_max + 50),
    main= title,
    xlab='Год',
    ylab=strsplit(title, " (", fixed = TRUE)[[1]][1]
  )
  legend('topleft', colnames(metric_data), ncol=3, lty=1, lwd=2, col=chart_colors)
}

lapply(seq_along(metrics_analysis), function(idx){
  visualize_metrics(metrics_analysis[[idx]], metric_names[idx])
})