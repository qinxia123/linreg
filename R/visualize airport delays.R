# 安装必要的包（如果尚未安装）
# install.packages(c("nycflights13", "dplyr", "ggplot2"))

# 加载必要的包
library(nycflights13)
library(dplyr)
library(ggplot2)

# 创建可视化函数
visualize_airport_delays <- function() {
  # 数据处理：计算每个机场的平均延误
  airport_delays <- flights %>%
    filter(!is.na(arr_delay)) %>%
    group_by(dest) %>%
    summarise(mean_delay = mean(arr_delay, na.rm = TRUE), .groups = 'drop') %>%
    left_join(airports, by = c("dest" = "faa")) %>%
    filter(!is.na(lon) & !is.na(lat))

  # 打印处理后的数据以进行调试，显示前20行
  print(airport_delays, n = 20)

  # 绘制可视化
  ggplot(data = airport_delays, aes(x = lon, y = lat, size = mean_delay)) +
    geom_point(alpha = 0.5) +
    scale_size_continuous(range = c(1, 10), name = "Mean Arrival Delay (minutes)") +
    labs(title = "Mean Arrival Delays by Airport",
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
}

# 调用可视化函数
visualize_airport_delays()
