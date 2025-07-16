rm(list=ls())
setwd("D:/毕业/方案二")

library(sf)
library(ggplot2)

# 读取数据
data <- read.csv("D:/毕业/方案二/data/data_Class.csv")
cropland <- st_read("D:/毕业/方案二/有机肥降酸潜力/cropland.shp")

# 导入dplyr库
library(dplyr)

# 统计每个经纬度点的数量
data_counts <- data %>%
  group_by(lon, lat) %>%
  summarise(count = n())

write.csv(data_counts, file = "D:/毕业/方案二/样点图/data_counts.csv", row.names = FALSE)

# 添加点并设置图例
mapWorld <- borders("world", fill = "white")

# 基础地图
p <- ggplot() + 
  mapWorld +
  theme(panel.grid.minor = element_blank()) +
  theme(
    panel.background = element_rect(fill = "white"),  # 设置底色为白色
    panel.grid.major = element_blank(),  # 去除网格线
    axis.text = element_text(size = 14),  # 设置坐标轴标签字体大小
    axis.title = element_text(size = 16)  # 设置坐标轴标题字体大小  
  ) + 
  scale_x_continuous(breaks=c(-150,-100, -50, 0, 50, 100, 150), expand=c(0,0),
                     labels=c('150°W','100°W','50°W','0','50°E','100°E','150°E'))+
  scale_y_continuous(breaks=c(-50, 0, 50),expand=c(0, 0),
                     labels=c('50°S','0','50°N'))+
  labs(x = 'Longitude', y = 'Latitude')
p1 <- p +
  # 绘制cropland.shp文件
  geom_sf(data = cropland, aes(fill = "Cropland"), color = "#E1D09D", size = 0.2) + 
  scale_fill_manual(values = c("Cropland" = "#E1D09D"), name = "Legend") +  # 设置cropland的填充颜色和图例名称
  
  # 绘制点数据，点的大小根据数量来调整
  geom_point(data = data_counts, aes(x = lon, y = lat, size = count), 
             color = "#00A087FF", alpha = 0.7) +
  
  # 设置点的大小映射
  scale_size_continuous(
    range = c(1, 3),  # 设置点的大小范围，2是最小值，10是最大值
    name = "Sample Size",  # 图例标题
    breaks = c(1, 10, 20, 30),  # 设置图例上的标记，指定不同大小的标记
    labels = c("1-10", "10-20", "20-30", "30-40")  # 设置对应的标签
  ) +  
  
  # 图例和其他样式设置
  theme(legend.position = c(0.1, 0.4),  # 设置图例位置
        legend.background = element_blank(),
        legend.key = element_rect(fill = NULL),
        panel.border = element_rect(color = "black", fill = NA))  # 添加黑色图框

p1
# p1 <- p +
#   # 绘制cropland.shp文件
#   geom_sf(data = cropland, aes(fill = "Cropland"), color = "#E1D09D", size = 0.2) + 
#   scale_fill_manual(values = c("Cropland" = "#E1D09D"), name = " ") +  # 设置cropland的填充颜色和图例名称
#   # 绘制点数据，点的大小根据数量来调整
#   geom_point(data = data_counts, aes(x = longitude, y = latitude, size = count), 
#              color = "#00A087FF", alpha = 0.7) +
#   scale_size_continuous(range = c(2, 4), name = "Sample Size") +  # 设置点的大小范围和图例名称
#   theme(legend.position = c(0.1, 0.3),  # 设置图例位置
#         legend.background = element_blank(),
#         legend.key = element_rect(fill = NULL),
#         panel.border = element_rect(color = "black", fill = NA))  # 添加黑色图框
# 
# p1

ggsave(
  filename = "D:/毕业/方案二/Figure/Figure1_11.png",
  plot = p1,
  width = 25,        # 宽度250cm
  height = 15,       # 高度150cm
  units = "cm",      # 厘米单位
  dpi = 600          # 打印级分辨率
)

pdf("D:/毕业/方案二/Figure/Figure1_11.pdf", height = 8, width = 10)  # 设置文件名和图像尺寸
p1
dev.off()