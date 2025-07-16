setwd("D:/毕业/方案二/降酸关键区")

# 加载本地数据
cl <- terra::rast("cluster.tif")
print(cl)

# 将SpatRaster转换为data.frame
cl.1 <- as.data.frame(cl, xy = TRUE)

# 获取基础世界地图
library(rnaturalearth)
library(ggplot2)
library(sf)
library(RColorBrewer)

world <- ne_countries(scale = "medium", returnclass = "sf")

# 这里我们根据Zone1, Zone2, Zone3, Zone4来设置颜色，并调整图例顺序
p3 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # 添加世界地图背景
  geom_tile(data = cl.1, aes(x = x, y = y, fill = CLUSTER)) +  # 使用CLUSTER列填充
  scale_fill_manual(
    values = c("Zone1" = "#f8766d", 
               "Zone2" = "#7Cae00", 
               "Zone3" = "#00bfc4", 
               "Zone4" = "#c77cff"),
    limits = c("Zone1", "Zone2", "Zone3", "Zone4")  # 设置图例的顺序
  ) +  # 手动设置每个Zone对应的颜色
  labs(fill = 'Cluster') +  # 手动设置填充色的色阶，不丢弃任何级别
  coord_sf(expand = FALSE) +
  theme_minimal() +  # 更换主题
  theme(
    legend.position = c(0.06, 0.12),  # 设置图例位置为左下角
    legend.justification = c(0, 0),  # 设置图例对齐为左下角
    legend.key.size = unit(1, "cm"),
    panel.grid = element_blank(),  # 删除网格线
    axis.line = element_blank(),  # 删除坐标轴线
    axis.ticks = element_blank(),  # 删除坐标轴刻度线
    axis.text = element_blank(),   # 删除坐标轴文本（包括刻度文本）
    axis.title = element_blank()   # 删除坐标轴标题
  ) +
  annotate("text", x = -165, y = 75, label = "(c)", size = 5, colour = "black", fontface = "bold")

# 打印图像
print(p3)
