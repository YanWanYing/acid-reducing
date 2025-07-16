#加载本地数据
MAT<-rast("change.tif")
MAT.1 <- as.data.frame(MAT, xy = TRUE)

library(viridis)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(ggspatial)

# 获取世界地图数据
world <- ne_countries(scale = "medium", returnclass = "sf")

# 转换 MAT 为数据框
MAT.1 <- as.data.frame(MAT, xy = TRUE)

# 创建图
p2 <- ggplot() +
  # 添加世界地图背景
  geom_sf(data = world, fill = "white", color = "black") +
  # 使用 geom_tile 或 geom_raster 来绘制 MAT 数据
  geom_tile(data = MAT.1, aes(x = x, y = y, fill = change)) + 
  # 配色方案
  scale_fill_gradient2(low = "#00bfc4", high = "#f8766d", mid = "#c49a00", 
                       midpoint = 0, space = "Lab", 
                       name = 'Change', na.value = 'transparent') +
  # 设置图例和标签
  labs(fill = 'Change') +
  # 设置坐标系为全球
  coord_sf(expand = FALSE) +
  # 设置主题
  theme_minimal() +
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
  # 添加文本标签 "(b)"
  annotate("text", x = -165, y = 75, label = "(b)", size = 5, colour = "black", fontface = "bold")

# 打印图
print(p2)

# pdf("D:/毕业/方案二/Figure/newFigure1/FigureS13.pdf", height = 4, width = 7.09)  # 设置文件名和图像尺寸
# p2
# dev.off()