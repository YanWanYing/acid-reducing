rm(list=ls())
library(ggplot2)
library(terra)
library(sf)
setwd("D:/毕业/方案二/酸化现状及趋势")

# 加载本地数据
pH <- terra::rast("pH2020.tif")
print(pH)

# 将SpatRaster转换为data.frame
pH.1 <- as.data.frame(pH, xy = TRUE)

# 获取基础世界地图
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")

p1 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # 添加世界地图背景
  geom_tile(data = pH.1, aes(x = x, y = y,
                             fill = cut(pH2020, limits = c(0, 10), breaks = c(0, 4.5, 5.0, 5.5, 6.5, 10),
                                        labels = c('<4.5', '4.5-5.0', '5.0-5.5', '5.5-6.5', '>6.5')))) +
  scale_fill_manual(values = c("#f8766d", "#7Cae00", "#00bfc4", "#c77cff", "#c49a00"), drop = FALSE) +
  labs(fill = 'pH value') +  # 手动设置填充色的色阶，不丢弃任何级别
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
  )+
  annotate("text", x = -165, y = 75, label = "(a)", size = 5, colour = "black", fontface = "bold")

print(p1)

# pdf("D:/毕业/方案二/Figure/newFigure1/FigureS12.pdf", height = 4, width = 7.09)  # 设置文件名和图像尺寸
# p1
# dev.off()