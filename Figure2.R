################################################################################
rm(list=ls())
potential <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif1/potential2.tif")
# 将SpatRaster转换为data.frame
potential.1 <- as.data.frame(potential, xy = TRUE)

range(potential.1$potential2);mean(potential.1$potential2)

# 获取基础世界地图
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")

p2 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # 添加世界地图背景
  geom_tile(data = potential.1, aes(x = x, y = y,
                                    fill = cut(potential2, limits = c(-50, 100), breaks = c(-50, 0, 25, 50, 75, 100),
                                               labels = c('<0', '0-25', '25-50', '50-75', '75-100')))) +
  scale_fill_manual(values = c("#f8766d", "#7Cae00", "#00bfc4", "#c77cff", "#c49a00"), drop = FALSE) +
  labs(fill = 'Relative change of pH (%)') +  # 手动设置填充色的色阶，不丢弃任何级别
  coord_sf(expand = FALSE) +
  theme_minimal() +  # 更换主题
  theme(
    legend.position = c(0.00, 0.10),  # 设置图例位置为左下角
    legend.justification = c(0, 0),  # 设置图例对齐为左下角
    legend.key.size = unit(0.8, "cm"),
    panel.grid = element_blank(),  # 删除网格线
    axis.line = element_blank(),  # 删除坐标轴线
    axis.ticks = element_blank(),  # 删除坐标轴刻度线
    axis.text = element_blank(),   # 删除坐标轴文本（包括刻度文本）
    axis.title = element_blank()   # 删除坐标轴标题
  )+
  ggtitle("Global") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))+
  annotate("text",x=0.5,y=-50,label="Mean: 39.7%",size=5, colour="#0070C0",fontface = "bold")
p2
