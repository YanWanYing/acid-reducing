
#################################################################
rm(list=ls())
types <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif1/types2.tif")
types.1 <- as.data.frame(types, xy = TRUE)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 这里我们根据Zone1, Zone2, Zone3, Zone4来设置颜色，并调整图例顺序
p11 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # 添加世界地图背景
  geom_tile(data = types.1, aes(x = x, y = y, fill = best_type1)) +  # 使用CLUSTER列填充
  scale_fill_manual(
    values = c("fm" = "#f8766d", 
               "gm" = "#7Cae00", 
               "cof" = "#00bfc4", 
               "haof" = "#c77cff"),
    limits = c("fm", "gm", "cof", "haof")  # 设置图例的顺序
  ) +  # 手动设置每个Zone对应的颜色
  labs(fill = 'Fertilizer types') +  # 手动设置填充色的色阶，不丢弃任何级别
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
  ggtitle("Global") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p11



dosages <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif1/dosages2.tif")
dosages.1 <- as.data.frame(dosages, xy = TRUE)
range(dosages.1$dosages2)
table(dosages.1$dosages2)
median(dosages.1$dosages2)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 创建图
p12 <- ggplot() +
  # 添加世界地图背景
  geom_sf(data = world, fill = "white", color = "black") +
  geom_tile(data = dosages.1, aes(x = x, y = y, fill = dosages2)) + 
  # 配色方案
  scale_fill_gradient2(low = "#00bfc4", high = "#f8766d", mid = "#c49a00",
                       midpoint = 15000, space = "Lab", 
                       name = 'Dosages', na.value = 'transparent',
                       limits = c(1500, 25000)) +
  # 设置图例和标签
  labs(fill = 'Dosages') +
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
  ggtitle("Global") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p12


types <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif2/Zone1_types.tif")
types.1 <- as.data.frame(types, xy = TRUE)
types.1 <- na.omit(types.1)
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 这里我们根据Zone1, Zone2, Zone3, Zone4来设置颜色，并调整图例顺序
p21 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # 添加世界地图背景
  geom_tile(data = types.1, aes(x = x, y = y, fill = best_type1)) +  # 使用CLUSTER列填充
  scale_fill_manual(
    values = c("fm" = "#f8766d", 
               "gm" = "#7Cae00", 
               "cof" = "#00bfc4", 
               "haof" = "#c77cff"),
    limits = c("fm", "gm", "cof", "haof")  # 设置图例的顺序
  ) +  # 手动设置每个Zone对应的颜色
  labs(fill = 'Fertilizer types') +  # 手动设置填充色的色阶，不丢弃任何级别
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
  ggtitle("Zone1") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p21

dosages <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif2/Zone1_doses.tif")
dosages.1 <- as.data.frame(dosages, xy = TRUE)
range(dosages.1$Zone1_doses)
table(dosages.1$Zone1_doses)
median(dosages.1$Zone1_doses)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 创建图
p22 <- ggplot() +
  # 添加世界地图背景
  geom_sf(data = world, fill = "white", color = "black") +
  geom_tile(data = dosages.1, aes(x = x, y = y, fill = Zone1_doses)) + 
  # 配色方案
  scale_fill_gradient2(low = "#00bfc4", high = "#f8766d", mid = "#c49a00",
                       midpoint = 15000, space = "Lab", 
                       name = 'Dosages', na.value = 'transparent',
                       limits = c(1500, 25000)) +
  # 设置图例和标签
  labs(fill = 'Dosages') +
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
  ggtitle("Zone1") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p22


types <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif2/Zone2_types.tif")
types.1 <- as.data.frame(types, xy = TRUE)
types.1 <- na.omit(types.1)
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 这里我们根据Zone1, Zone2, Zone3, Zone4来设置颜色，并调整图例顺序
p31 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # 添加世界地图背景
  geom_tile(data = types.1, aes(x = x, y = y, fill = best_type1)) +  # 使用CLUSTER列填充
  scale_fill_manual(
    values = c("fm" = "#f8766d", 
               "gm" = "#7Cae00", 
               "cof" = "#00bfc4", 
               "haof" = "#c77cff"),
    limits = c("fm", "gm", "cof", "haof")  # 设置图例的顺序
  ) +  # 手动设置每个Zone对应的颜色
  labs(fill = 'Fertilizer types') +  # 手动设置填充色的色阶，不丢弃任何级别
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
  ggtitle("Zone2") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p31

dosages <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif2/Zone2_doses.tif")
dosages.1 <- as.data.frame(dosages, xy = TRUE)
range(dosages.1$Zone2_doses)
table(dosages.1$Zone2_doses)
median(dosages.1$Zone2_doses)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 创建图
p32 <- ggplot() +
  # 添加世界地图背景
  geom_sf(data = world, fill = "white", color = "black") +
  geom_tile(data = dosages.1, aes(x = x, y = y, fill = Zone2_doses)) + 
  # 配色方案
  scale_fill_gradient2(low = "#00bfc4", high = "#f8766d", mid = "#c49a00",
                       midpoint = 15000, space = "Lab", 
                       name = 'Dosages', na.value = 'transparent',
                       limits = c(1500, 25000)) +
  # 设置图例和标签
  labs(fill = 'Dosages') +
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
  ggtitle("Zone2") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p32


types <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif2/Zone3_types.tif")
types.1 <- as.data.frame(types, xy = TRUE)
types.1 <- na.omit(types.1)
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 这里我们根据Zone1, Zone2, Zone3, Zone4来设置颜色，并调整图例顺序
p41 <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +  # 添加世界地图背景
  geom_tile(data = types.1, aes(x = x, y = y, fill = best_type1)) +  # 使用CLUSTER列填充
  scale_fill_manual(
    values = c("fm" = "#f8766d", 
               "gm" = "#7Cae00", 
               "cof" = "#00bfc4", 
               "haof" = "#c77cff"),
    limits = c("fm", "gm", "cof", "haof")  # 设置图例的顺序
  ) +  # 手动设置每个Zone对应的颜色
  labs(fill = 'Fertilizer types') +  # 手动设置填充色的色阶，不丢弃任何级别
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
  ggtitle("Zone3") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p41

dosages <- terra::rast("D:/毕业/方案二/有机肥降酸潜力/潜力/tif2/Zone3_doses.tif")
dosages.1 <- as.data.frame(dosages, xy = TRUE)
range(dosages.1$Zone3_doses)
table(dosages.1$Zone3_doses)
median(dosages.1$Zone3_doses)
world <- ne_countries(scale = "medium", returnclass = "sf")
# 创建图
p42 <- ggplot() +
  # 添加世界地图背景
  geom_sf(data = world, fill = "white", color = "black") +
  geom_tile(data = dosages.1, aes(x = x, y = y, fill = Zone3_doses)) + 
  # 配色方案
  scale_fill_gradient2(low = "#00bfc4", high = "#f8766d", mid = "#c49a00",
                       midpoint = 15000, space = "Lab", 
                       name = 'Dosages', na.value = 'transparent',
                       limits = c(1500, 25000)) +
  # 设置图例和标签
  labs(fill = 'Dosages') +
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
  ggtitle("Zone3") +
  theme(plot.title = element_text(size = 16))+
  theme(plot.title = element_text(hjust = 0.5))
p42

pdf("D:/毕业/方案二/Figure/Figure/加入分区潜力的新图（Figure1没改）/附件图.pdf", height = 18, width = 16)  # 设置文件名和图像尺寸
grid.arrange(p11, p12, p21, p22, p31, p32, p41, p42, ncol = 2)
dev.off()
