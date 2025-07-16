###################Figure4####################
rm(list=ls())
library(ggplot2)
setwd("D:/毕业/方案二/优先区")
library(raster)

priority_df <- terra::rast("priority.tif")
# 将SpatRaster转换为data.frame
priority_df.1 <- as.data.frame(priority_df, xy = TRUE)

# 获取基础世界地图
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")

p <- ggplot() +
  geom_sf(data = world, fill = "white", color = "black") +
  geom_raster(data = priority_df.1, aes(x = x, y = y, fill = class)) +
  scale_fill_manual(
    values = c("100%" = "#5180A7", 
               "85%" = "#7B9BAF", 
               "70%" = "#AABCB7", 
               "55%" = "#CED7B8",
               "40%" = "#9BC392",
               "30%" = "#F8E8B8",
               "20%" = "#F1C47E",
               "15%" = "#D74F23",
               "10%" = "#D12C27",
               "5%" = "#AC1F24"),
    limits = c("100%", "85%", "70%", "55%", "40%", "30%", "20%", "15%", "10%", "5%")) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.12),
    legend.justification = c(0, 0),
    legend.spacing.y = unit(c(0.2, 0.2, 1, 1, 1), "cm"),  # 调整条目间距
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(title = "Global")
p
