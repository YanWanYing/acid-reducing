rm(list=ls())
setwd("D:/毕业/方案二/有机肥降酸潜力/潜力/tif1")

# 重新加载所需包
library(ggplot2)
library(dplyr)
library(data.table)

# 读取数据
d1 <- readxl::read_xlsx("D:/毕业/方案二/data/souredata.xlsx", sheet = "Sheet2")
d1 <- as.data.table(d1)
d1
# set moderater levels
d1$moderator <- factor(d1$moderator,
                       levels = rev(c("ph", "soc", "tn", "ap", "clay",
                                      "map", "mat", "time", "dosages",
                                      "typescof", "typesfm", "typesgm", "typeshaof",
                                      "cropland1", "tillage1", "clusterZone2", "clusterZone3",
                                      "ph:time", "soc:dosages",
                                      "tn:typesfm", "tn:typesgm", "tn:typeshaof")))


# 先为显著性标记添加一个新列
d1$significance <- ifelse(d1$pval <= 0.001, "***",
                          ifelse(d1$pval <= 0.01, "**",
                                 ifelse(d1$pval <= 0.05, "*", "")))

p1 <- ggplot(d1, aes(estimate, factor(moderator, levels = rev(levels(factor(moderator)))))) +
  geom_col(fill = "#BE9612") +  # 设置柱状图颜色
  geom_errorbar(aes(xmin = estimate - se, xmax = estimate + se), width = 0.2) +  # 添加误差线
  geom_text(aes(label = significance),
            position = position_dodge(0.8), size = 5) +  # 添加显著性标记，控制位置在误差线外
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12, angle = 45),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)) +
  scale_x_continuous(limits = c(-0.45, 0.6), breaks = c(-0.4, -0.2, 0,  0.2, 0.4, 0.6)) +
  scale_y_discrete(limits= rev(c("tn:typeshaof", "tn:typesgm", "tn:typesfm", "soc:dosages", "ph:time", 
                                 "clusterZone3", "clusterZone2", "tillage1", "cropland1", 
                                 "typeshaof", "typesgm", "typesfm", "typescof", "dosages", "time", 
                                 "mat", "map", "clay", "ap", "tn", "soc", "ph")),
                   labels = rev(c("TN:Types_haof", "TN:Types_gm", "TN:Types_fm", "SOC:Dosages", "pH:Time",
                                  "Cluster_Zone3", "Cluster_Zone2", "Tillage_rotation", "Cropland_paddy",
                                  "Types_haof", "types_gm", "Types_fm","Types_cof", "Treatment dosages", "Treatment time",
                                  "MAT", "MAP", "Clay", "AP", "TN", "SOC", "pH"))) +
  xlab("Parameter estimate") +
  ylab("")+
  coord_flip()
p1

pdf("D:/毕业/方案二/Figure/Figure/加入分区潜力的新图（Figure1没改）/Figure2.pdf", height = 8, width = 16)  # 设置文件名和图像尺寸
p1
dev.off()