#######################################Zone1#######################
rm(list=ls())
library(ggplot2)
library(data.table)
library(ggthemes)
setwd("D:/毕业/方案二/data")
mydata <- readxl::read_xlsx('souredata.xlsx',sheet = "Figure2")
mydata <- as.data.table(mydata)
mydata

mydata1 <- mydata[mydata$Area == "Zone1", ]
mydata1

# 重新定义 'Management' 的标签
mydata$Management <- factor(mydata$Management, 
                            levels = c("Humic acid organic fertilizer",
                                       "commodity organic fertilizer","green manure","farmyard manure","All"),
                            labels = c("HAOF","COF","GM", "FM","GE"))

library(ggplot2)
library(ggthemes)
library(grid)

p1<- ggplot(data = mydata1, aes(x = mean, y = Management)) +
  geom_errorbar(position = position_dodge(0.7), 
                aes(xmin = ci.lb, xmax = ci.ub), 
                width = 0.3, size = 0.8, color = "#1b9e77") +
  geom_point(size = 2, color = "#1b9e77") +
  scale_x_continuous(limits = c(-20, 40)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(y = Management, x = ci.ub +2, label = n),position = position_dodge(width = 0.7),
            vjust = 0, hjust=0.5, size = 4, check_overlap = FALSE)+
  scale_y_discrete(limits=c("Humic acid organic fertilizer",
                            "commodity organic fertilizer","green manure","farmyard manure","All"),
                   labels = c("HAOF","COF","GM", "FM","GE"))+
  theme_bw()+
  theme(legend.position = "none")+ #去掉图注
  theme_few()+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  #theme(axis.text.y = element_text(size = 20, color = "black"))+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank(),   # 去掉 x 轴标签
        axis.title.y = element_blank())   # 去掉 y 轴标签
p1


setwd("D:/毕业/方案二/data")
mydata <- readxl::read_xlsx('souredata.xlsx',sheet = "Figure3a")
mydata1 <- as.data.table(mydata)

# 首先对type进行排序
mydata1$type <- factor(
  mydata1$type,
  levels = rev(c("low MAT","medium MAT","high MAT","low MAP","medium MAP","high MAP",
                 "low clay","medium clay","high clay","low AP","medium AP","high AP",
                 "low TN","medium TN","high TN","low SOC","medium SOC","high SOC",
                 "weak acid","medium-strong acid","strong acid","super strong acid")),
  labels = rev(c("<10 ℃","10-25 ℃",">25 ℃","<500 mm","500-1000 mm",">1000 mm",
                 "<33.3 %","33.3-66.6 %",">66.6 %","<20 mg/kg","20-40 mg/kg",">40 mg/kg",
                 "<1.5 g/kg","1.5-2 g/kg",">2 g/kg","<10 g/kg","10-15 g/kg",">15 g/kg",
                 "5.5-6.5","5.0-5.5","4.5-5.0","0-4.5")))

mydata1 <- mydata1[mydata1$Area=="Zone1"]

# 创建 ggplot 图形
p2 <- ggplot(data = mydata1, aes(x = estimate, y = type)) +
  geom_errorbar(position = position_dodge(0.7), 
                aes(xmin = ci.lb, xmax = ci.ub), 
                width = 0.3, size = 0.8, color = "#1b9e77") +
  geom_point(size = 2, color = "#1b9e77") +
  scale_x_continuous(limits = c(-20, 40)) +
  scale_y_discrete(limits = rev) +  # 关键：反转坐标轴顺序
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme_bw() +
  theme_few() +
  geom_hline(yintercept = 3.5, color = "grey", linetype = "dashed")+
  geom_hline(yintercept = 6.5, color = "grey", linetype = "dashed")+
  geom_hline(yintercept = 9.5, color = "grey", linetype = "dashed")+
  geom_hline(yintercept = 12.5, color = "grey", linetype = "dashed")+
  geom_hline(yintercept = 15.5, color = "grey", linetype = "dashed")+
  geom_hline(yintercept = 18.5, color = "grey", linetype = "dashed")+
  geom_text(aes(y = type, x = ci.ub +2, label = n),position = position_dodge(width = 0.7),
            vjust = 0, hjust=0.5, size = 4, check_overlap = FALSE)+
  theme(legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.85, 0.85),
        legend.key = element_rect(fill = "white", size = 1.5),
        legend.key.width = unit(0.4, "lines"),
        legend.key.height = unit(0.5, "lines"),
        legend.background = element_blank(),
        #axis.text.y = element_text(size = 16, colour = 'black'),  # 只保留一个 axis.text.y
        axis.text.y = element_blank(), 
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12, colour = 'black', face = 'bold'),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, colour = 'black'))

# 显示图形
p2


mydata <- readxl::read_xlsx('souredata.xlsx',sheet = "Figure3b")
mydata1 <- as.data.table(mydata)

# 首先对type进行排序
mydata1$type <- factor(
  mydata1$type,
  levels = rev(c(">60", "45-60", "30-45", "22.5-30", "15-22.5", "7.5-15", "0-7.5",
                 "40-50 a", "30-40 a", "20-30 a", "10-20 a", "5-10 a", "1-5 a", "0-1 a", 
                 "rotation", "continuous", "paddy", "dryland")),
  labels = rev(c(">60 t/ha/a", "45-60 t/ha/a", "30-45 t/ha/a", "22.5-30 t/ha/a", "15-22.5 t/ha/a", "7.5-15 t/ha/a", "0-7.5 t/ha/a",
                 "40-50 a", "30-40 a", "20-30 a", "10-20 a", "5-10 a", "1-5 a", "0-1 a", 
                 "Rotation", "Continuous", "Paddy", "Dryland")))

mydata1 <- mydata1[mydata1$Area=="Zone1"]

# 创建 ggplot 图形
p3 <- ggplot(data = mydata1, aes(x = estimate, y = type)) +
  geom_errorbar(position = position_dodge(0.7), 
                aes(xmin = ci.lb, xmax = ci.ub), 
                width = 0.3, size = 0.8, color = "#1b9e77") +
  geom_point(size = 2, color = "#1b9e77") +
  scale_x_continuous(limits = c(-20, 40)) +
  scale_y_discrete(limits = rev) +  # 关键：反转坐标轴顺序
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme_bw() +
  theme_few() +
  geom_text(aes(y = type, x = ci.ub +2, label = n),position = position_dodge(width = 0.7),
            vjust = 0, hjust=0.5, size = 4, check_overlap = FALSE)+
  geom_hline(yintercept = 7.5, color = "grey", linetype = "dashed")+
  geom_hline(yintercept = 14.5, color = "grey", linetype = "dashed")+
  geom_hline(yintercept = 16.5, color = "grey", linetype = "dashed")+
  theme(legend.title = element_blank(),
        legend.direction = "vertical",
        legend.position = c(0.85, 0.85),
        legend.key = element_rect(fill = "white", size = 1.5),
        legend.key.width = unit(0.4, "lines"),
        legend.key.height = unit(0.5, "lines"),
        legend.background = element_blank(),
        axis.text.y = element_blank(), 
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 12, colour = 'black', face = 'bold'),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12, colour = 'black'),
  )

# 显示图形
p3

library(gridExtra)
left <- arrangeGrob(p1,p3, ncol = 1, heights= c(3,10))

# 导出到 PNG 文件
png("D:/毕业/方案二/Figure/newFigure1/Zone1.png", height = 20, width = 18, units = "cm", res = 600)  # 设置文件名和图像尺寸
grid.arrange(left, p2, ncol = 2)  # heights 参数设置图形的相对高度比例
dev.off()

pdf("D:/毕业/方案二/Figure/newFigure1/Zone1.pdf", height = 7, width = 5)  # 设置文件名和图像尺寸
grid.arrange(left, p2, ncol = 2)  # heights 参数设置图形的相对高度比例
dev.off()



