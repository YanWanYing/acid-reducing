library(readxl)
# 读取数据
data <- read_excel('D:/毕业/桑基图.xlsx',sheet = "one")
data

# 安装和载入需要的R软件包
#install.packages("networkD3")
library(networkD3)
library(dplyr)

# 修改为符合networkD3软件包要求的数据格式
data <- data %>% mutate_if(is.character, as.factor)
str(data)

# 创建节点名称数据框
nodes <- data.frame(node = c(data$From, data$To)) %>% unique()
data$ID_AA <- match(data$From, nodes$node) - 1
data$ID_BB <- match(data$To, nodes$node) - 1
# 查看数据
head(data)

#核心的绘图参数如下
p4<-sankeyNetwork(Links = data, Nodes = nodes,  
                  Source = "ID_AA", # 来源变量 
                  Target = "ID_BB",# 接受变量 
                  Value = "Value",# 关系权重  
                  NodeID = "node",#节点名称 
                  fontSize= 15, nodeWidth = 30)#设置节点文本标签字体以及节点宽度


p4

p4 <- ggplot() + 
  geom_blank() + 
  theme_void()  # 使用theme_void()创建一个完全空白的主题
p4
