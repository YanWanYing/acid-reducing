# library(doParallel)
# registerDoParallel(96)
# # 重启 R 会话（这会清除所有缓存）
# .rs.restartR()
# 清空工作空间并加载必要的包
rm(list = ls())
library(data.table)
library(metafor)
library(metagear)

# 设置工作目录
setwd("D:/毕业/方案二/data")

# 读取数据
data <- read.csv("data_Class.csv")

# 加载 dplyr 包
library(dplyr)

# 删除 'cluster' 列为 'Zone4' 的行
data <- data %>%
  filter(cluster != "Zone4")

# 查看处理后的数据
table(data$cluster)



combine.dummy <- function(x, lab) {
  ty <- lab[as.logical(x)]
  if (length(ty) == 0) ty <- NA
  ty
}

data$types <- apply(data[,c("fm", "gm", "cof", "haof")], 1, combine.dummy, c("fm", "gm", "cof", "haof"))

data$cropland <- as.factor(data$cropland)
data$tillage <- as.factor(data$tillage)
data$studyid <- as.factor(data$studyid)
data$types <- as.factor(data$types)
data$cluster <- as.factor(data$cluster)

data <- data.frame(data$studyid,data$ph,data$soc,data$tn,data$ap,data$clay,data$map,data$mat,
                   data$cropland,data$tillage,data$types,data$time,data$dosages,data$cluster,data$yi,data$vi)
colnames(data)<-c("studyid","ph","soc","tn","ap","clay","map","mat",
                  "cropland","tillage","types","time","dosages","cluster","yi","vi")
data <- na.omit(data)
data <- as.data.frame(data)

var.site <- c('ph','soc','tn','ap','clay','map','mat',"cluster")
var.crop <- c('cropland','tillage')
var.trea <- c("types","time","dosages")

var.sel <- c(var.trea,var.crop,var.site)
setDT(data)


# 基础模型：r_n2o_0
r_n2o_0 <- rma.mv(yi, vi, data = data, random = list(~ 1 | studyid), method = "REML", sparse = TRUE)

# 存储结果的列表
out1.est = out1.sum = list()

# 遍历变量集合进行分析
for(i in var.sel) {
  
  # 检查变量类型（是否为字符型）
  vartype = is.character(data[[i]])  # 使用 data[[i]] 访问列
  
  if(vartype == TRUE) {
    # 对于字符型（因子）变量，转换为因子并进行回归分析
    r_n2o_1 <- rma.mv(yi, vi, 
                      mods = ~factor(data[[i]]) - 1,  # 使用 factor() 处理类别变量
                      data = data[, .(yi, vi, studyid, varsel = data[[i]])], 
                      random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
    
  } else {
    # 对于数值型变量，直接进行回归分析
    r_n2o_1 <- rma.mv(yi, vi, 
                      mods = ~data[[i]],  # 数值型变量直接使用
                      data = data[, .(yi, vi, studyid, varsel = data[[i]])], 
                      random = list(~ 1 | studyid), method = "REML", sparse = TRUE)
  }
  
  # 存储回归结果
  out1.est[[i]] <- data.table(var = i,
                              varname = gsub('factor\\(varsel\\)', '', rownames(r_n2o_1$b)),
                              mean = round(as.numeric(r_n2o_1$b), 3),
                              se = round(as.numeric(r_n2o_1$se), 3),
                              ci.lb = round(as.numeric(r_n2o_1$ci.lb), 3),
                              ci.ub = round(as.numeric(r_n2o_1$ci.ub), 3),
                              pval = round(as.numeric(r_n2o_1$pval), 3))
  
  # 存储模型统计量
  out1.sum[[i]] <- data.table(var = i,
                              AIC = r_n2o_1$fit.stats[4, 2],
                              ll = r_n2o_1$fit.stats[1, 2],
                              ll_impr = round(100 * (1 - r_n2o_1$fit.stats[1, 2] / r_n2o_0$fit.stats[1, 2]), 2),
                              r2_impr = round(100 * max(0, (sum(r_n2o_0$sigma2) - sum(r_n2o_1$sigma2)) / sum(r_n2o_0$sigma2)), 2),
                              pval = round(anova(r_n2o_1, r_n2o_0)$pval, 3))
}

# 合并并显示结果
out1.sum <- rbindlist(out1.sum)
out1.est <- rbindlist(out1.est)

# 打印结果
print(out1.sum)
print(out1.est)


estats <- function(model_new,model_base){
  out <- data.table(AIC = model_new$fit.stats[4,2],
                    ll = model_new$fit.stats[1,2],
                    ll_impr = round(100 * (1-model_new$fit.stats[1,2]/model_base$fit.stats[1,2]),2),
                    r2_impr = round(100*max(0,(sum(model_base$sigma2)-sum(model_new$sigma2))/sum(model_base$sigma2)),2),
                    pval = round(anova(r_n2o_1,r_n2o_0)$pval,3))
  return(out)
}

r_nue_0 <- rma.mv(yi,vi, data = data,random= list(~ 1|studyid), method="REML",sparse = TRUE)

m1 <- rma.mv(yi,vi,
             mods = ~ph + soc + tn + ap + clay + map + mat +time + dosages + types +
               cropland + tillage + cluster + ph:time + soc:dosages + tn:types - 1,
             data = data,
             random = list(~ 1|studyid), 
             method="REML",
             sparse = TRUE)
summary(m1)
a<-coef(summary(m1))
#View(a)



# # 模型比较（需 ML 拟合）
# r_n2o_0_ML <- update(r_n2o_0, method = "ML")
# m1_ML <- update(m1, method = "ML")
# anova_result <- anova(r_n2o_0_ML, m1_ML)
# print(anova_result)
# 
# # 信息准则
# cat("AIC:", AIC(m1), "\nBIC:", BIC(m1), "\n")
# 
# # 残差诊断
# par(mfrow = c(2, 2))
# std_resid <- rstandard(m1)$z
# qqnorm(std_resid, main = "Q-Q Plot of Residuals")
# qqline(std_resid, col = "red")
# plot(fitted(m1), std_resid, xlab = "Fitted Values", ylab = "Residuals")
# abline(h = 0, col = "red")
# ranef_m1 <- ranef(m1)$studyid$intrcpt
# qqnorm(ranef_m1, main = "Q-Q Plot of Random Effects")
# qqline(ranef_m1, col = "red")
# par(mfrow = c(1, 1))
# 
# k <- m1$k
# wi <- 1/m1$vi
# vt <- (k-1) / (sum(wi) - sum(wi^2)/sum(wi))
# PR2 <- r_nue_0$sigma2 / (sum(m1$sigma2) + vt)
# PR2


dt.new <- read.csv("predict.csv")
dt.new <- dt.new %>%
  filter(cluster != "Zone4")


dt.new$time_scaled <- 30

dt.new$cropland <- as.factor(dt.new$cropland)
dt.new$tillage <- as.factor(dt.new$tillage)
dt.new$cluster <- as.factor(dt.new$cluster)

dt.new<-data.frame(dt.new$lon,dt.new$lat,dt.new$pH,dt.new$SOC,dt.new$TN,dt.new$AP,dt.new$clay,dt.new$mat,dt.new$map,dt.new$cropland,dt.new$tillage,dt.new$time_scaled,dt.new$cluster)
colnames(dt.new)<-c("lon","lat","ph","soc","tn","ap","clay","map","mat","cropland","tillage","time","cluster")
dt.new <- na.omit(dt.new)

# 计算每个 types 对应的 25%-75% 分位范围
fm_quantiles <- quantile(data[data$types == "fm", "dosages"], probs = c(0.25, 0.75), na.rm = TRUE)
print(fm_quantiles)

gm_quantiles <- quantile(data[data$types == "gm", "dosages"], probs = c(0.25, 0.75), na.rm = TRUE)
print(gm_quantiles)

cof_quantiles <- quantile(data[data$types == "cof", "dosages"], probs = c(0.25, 0.75), na.rm = TRUE)
print(cof_quantiles)

haof_quantiles <- quantile(data[data$types == "haof", "dosages"], probs = c(0.25, 0.75), na.rm = TRUE)
print(haof_quantiles)

types_values <- c("fm", "gm", "cof", "haof")

dosages_values <- list(
  fm = seq(from = fm_quantiles[1], to = fm_quantiles[2], by = 50),
  gm = seq(from = gm_quantiles[1], to = gm_quantiles[2], by = 50),
  cof = seq(from = cof_quantiles[1], to = cof_quantiles[2], by = 50),
  haof = seq(from = haof_quantiles[1], to = haof_quantiles[2], by = 50)
)


dosages_values <- unlist(dosages_values)
types_values <- gsub("([[:digit:]]+)", "", names(unlist(dosages_values)))

types_dosages <- data.frame(types = types_values, dosages = dosages_values)
types_dosages$types <- factor(types_dosages$types, levels = levels(data$types))

pred.fun <- function(i) {
  xx <- data.frame(dt.new[i,], types_dosages, row.names = NULL)
  
  # 创建新模型矩阵
  newmods <- model.matrix(~ph + soc + tn + ap + clay + map + mat + time + dosages + types +
                            cropland + tillage + cluster + ph:time + soc:dosages + tn:types - 1,
                          data = xx)
  
  # 获取 yi 的预测值
  p_new <- predict(m1, newmods = newmods)$pred
  
  # 将 yi 转换为 (exp(yi) - 1) * 100
  p_new_transformed <- (exp(p_new) - 1) * 100
  
  # 过滤超过1的预测值，并保留原始索引
  valid_idx <- which(p_new_transformed <= 100)
  p_valid <- p_new_transformed[valid_idx]
  
  # 如果没有有效值，返回NA
  if (length(p_valid) == 0) {
    return(data.frame(
      lat = dt.new$lat[i], lon = dt.new$lon[i],
      ph = dt.new$ph[i], cluster = dt.new$cluster[i],
      potential_025 = NA, best_type_025 = NA, best_dose_025 = NA,
      potential_median = NA, best_type_50 = NA, best_dose_50 = NA,
      potential_975 = NA, best_type_975 = NA, best_dose_975 = NA,
      potential_max = NA, best_type_max = NA, best_dose_max = NA
    ))
  }
  
  # 计算分位数（基于有效值）
  p_quantiles <- quantile(p_valid, probs = c(0.025, 0.5, 0.975, 1), na.rm = TRUE)
  
  # 定义查找函数：在有效值中找最近分位数的原始索引
  find_origin_idx <- function(q) {
    idx_in_valid <- which.min(abs(p_valid - q))
    valid_idx[idx_in_valid]  # 映射回原始索引
  }
  
  # 获取各分位数对应的原始索引
  idx_025 <- find_origin_idx(p_quantiles[1])
  idx_50 <- find_origin_idx(p_quantiles[2])
  idx_975 <- find_origin_idx(p_quantiles[3])
  idx_max <- find_origin_idx(p_quantiles[4])  # 最大值已确保<=1
  
  # 提取组合信息
  extract_comb <- function(idx) {
    if (is.na(idx)) {
      list(type = NA, dose = NA)
    } else {
      list(
        type = types_dosages$types[idx],
        dose = types_dosages$dosages[idx]
      )
    }
  }
  
  # 构建结果
  data.frame(
    lat = dt.new$lat[i], lon = dt.new$lon[i],
    ph = dt.new$ph[i], cluster = dt.new$cluster[i],
    potential_025 = p_quantiles[1],
    best_type_025 = extract_comb(idx_025)$type,
    best_dose_025 = extract_comb(idx_025)$dose,
    potential_median = p_quantiles[2],
    best_type_50 = extract_comb(idx_50)$type,
    best_dose_50 = extract_comb(idx_50)$dose,
    potential_975 = p_quantiles[3],
    best_type_975 = extract_comb(idx_975)$type,
    best_dose_975 = extract_comb(idx_975)$dose,
    potential_max = p_quantiles[4],
    best_type_max = extract_comb(idx_max)$type,
    best_dose_max = extract_comb(idx_max)$dose
  )
}



res <- foreach(b =1:nrow(dt.new), .packages = "metafor") %dopar% pred.fun(b)
res <- do.call(rbind, res)
write.csv(res,"res.csv")

