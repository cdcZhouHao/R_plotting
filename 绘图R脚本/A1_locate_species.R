# 安装并加载必要的R包
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("RColorBrewer")

library(ggplot2)
library(dplyr)
library(RColorBrewer)

# 创建数据框
data <- data.frame(
  Location = c('Shiqu', 'Shiqu', 'Shiqu', 'Shiqu', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Dulan', 'Delinha', 'Delinha', 'Wulan', 'Wulan', 'Golmud', 'Golmud', 'Golmud', 'Golmud'),
  Category = c('Montifringilla taczanowskii', 'Ochotona curzoniae', 'Neodon fuscus', 'Cricetulus kamensis', 'Pseudois nayaur', 'Ovis ammon', 'Cervus albirostris', 'Marmota himalayana', 'Equus kiang', 'Canis lupus', 'Procapra picticaudata', 'Vulpes vulpes', 'Cervus canadensis', 'Lepus tibetanus', 'Gyps himalayensis', 'Alectoris chukar', 'Pyrrhocorax pyrrhocorax', 'Anser anser', 'Tadorna ferruginea', 'Gazella subgutturosa', 'Anser anser', 'Ursus arctos pruinosus', 'Cygnus atratus', 'Anas platyrhynchos', 'Canis lupus'),
  Count = c(10, 34, 100, 4, 38, 18, 11, 6, 17, 1, 17, 1, 5, 6, 8, 12, 4, 21, 15, 9, 23, 2, 6, 6, 1)
)

# 计算每个Location的总Count
data_summary <- data %>%
  group_by(Location) %>%
  summarise(TotalCount = sum(Count)) %>%
  arrange(desc(TotalCount))

# 按TotalCount的降序排列原数据框
data$Location <- factor(data$Location, levels = data_summary$Location)

# 生成 23 种颜色的调色板，使用多个调色板组合
base_colors <- c(brewer.pal(9, "Set1"), brewer.pal(8, "Dark2"), brewer.pal(12, "Set3"))[1:23]

# 设置透明度
alpha_value <- 0.7 # 设置透明度，可以根据需要调整

# 将透明度添加到颜色中
add_alpha <- function(color, alpha) {
  apply(sapply(color, col2rgb)/255, 2, function(x) 
    rgb(x[1], x[2], x[3], alpha = alpha))
}

colors <- add_alpha(base_colors, alpha_value)

# 绘制堆叠条形图
ggplot(data, aes(x = Location, y = Count, fill = reorder(Category, desc(Count)))) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) + # 添加透明度，并调整堆叠顺序
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5, reverse = TRUE), size = 3, alpha = alpha_value) + # 调整文本位置
  theme_classic() +
  labs(x = "Location", y = "Sample Size", title = "Sample Size by Location and Category") +
  scale_fill_manual(values = setNames(colors, unique(data$Category))) + # 设置图例中颜色的透明度
  guides(fill = guide_legend(override.aes = list(alpha = alpha_value))) + # 设置图例中颜色的透明度
  scale_y_continuous(expand = c(0, 0)) # 消除柱子与x轴之间的距离
