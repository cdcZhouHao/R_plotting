# 加载所需的库
library(readxl)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(RColorBrewer)

# 读取Excel文件中的五个工作表
data_A <- read_excel("C:/Rtraining/artical1/locate_species.xlsx", sheet = 1)
data_B <- read_excel("C:/Rtraining/artical1/locate_species.xlsx", sheet = 2)
data_C <- read_excel("C:/Rtraining/artical1/locate_species.xlsx", sheet = 3)
data_D <- read_excel("C:/Rtraining/artical1/locate_species.xlsx", sheet = 4)
data_E <- read_excel("C:/Rtraining/artical1/locate_species.xlsx", sheet = 5)

# 将数据转换成列表格式
data_sets <- list(data_A, data_B, data_C, data_D, data_E)

# 获取所有唯一的标签
all_labels <- unique(unlist(lapply(data_sets, function(data_set) data_set$labels)))

# 定义新的调色板名称
new_palette_names <- c("Accent", "Dark2", "Pastel1", "Paired", "Set1")

# 初始化一个空向量来存储组合后的颜色
combined_colors <- c()

# 每个调色板中你想要的颜色数量
colors_per_palette <- ceiling(23 / length(new_palette_names))

# 遍历每个调色板，并提取所需数量的颜色
for (name in new_palette_names) {
  palette <- brewer.pal(n = colors_per_palette, name = name)
  combined_colors <- c(combined_colors, palette)
  if (length(combined_colors) >= 23) break  # 确保颜色总数不超过23种
}

# 截取前23种颜色
colors <- combined_colors[1:23]
names(colors) <- all_labels

# 绘制多个饼图，但不显示图例和标签名称
plots <- lapply(data_sets, function(data_set) {
  sizes <- data_set$sizes
  labels <- data_set$labels
  df <- data.frame(sizes = sizes, labels = factor(labels, levels = all_labels))
  
  p <- ggplot(df, aes(x = "", y = sizes, fill = labels)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = paste("Data Set", data_set$label[1])) +
    scale_fill_manual(values = colors) +
    theme(legend.position = "none")  # 不显示图例
  
  return(p)
})

# 创建一个共享图例
legend_plot <- ggplot(data.frame(labels = all_labels, sizes = rep(1, length(all_labels))),
                      aes(x = "", y = sizes, fill = labels)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "right")  # 仅显示图例

# 提取图例
legend <- cowplot::get_legend(legend_plot)

# 显示多个饼图和共享图例
grid.arrange(
  gridExtra::arrangeGrob(grobs = plots, ncol = length(data_sets)),
  legend,
  ncol = 2,
  widths = c(5, 1)
)


###########################################################
###气泡图
library(ggplot2)

# 创建数据框
data <- data.frame(
  x = c(3, 2, 3, 4, 1),
  y = c(32, 27, 20, 25, 22),
  bubble_sizes = c(148, 144, 36, 32, 15)
)

# 绘制气泡图
ggplot(data, aes(x = x, y = y, size = bubble_sizes)) +
  geom_point(shape = 1, color = "#011627") +  # 将形状设置为空心圆圈
  scale_size(range = c(15, 50)) +  # 设置气泡大小范围
  theme_classic() +
  xlim(0, 5) +  # 设置 x 轴范围
  ylim(15, 38)  # 设置 y 轴范围







