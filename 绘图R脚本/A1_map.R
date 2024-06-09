rm(list = ls())
library(tidyverse)
library(sf)
library(ggspatial)
map <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json")


ggplot(map)+#数据
  geom_sf(color='black',#线条颜色
          fill=NA,#填充色
          size=0.8)+#地图线条粗细
  annotation_scale(location = "bl", width_hint = 0.3) +#添加比例尺并调整位置及长度
  annotation_north_arrow(location = "tl", which_north = F, 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"))+#添加指北针，指北针类型style有north_arrow_orienteering；north_arrow_fancy_orienteering；north_arrow_minimal与north_arrow_nautical四种类型
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+#坐标参考系统(CRS)
  theme(text = element_text(size = 14,face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(color = "grey",size=0.2),
        axis.line = element_blank())+
  labs(x='', y='')
#################################
library(sf)
library(here)
library(tidyverse)
library(ggspatial)
#加载数据
QTP <- 'C:/Rtraining/DBATP/DBATP_Polygon.shp'
QTP_pro <- sf::read_sf(QTP)
border <- 'C:/Rtraining/DBATP/DBATP_Line.shp'
border_line <- sf::read_sf(border)
#绘制简单地图
china_QTP <- ggplot() +
  
  geom_sf(data = QTP_pro) +
  
  geom_sf(data = border_line)

china_QTP
#比例尺和指北针的添加
library(ggspatial)

china_QTP_gg <- ggplot() +
  
  geom_sf(data = QTP_pro) +
  
  geom_sf(data = border_line) +
  
  annotation_scale(location = "bl") +
  
  # spatial-aware automagic north arrow
  
  annotation_north_arrow(location = "tl", which_north = "true",
                         
                         style = north_arrow_fancy_orienteering)+
  
  labs(x ='Longitude',y="Latitude",
       
       title = "Scale bar and North arrow Test",
       
       subtitle = "ggspatial make")

china_QTP_gg
#投影
QTP_shp_pro <- st_transform(QTP_pro, 2343)

border_line_pro <- st_transform(border_line, 2343)

#添加字体
windowsFonts(
  
  Cinzel = windowsFont("Cinzel"),#这里使用的是字体的主题名称
  
  Poppins = windowsFont("Poppins"),
  
  IBMPSBold = windowsFont("IBMPlexSans-Bold"),
  
  Roboto_Mono = windowsFont("Roboto Mono"),
  
  Open_Sans = windowsFont("Open Sans"),
  
  Open_Sans_ExtraBold = windowsFont("Open Sans ExtraBold"),
  
  Times_New_Roman = windowsFont("Times New Roman")
  
)

#绘图
QTP_map_pro <- ggplot() +
  
  geom_sf(data = QTP_shp_pro)+
  
  geom_sf(data = border_line_pro)+
  
  annotation_scale(location = "bl") +
  
  
  
  annotation_north_arrow(location = "tl",
                         
                         style = north_arrow_fancy_orienteering)+
  
  labs(x ='Longitude',y="Latitude",
       
       title = "Scale bar and North arrow Test",
       
       subtitle = "ggspatial make 2343",
       
       caption = '生态R学社')+
  
  theme_bw()+
  
  theme(text = element_text(family = "Times_New_Roman",face='bold'),
        
        axis.text = element_text(family = 'Times_New_Roman',size = 14,face = 'bold'),
        
        axis.title.x = element_text(family = 'Times_New_Roman',size = 16,face = 'bold'),
        
        axis.title.y = element_text(family = 'Times_New_Roman',size = 16,face = 'bold'),
        
        axis.ticks.length=unit(0.2, "cm"),
        
        plot.background = element_rect(color = "white"),
        
        axis.ticks = element_line(size = .8))

QTP_map_pro

####################################
#指北针样式
#north_arrow_minimal: 简约风格的指北针。
#north_arrow_fancy_orienteering: 复杂的探险风格指北针
#north_arrow_orienteering: 标准的探险风格指北针
#north_arrow_nautical: 航海风格的指北针
library(ggspatial)
library(sf)
library(tidyverse)
library(cowplot)

# 读取中国省级行政区划数据和九段线数据并设置为WGS84地理坐标系
country <- read_sf('C:/Rtraining/china_map/边界线_省级_九段线.shp')
country <- country %>% st_transform(4326)

# 读取青藏高原数据并设置为WGS84地理坐标系
QT_shp <- read_sf("C:/Rtraining/DBATP/DBATP_Polygon.shp")
QT_shp_Projected <- QT_shp %>% st_transform(4326)
# 创建包含位点信息的数据框
points <- data.frame(
  x = c(97.36, 98.09, 98.48, 94.92, 98.10),
  y = c(37.36, 36.30, 36.92, 36.40, 32.97),
  site = c("Site1", "Site2", "Site3", "Site4", "Site5")
)

# 原始地图绘制代码
p1 <- ggplot() +
  geom_sf(data = country, color = "#686D76", size = 0.5, alpha = 0.8) +
  geom_sf(data = QT_shp_Projected, color = "white", fill = '#92C7CF', alpha = 0.5, size = 0.1) +
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_family = "sans",
    text_cex = 1,
    text_col = "black",
    height = unit(0.1, "cm"),
    width_hint = 0.1
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    pad_x = unit(0.2, "cm"),
    pad_y = unit(0.2, "cm"),
    style = north_arrow_fancy_orienteering,
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  annotate(
    "text",
    x = 130,
    y = 50,
    label = "指北针类型: fancy orienteering",
    size = 3,
    color = "black"
  ) +
  theme_classic() +
  coord_sf(xlim = c(73, 135), ylim = c(18, 55))

# 添加点标注，并按照不同位点设置不同颜色
p1 <- p1 +
  geom_point(data = points, aes(x = x, y = y, color = site), shape = 15,size = 3) +
  scale_color_manual(values = c("Site1" = "#B2A4FF", "Site2" = "#DD5746", "Site3" = "#FFB4B4", "Site4" = "#FFD966", "Site5" = "#4793AF"))

# 显示地图
print(p1)

#################################
# 加载所需的库
library(sf)
library(here)
library(tidyverse)
library(ggspatial)
library(readxl)
library(ggrepel)

# 加载数据
QTP <- 'C:/Rtraining/DBATP/DBATP_Polygon.shp'
QTP_pro <- sf::read_sf(QTP)
border <- 'C:/Rtraining/DBATP/DBATP_Line.shp'
border_line <- sf::read_sf(border)

# 从 Excel 文件中读取 points_df 数据
points_path <- 'C:/Rtraining/artical1/points.xlsx'
points_df <- read_excel(points_path)

# 转换为 sf 对象
points_sf <- st_as_sf(points_df, coords = c("longitude", "latitude"), crs = 4326)

# 定义23种不同的颜色
color_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                   "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5",
                   "#393b79", "#637939", "#8c6d31")

# 绘制地图，并填充颜色
china_QTP <- ggplot() +
  geom_sf(data = QTP_pro, color = "white", fill = '#92C7CF', alpha = 0.3) +   # 填充颜色为蓝色
  geom_sf(data = border_line, color = "#92C7CF") +
  geom_sf(data = points_sf, aes(color = category), size = 2) +  # 添加位点，按类别着色
  scale_color_manual(values = color_palette) +  # 使用自定义调色板
  geom_text_repel(data = points_df, aes(x = longitude, y = latitude, label = category, color = category), 
                  size = 4, max.overlaps = Inf,fontface = "bold.italic", force = 100) +  # 增加 max.overlaps 参数
  theme_classic() +  # 移除所有背景和坐标轴
  theme(
    axis.title = element_text(size = 14, color = "black"),  # 坐标轴标题样式
    axis.text = element_text(size = 12, color = "black"),   # 坐标轴标签样式
    axis.ticks = element_line(color = "black", size = 0.5)  # 坐标轴刻度样式
  )

# 显示地图
print(china_QTP)

#####################################青藏高原地图
# 加载必要的库
library(tidyverse)
library(ggspatial)
library(sf)
library(readxl)
library(ggrepel)

# 加载数据
QTP <- 'C:/Rtraining/DBATP/DBATP_Polygon.shp'
QTP_pro <- sf::read_sf(QTP)
border <- 'C:/Rtraining/DBATP/DBATP_Line.shp'
border_line <- sf::read_sf(border)
map1 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/632802.json")
map2 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/513332.json")
map3 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/632822.json")
map4 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/632801.json")
map5 <- read_sf("https://geo.datav.aliyun.com/areas_v3/bound/632821.json")


# 绘制地图
ggplot() +
  # 绘制多边形图层
  geom_sf(data = QTP_pro, fill = "lightblue", color = "lightblue") +
  # 绘制边界线图层
  geom_sf(data = border_line, color = "white", size = 1) +
  # 添加地图图层
  geom_sf(data = map1, fill = NA, color = "white") +  # 修改此行来添加地图图层
  geom_sf(data = map2, fill = NA, color = "white") +
  geom_sf(data = map3, fill = NA, color = "white") +
  geom_sf(data = map4, fill = NA, color = "white") +
  geom_sf(data = map5, fill = NA, color = "white") +
  # 添加坐标参考系统 (CRS)
  coord_sf(crs = st_crs(QTP_pro)) +
  # 添加主题
  theme_classic() +
  # 添加比例尺
  annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    text_family = "sans",
    text_cex = 1,
    text_col = "black",
    height = unit(0.1, "cm"),
    width_hint = 0.1
  ) 

