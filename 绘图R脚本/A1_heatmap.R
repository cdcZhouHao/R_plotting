

    
################################################################################
    library(readxl)
    # 加载数据
    data <- read_excel("C:/Rtraining/article/virus_abundance.xlsx")
    metadata <- read_excel("C:/Rtraining/article/virus_info.xlsx")
    metadata2 <- read_excel("C:/Rtraining/article/viruinfo2.xlsx")
    
    # 数据转换
    data[is.na(data)] <- 0
    data <- column_to_rownames(data, "Viruses")
    
    # 加载软件包
    library(tidyverse)
    library(ComplexHeatmap)
    library(circlize)
    library(RColorBrewer)
    display.brewer.all()
    
    # 设置行和列分割的间隔
    row_gap <- unit(0.5, "mm")  # 设置行分割间隔为0.5毫米，可以根据需要调整
    column_gap <- unit(0.5, "mm")  # 设置列分割间隔为0.5毫米，可以根据需要调整
    
    # 使用RColorBrewer设置自定义颜色
    species_palette <- colorRampPalette(brewer.pal(9, "Set3"))(17) # 生成17种颜色
    order_palette <- colorRampPalette(brewer.pal(8, "Set2"))(length(unique(metadata2$Order)))
    
    # 自定义class颜色
    class_colors <- c("Mammalia" = "pink", "Aves" = "cyan") # 根据实际class名称设置颜色
    
    # 创建颜色映射字典
    species_colors <- setNames(species_palette, unique(metadata2$species))
    order_colors <- setNames(order_palette, unique(metadata2$Order))
    
    # 自定义行注释颜色
    nucleic_acid_palette <- colorRampPalette(brewer.pal(3, "Dark2"))(length(unique(metadata$Nucleicacid_type)))
    host_palette <- colorRampPalette(brewer.pal(3, "Accent"))(length(unique(metadata$host)))
    
    nucleic_acid_colors <- setNames(nucleic_acid_palette, unique(metadata$Nucleicacid_type))
    host_colors <- setNames(host_palette, unique(metadata$host))
    
    # 设置列注释
    column_ha <- HeatmapAnnotation(
      species = metadata2$species,
      order = metadata2$Order,
      class = metadata2$Class,
      col = list(
        species = species_colors,
        order = order_colors,
        class = class_colors
      ),
      annotation_name_gp = gpar(fontsize = 8, fontface = 'italic'),
      simple_anno_size = unit(0.3, 'cm')
    )
    
    # 设置行注释
    row_ha <- rowAnnotation(
      nucleic_acid = metadata$Nucleicacid_type,
      host = metadata$host,
      col = list(
        nucleic_acid = nucleic_acid_colors,
        host = host_colors
      ),
      annotation_name_gp = gpar(fontsize = 8, fontface = 'italic'),
      simple_anno_size = unit(0.3, 'cm')
    )
    
    # 绘制热图
    
    p1 <- Heatmap(
      log10(data) + 2,
      show_column_names = FALSE,
      show_row_names = TRUE,
      column_split = factor(metadata2$Order),
      column_title = NULL,
      row_split = factor(metadata$Nucleicacid_type),
      row_title = NULL,
      name = "(log10)+2",
      row_names_gp = gpar(fontsize = 12, fontface = 'italic'),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      col = colorRamp2(
        breaks = c(1, 2, 3, 4, 5),
        colors = c('#F4F2ED', '#E1908F', '#DE6369', '#DC3345', '#DB0527')
      ),
      top_annotation = column_ha,
      row_gap = row_gap,
      left_annotation = row_ha,
      column_gap = column_gap
    )
    
    
    p3 <- Heatmap(
      log10(data) + 2,
      show_column_names = FALSE,
      show_row_names = TRUE,
      column_split = factor(metadata2$Class),
      column_title = NULL,
      row_split = factor(metadata$Nucleicacid_type),
      row_title = NULL,
      name = "(log10)+2",
      row_names_gp = gpar(fontsize = 12, fontface = 'italic'),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      col = colorRamp2(
        breaks = c(1, 2, 3, 4, 5),
        colors = c('#F4F2ED', '#E1908F', '#DE6369', '#DC3345', '#DB0527')
      ),
      top_annotation = column_ha,
      row_gap = row_gap,
      left_annotation = row_ha,
      column_gap = column_gap
    )
    
    
    p2 <-  Heatmap(
      log10(data) + 2,
      show_column_names = TRUE,
      show_row_names = TRUE,
      column_split = factor(metadata2$species),
      column_title = NULL,
      row_split = factor(metadata$Nucleicacid_type),
      row_title = NULL,
      name = "(log10)+2",
      row_names_gp = gpar(fontsize = 12, fontface = 'italic'),
      cluster_rows = FALSE,
      cluster_columns = FALSE,
      col = colorRamp2(
        breaks = c(1, 2, 3, 4, 5),
        colors = c('#F4F2ED', '#E1908F', '#DE6369', '#DC3345', '#DB0527')
      ),
      top_annotation = column_ha,
      row_gap = row_gap,
      left_annotation = row_ha,
      column_gap = column_gap
    )
    
  
    