library(ggsci)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(readxl)
library(tidyverse)
library(ComplexHeatmap)
library(viridis)
library(gplots)
library(ggbeeswarm)
library(circlize)
# Figure size according to ASC
# https://pubs.acs.org/page/4authors/submission/graphics_prep.html


### Figure 1 -------

# Pos Scores
POS_scores <- POS_scores %>% mutate(Group = 
                        case_when(
                          Group %in% "A " ~ "Adult",
                          Group %in% "JP " ~ "Juvenile",
                          Group %in% "P " ~ "Seedling",
                          Group %in% "QC" ~ "Quality control"
                        ) ) %>% 
  mutate(Group = factor(Group, 
                        levels = c("Seedling", "Juvenile",
                                   "Adult", "Quality control")))

scores_post_plt <- ggplot(POS_scores, 
                          aes(PC1, PC2, shape = Group, color = Group)) +
  geom_point(size = 5) +
  guides(x=guide_axis(title = "PC 1 (41%)"),
         y=guide_axis(title = "PC 2 (20%)")) +
  labs(color = "Growth stage", shape = "Growth stage") +
  ggsci::scale_color_startrek() +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.4), 
        legend.background = element_rect(fill = "white", color = "black")) +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9)
scores_post_plt


# Pos Loadings

pos_loadings_plt <- ggplot(POS_loadings, aes(PC1, PC2)) + 
  geom_point(alpha = 0.05) +
  geom_point(data = POS_compouds_all,
             aes(shape = Identification_level, 
                 color = Identification_level),
             size = 3) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9)  +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  ggrepel::geom_label_repel(data = POS_compouds_all, aes(label = Compound),
                            box.padding = 0.3) +
  guides(x=guide_axis(title = "PC 1 (43%)"), y=guide_axis(title = "PC 2 (20%)")) +
  scale_color_manual(values = c("#EE0000FF", "#3B4992FF")) +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.85), 
        legend.background = element_rect(fill = "white", color = "black")) +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"))  +
  labs(color = "Identification\nlevel", shape = "Identification\nlevel")
pos_loadings_plt
# Neg scores

NEG_scores <- NEG_scores %>%  mutate(Group = 
                    case_when(
                      Group %in% "A " ~ "Adult",
                      Group %in% "JP " ~ "Juvenile",
                      Group %in% "P " ~ "Seedling",
                      Group %in% "QC" ~ "Quality control"
                    ) ) %>% 
  mutate(Group = factor(Group, 
                        levels = c("Seedling", "Juvenile",
                                   "Adult", "Quality control")))

scores_neg_plt <- ggplot(NEG_scores, 
       aes(PC1, PC2, shape = Group, color = Group)) +
  geom_point(size = 5) +
  guides(x=guide_axis(title = "PC 1 (43%)"),
         y=guide_axis(title = "PC 2 (20%)")) +
  labs(color = "Growth stage", shape = "Growth stage") +
  ggsci::scale_color_startrek() +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.4), 
        legend.background = element_rect(fill = "white", color = "black")) +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9)
scores_neg_plt

# Loadings neg plot

NEG_loadings_plt <- ggplot(NEG_loadings, aes(PC1, PC2)) + 
  geom_point(alpha = 0.05) +
  geom_point(data = NEG_compouds_all,
             aes(shape = Identification_level, 
                 color = Identification_level),
             size = 3) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9)  +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  ggrepel::geom_label_repel(data = NEG_compouds_all, aes(label = Compound),
                            box.padding = 0.3, max.overlaps = 50) +
  guides(x=guide_axis(title = "PC 1 (43%)"), y=guide_axis(title = "PC 2 (20%)")) +
  scale_color_manual(values = c("#EE0000FF", "#3B4992FF")) +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.85), 
        legend.background = element_rect(fill = "white", color = "black")) +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"))  +
  labs(color = "Identification\nlevel", shape = "Identification\nlevel")
NEG_loadings_plt

# Exporting plot
# Scores
scores_post_plt # Pos scores
scores_neg_plt # Scores neg
# Loadings
pos_loadings_plt # Pos loadings
NEG_loadings_plt # neg loadings


Figure1 <- arrangeGrob(scores_post_plt, scores_neg_plt,
                       pos_loadings_plt, NEG_loadings_plt,
                      layout_matrix =rbind(c(1,   2),
                                           c(1,  2),
                                           rep(3, 2),
                                           rep(3, 2),
                                           rep(4, 2),
                                           rep(4, 2),
                                           rep(4, 2),
                                           rep(4, 2)))
                      
figure_1 <- ggpubr::as_ggplot(Figure1) +
  draw_plot_label(label = LETTERS[1:4],
                  x = c(0, 0.5, 0, 0),
                  y = c(.99, .99, .75, 0.50))


# JPEG
ggsave(filename = "Plots/jpeg/Figure_1.jpeg", plot = figure_1,
       width = 140, height = 180, units = "mm", dpi = 300, scale = 2.15)

# PDF
ggsave(filename = "Plots/pdf/Figure_1.pdf", plot = figure_1,
      width = 140, height = 180, units = "mm", dpi = 300, scale = 2.5)


# Figure 2 -----

# Scores
GC_scores <- scores  %>% mutate(Group = 
                 case_when(
                   Group %in% "A" ~ "Adult",
                   Group %in% "JP" ~ "Juvenile",
                   Group %in% "P" ~ "Seedling",
                   Group %in% "QC" ~ "Quality control"
                 ) ) %>% 
  mutate(Group = factor(Group, 
                        levels = c("Seedling", "Juvenile",
                                   "Adult", "Quality control")))


scores_GC_plt <- ggplot(GC_scores, 
                          aes(PC1, PC2, shape = Group, color = Group)) +
  geom_point(size = 3) +
  guides(x=guide_axis(title = "PC 1 (43%)"),
         y=guide_axis(title = "PC 2 (20%)")) +
  labs(color = "Growth stage", shape = "Growth stage") +
  ggsci::scale_color_startrek() +
  theme_minimal() +
  theme(legend.position = c(0.15, 0.75), 
        legend.background = element_rect(fill = "white", color = "black")) +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9)
scores_GC_plt

# Loadings

loadings_GC_plt <- ggplot(loadings, aes(PC1, PC2)) + 
  geom_point(alpha = 0.2) +
  theme_classic() + 
  geom_point(data = EI_compouds_all, size = 1) +
  ggrepel::geom_label_repel(data = EI_compouds_all, aes(label = Compound), box.padding = 0.8, label.padding = 0.27, label.r = 0.3, cex = 3) +
  guides(x=guide_axis(title = "PC 1 (62 %)"), y=guide_axis(title = "PC 2 (15 %)")) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9)  +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9)  +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent"))


# Second_plot  ---
Figure2 <- arrangeGrob(scores_GC_plt, loadings_GC_plt)

figure_2 <- ggpubr::as_ggplot(Figure2) +
  draw_plot_label(label = c("A", "B"),
                  y = c(1, 0.55), 
                  x = c(0, 0))

ggsave(filename = "Plots/jpeg/Figure_2.jpeg", plot = figure_2,
       width = 3.25, height = 4, units = "in", dpi = 300, scale = 2)

#HeatMap plot
# https://jokergoo.github.io/ComplexHeatmap-reference/book/
set.seed(2023)
neg_heatmap_dt <- read_excel("Data/NEG_Metabolites_Hetmap.xlsx",
                             sheet = "To-ComplexHeatmap")

# Total phenol level
top_info <- data.frame(`Growth stage` = c( rep("Seedling", 3), 
                                       rep("Juvenile", 3), 
                                       rep("Adult", 3)) ) 
rownames(top_info) <- paste(top_info$Growth.stage, rep(c(1, 2, 3), 3))
top_info <- as.matrix(top_info)

cols_growth <- c(Seedling = "#CC0C00FF", Juvenile = "#5C88DAFF", Adult = "#84BD00FF")
top_info_ann <- HeatmapAnnotation(`Growth stage` = top_info,
                                  col = list(`Growth stage` = cols_growth),
                                  show_annotation_name = T,show_legend=F, 
                                  border = TRUE)

# Creating metabolite class
metabolite_class <- neg_heatmap_dt %>% 
  select(Classification = Superclass, Metabolite)
# pal_uchicago("default")(9)
cols_metClass <- c("Phenylpropanoids and polyketides" = "#800000FF",
                   "Organic oxygen compounds" = "#767676FF",
                   "Lipids and lipid-like molecules" = "#FFA319FF",
                   "Nucleosides, nucleotides, and analogues" = "#8A9045FF",
                   "Organic acids and derivatives" = "#155F83FF",
                   "Benzenoids" = "#C16622FF",
                   "Organoheterocyclic compounds" = "#725663FF",
                   "Lignans, neolignans and related compounds" = "#58593FFF",
                   "Alkaloids and derivatives" = "#9c7c46")
  
met_class_annotation <-  metabolite_class %>% select(Classification) %>% 
  as.matrix()
rownames(met_class_annotation) <- metabolite_class$Metabolite

row_annot <- rowAnnotation(Metabolite = met_class_annotation,
                           col = list(Metabolite = cols_metClass),
                           show_annotation_name = T,show_legend=F)

mycol <-colorRamp2(c(2, 4.5, 7), c("blue", "white", "#FF5A5A"))
neg_hm_mp <- neg_heatmap_dt[, 2:10] %>% as.matrix %>% log10()
rownames(neg_hm_mp) <- neg_heatmap_dt$Metabolite

# Scaling for 


neg_heatmap <- Heatmap(neg_hm_mp, col = mycol,
                       border_gp = grid::gpar(col = "black", lty = 1),
                       rect_gp = grid::gpar(col = "black", lwd = 0.75),
                       clustering_distance_columns = "euclidean",
                       clustering_method_columns = "complete",
                       right_annotation = row_annot, 
                       top_annotation = top_info_ann, 
                       show_heatmap_legend = F,
                       row_km = 3, column_km = 2)
neg_heatmap



# Pos heatmap 
pos_heatmap_dt <- read_xlsx("Data/POS_Metabolites_Hetmap.xlsx",
                            sheet = "To-ComplexHeatmap")

pos_hm_mp <- pos_heatmap_dt[, 2:10] %>% as.matrix %>% log10()
rownames(pos_hm_mp) <- pos_heatmap_dt$Metabolite

pos_metab_class <- pos_heatmap_dt %>% select(Classification = Superclass) %>% 
  as.matrix()
rownames(pos_metab_class) <- pos_heatmap_dt$Metabolite


pos_row_ann <- rowAnnotation(Metabolite = pos_metab_class,
                            col = list(Metabolite = cols_metClass),
                            show_annotation_name = T,show_legend=F)



pos_heatmap <- Heatmap(pos_hm_mp, col = mycol,
                       border_gp = grid::gpar(col = "black", lty = 1),
                       rect_gp = grid::gpar(col = "black", lwd = 0.75),
                       clustering_distance_columns = "euclidean",
                       clustering_method_columns = "complete",
                       top_annotation = top_info_ann, 
                       right_annotation = pos_row_ann,
                       show_heatmap_legend = F,
                       row_km = 3, column_km = 2 )
pos_heatmap

# Legend  log10 Abundance
lgd1 <- Legend(col_fun = mycol,
               title = "log10(Abundance)",
               at = seq(7), #legend_width = unit(6, "cm"), 
               direction = "horizontal" )

# Legend Growth stage
lgd2 <- Legend(labels = c("Seedling", "Juvenile", "Adult"),
               legend_gp = gpar(fill = cols_growth), title = "Growth stage")

# Legend metabolite class
lgd3 <- Legend(labels = c(unique(metabolite_class$Classification),"Alkaloid") ,
               legend_gp = gpar(fill = cols_metClass), 
               title = "Metabolite Classification", ncol = 3)

# Converting to ggplot
# Neg heatmap
gg_heatmap <- grid.grabExpr(draw(neg_heatmap))
gg_heatmap <- ggpubr::as_ggplot(gg_heatmap)
gg_heatmap

# Pos heatmap
gg_heatmap_pos <- grid.grabExpr(draw(pos_heatmap))
gg_heatmap_pos <- ggpubr::as_ggplot(gg_heatmap_pos)
gg_heatmap_pos


all_legends <- packLegend(lgd1, lgd2, lgd3, direction = "horizontal")
gg_legend <- grid.grabExpr(draw(all_legends))
gg_legend_fn <- ggpubr::as_ggplot(gg_legend)


buttom_heatmaps <- cowplot::plot_grid(gg_heatmap, gg_heatmap_pos,
                                      labels = c('A [M-H]-', "B [M+H]+"),
                                      nrow = 1,
                                      rel_widths = c(0.5, 0.45), 
                                      label_x = c(0, 0), 
                                      hjust =  c(-.5, .7) )
buttom_heatmaps

heatmaps_plot <- plot_grid(gg_legend_fn, buttom_heatmaps, ncol = 1,
                           rel_heights = c(0.12, 0.88))

heatmaps_plot

ggsave(filename = "Plots/jpeg/heatmaps.jpeg", plot = heatmaps_plot,
      width = 7.5, height = 6, units = "in", dpi = 600, scale = 1.7)


ggsave(filename = "Plots/pdf/heatmaps.pdf", plot = heatmaps_plot,
       width = 7.5, height = 6, units = "in", dpi = 600, scale = 1.7)
