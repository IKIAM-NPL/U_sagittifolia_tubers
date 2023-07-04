library(ggsci)
library(gridExtra)
library(ggpubr)
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
                                           c(1,  2),
                                           rep(3, 2),
                                           rep(3, 2),
                                           rep(3, 2),
                                           rep(4, 2),
                                           rep(4, 2),
                                           rep(4, 2)))
                      
figure_1 <- ggpubr::as_ggplot(Figure1)

ggsave(filename = "Plots/jpeg/Figure_1.jpeg", plot = figure_1,
      width = 140, height = 180, units = "mm", dpi = 300, scale = 2.5)


