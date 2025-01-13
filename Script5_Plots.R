### Script for paper 'Does concern regarding climate change impact subsequent mental health? A longitudinal analysis using data from the Avon 2Longitudinal Study of Parents and Children (ALSPAC)' - ALSPAC B-number B4572
### Script 5: Plotting of results
### Created 10/12/2024 by Dan Major-Smith
### R version 4.3.1

## A pre-registered analysis plan for this paper is available on the OSF: https://rr.peercommunityin.org/articles/rec?id=793


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("X:\\Studies\\RSBB Team\\Dan\\B4572 - Climate Concern")

#install.packages("tidyverse")

library(tidyverse)


##########################################################################################
#### Read in the RQ1 results data

res <- read_csv("RQ1_Results.csv")
head(res)

# Tidy the data
res <- res %>%
  mutate(Outcome = factor(Outcome, levels = c("Dep", "Anx", "WB"))) %>%
  mutate(Model = factor(Model, levels = c("Unadj", "Adj"))) %>%
  mutate(CCA_MI = factor(CCA_MI, levels = c("CCA", "MI"))) %>%
  mutate(Contrast = factor(Contrast, levels = c("Somewhat - No", "Very - No", "Very - Somewhat")))

head(res)
summary(res)


## MI results first
res_mi <- res %>%
  filter(CCA_MI == "MI")
head(res_mi)
summary(res_mi)

(p_mi <- ggplot(res_mi, aes(x = fct_rev(Contrast), y = Est, ymin = LCI, ymax = UCI,
                         col = fct_rev(Model), fill = fct_rev(Model))) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
  geom_point(size = 2, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                    labels = c("Adjusted", "Unadjusted")) +
  scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                     labels = c("Adjusted", "Unadjusted")) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1, 1.5, 2), limits = c(-1.25, 2.25)) +
  labs(x = "", y = "Mean difference in outcome") +
  coord_flip() +
  theme_bw() + 
  facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                              Anx = "Anxiety (GAD7)",
                                                              WB = "Well-being (WEMWBS)"))) +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), legend.title = element_text(size = 12),
        axis.title.x = element_text(size = 14), strip.background = element_blank(),
        strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))


# Save as PDF
pdf("./Results/RQ1_MIResults.pdf", width = 10, height = 6)
p_mi
dev.off()



## MI and CCA results together
head(res)
summary(res)

# Unadjusted results
res_unadj <- res %>%
  filter(Model == "Unadj")
head(res_unadj)
summary(res_unadj)

(p_mi_cca_unadj <- ggplot(res_unadj, aes(x = fct_rev(Contrast), y = Est, ymin = LCI, ymax = UCI,
                         col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("Multiple\nimputation", "Complete-case\nanalysis")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("Multiple\nimputation", "Complete-case\nanalysis")) +
    scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3, 4), limits = c(-3.5, 4.5)) +
    labs(x = "", y = "Mean difference in outcome (unadjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ1_CCAvsMIResults_unadj.pdf", width = 10, height = 6)
p_mi_cca_unadj
dev.off()


# Adjusted results
res_adj <- res %>%
  filter(Model == "Adj")
head(res_adj)
summary(res_adj)

(p_mi_cca_adj <- ggplot(res_adj, aes(x = fct_rev(Contrast), y = Est, ymin = LCI, ymax = UCI,
                                         col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("Multiple\nimputation", "Complete-case\nanalysis")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("Multiple\nimputation", "Complete-case\nanalysis")) +
    scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3, 4), limits = c(-3.5, 4.5)) +
    labs(x = "", y = "Mean difference in outcome (adjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ1_CCAvsMIResults_adj.pdf", width = 10, height = 6)
p_mi_cca_adj
dev.off()



##########################################################################################
#### Read in the RQ2 interaction results data

res <- read_csv("RQ2_Results_Interactions.csv")
head(res)

# Tidy the data
res <- res %>%
  mutate(Outcome = factor(Outcome, levels = c("Dep", "Anx", "WB"))) %>%
  mutate(Model = factor(Model, levels = c("Unadj", "Adj"))) %>%
  mutate(CCA_MI = factor(CCA_MI, levels = c("CCA", "MI_allInts", "SMCFCS"))) %>%
  mutate(Term = factor(Term, levels = c("Actions", "Efficacy"))) %>%
  mutate(Level = factor(Level, levels = c("Somewhat", "Very")))

head(res)
summary(res)


## Unadjusted and adjusted all interactions results for actions
res_mi_allInts_actions <- res %>%
  filter(CCA_MI == "MI_allInts" & Term == "Actions") %>%
  mutate(Level = recode(Level, "Somewhat" = "Somewhat concerned * Actions", "Very" = "Very concerned * Actions")) %>%
  mutate(Level = factor(Level, levels = c("Somewhat concerned * Actions", "Very concerned * Actions")))
head(res_mi_allInts_actions)
summary(res_mi_allInts_actions)

(p_mi_action <- ggplot(res_mi_allInts_actions, aes(x = fct_rev(Level), y = Est, ymin = LCI, ymax = UCI,
                            col = fct_rev(Model), fill = fct_rev(Model))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("Adjusted", "Unadjusted")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("Adjusted", "Unadjusted")) +
    scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5), limits = c(-0.8, 0.5)) +
    labs(x = "", y = "Interaction term coefficient") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))


# Save as PDF
pdf("./Results/RQ2_MIAllIntsResults_actions.pdf", width = 10, height = 6)
p_mi_action
dev.off()


## Unadjusted and adjusted all interactions results for actions
res_mi_allInts_efficacy <- res %>%
  filter(CCA_MI == "MI_allInts" & Term == "Efficacy") %>%
  mutate(Level = recode(Level, "Somewhat" = "Somewhat concerned * Efficacy", "Very" = "Very concerned * Efficacy")) %>%
  mutate(Level = factor(Level, levels = c("Somewhat concerned * Efficacy", "Very concerned * Efficacy")))
head(res_mi_allInts_efficacy)
summary(res_mi_allInts_efficacy)

(p_mi_efficacy <- ggplot(res_mi_allInts_efficacy, aes(x = fct_rev(Level), y = Est, ymin = LCI, ymax = UCI,
                                                   col = fct_rev(Model), fill = fct_rev(Model))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("Adjusted", "Unadjusted")) +
    scale_color_manual(values = c("red", "black"), guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("Adjusted", "Unadjusted")) +
    scale_y_continuous(breaks = c(-4, -3, -2, -1, 0, 1, 2, 3), limits = c(-4, 3)) +
    labs(x = "", y = "Interaction term coefficient") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))


# Save as PDF
pdf("./Results/RQ2_MIAllIntsResults_efficacy.pdf", width = 10, height = 6)
p_mi_efficacy
dev.off()



### CCA vs MI all interactions vs SMCFCS

## Unadjusted actions
res_unadj_actions <- res %>%
  filter(Model == "Unadj" & Term == "Actions") %>%
  mutate(Level = recode(Level, "Somewhat" = "Somewhat concerned * Actions", "Very" = "Very concerned * Actions")) %>%
  mutate(Level = factor(Level, levels = c("Somewhat concerned * Actions", "Very concerned * Actions")))
head(res_unadj_actions)
summary(res_unadj_actions)

(p_mi_cca_unadj_action <- ggplot(res_unadj_actions, aes(x = fct_rev(Level), y = Est, ymin = LCI, ymax = UCI,
                                     col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black", "blue"), 
                      guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_color_manual(values = c("red", "black", "blue"), 
                       guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2), limits = c(-3, 2)) +
    labs(x = "", y = "Interaction term coefficient (unadjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ2_CCAvsMIResults_unadj_actions.pdf", width = 10, height = 6)
p_mi_cca_unadj_action
dev.off()



## Adjusted actions
res_adj_actions <- res %>%
  filter(Model == "Adj" & Term == "Actions") %>%
  mutate(Level = recode(Level, "Somewhat" = "Somewhat concerned * Actions", "Very" = "Very concerned * Actions")) %>%
  mutate(Level = factor(Level, levels = c("Somewhat concerned * Actions", "Very concerned * Actions")))
head(res_adj_actions)
summary(res_adj_actions)

(p_mi_cca_adj_action <- ggplot(res_adj_actions, aes(x = fct_rev(Level), y = Est, ymin = LCI, ymax = UCI,
                                                        col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black", "blue"), 
                      guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_color_manual(values = c("red", "black", "blue"), 
                       guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_y_continuous(breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1), limits = c(-2, 1.1)) +
    labs(x = "", y = "Interaction term coefficient (adjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ2_CCAvsMIResults_adj_actions.pdf", width = 10, height = 6)
p_mi_cca_adj_action
dev.off()


## Unadjusted efficacy
res_unadj_efficacy <- res %>%
  filter(Model == "Unadj" & Term == "Efficacy") %>%
  mutate(Level = recode(Level, "Somewhat" = "Somewhat concerned * Efficacy", "Very" = "Very concerned * Efficacy")) %>%
  mutate(Level = factor(Level, levels = c("Somewhat concerned * Efficacy", "Very concerned * Efficacy")))
head(res_unadj_efficacy)
summary(res_unadj_efficacy)

(p_mi_cca_unadj_efficacy <- ggplot(res_unadj_efficacy, aes(x = fct_rev(Level), y = Est, ymin = LCI, ymax = UCI,
                                                        col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black", "blue"), 
                      guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_color_manual(values = c("red", "black", "blue"), 
                       guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_y_continuous(breaks = c(-10, -8, -6, -4, -2, 0, 2, 4, 6, 8, 10), limits = c(-10, 10)) +
    labs(x = "", y = "Interaction term coefficient (unadjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ2_CCAvsMIResults_unadj_efficacy.pdf", width = 10, height = 6)
p_mi_cca_unadj_efficacy
dev.off()


## Adjusted efficacy
res_adj_efficacy <- res %>%
  filter(Model == "Adj" & Term == "Efficacy") %>%
  mutate(Level = recode(Level, "Somewhat" = "Somewhat concerned * Efficacy", "Very" = "Very concerned * Efficacy")) %>%
  mutate(Level = factor(Level, levels = c("Somewhat concerned * Efficacy", "Very concerned * Efficacy")))
head(res_adj_efficacy)
summary(res_adj_efficacy)

(p_mi_cca_adj_efficacy <- ggplot(res_adj_efficacy, aes(x = fct_rev(Level), y = Est, ymin = LCI, ymax = UCI,
                                                           col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black", "blue"), 
                      guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_color_manual(values = c("red", "black", "blue"), 
                       guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("SMCFCS", "All Interactions", "CCA")) +
    scale_y_continuous(breaks = c(-4, -2, 0, 2, 4, 6, 8, 10), limits = c(-6, 10)) +
    labs(x = "", y = "Interaction term coefficient (adjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ2_CCAvsMIResults_adj_efficacy.pdf", width = 10, height = 6)
p_mi_cca_adj_efficacy
dev.off()



##########################################################################################
#### Read in the RQ2 main effects results data

res <- read_csv("RQ2_Results_MainEffects.csv")
head(res)

# Tidy the data
res <- res %>%
  mutate(Outcome = factor(Outcome, levels = c("Dep", "Anx", "WB"))) %>%
  mutate(Model = factor(Model, levels = c("Unadj", "Adj"))) %>%
  mutate(CCA_MI = factor(CCA_MI, levels = c("CCA", "MI", "MI_action_allInts", "MI_efficacy_allInts",
                                            "MI_action_SMCFCS", "MI_efficacy_SMCFCS"))) %>%
  mutate(Contrast = factor(Contrast, levels = c("Somewhat - No", "Very - No", "Very - Somewhat")))

head(res)
summary(res)


# Unadjusted results
res_unadj <- res %>%
  filter(Model == "Unadj")
head(res_unadj)
summary(res_unadj)

(p_mi_cca_unadj <- ggplot(res_unadj, aes(x = fct_rev(Contrast), y = Est, ymin = LCI, ymax = UCI,
                                         col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black", "blue", "green", "orange", "purple"), 
                      guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("MI efficacy (SMCFCS)", "MI efficacy (all ints)", 
                                 "MI actions (SMCFCS)", "MI actions (all ints)", 
                                 "MI  (no interactions)", "CCA")) +
    scale_color_manual(values = c("red", "black", "blue", "green", "orange", "purple"), 
                       guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("MI efficacy (SMCFCS)", "MI efficacy (all ints)", 
                                  "MI actions (SMCFCS)", "MI actions (all ints)", 
                                  "MI  (no interactions)", "CCA")) +
    scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3, 4), limits = c(-3.5, 4.5)) +
    labs(x = "", y = "Mean difference in outcome (unadjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ2_mainEffects_unadj.pdf", width = 10, height = 10)
p_mi_cca_unadj
dev.off()


# Unadjusted results
res_adj <- res %>%
  filter(Model == "Adj")
head(res_adj)
summary(res_adj)

(p_mi_cca_adj <- ggplot(res_adj, aes(x = fct_rev(Contrast), y = Est, ymin = LCI, ymax = UCI,
                                         col = fct_rev(CCA_MI), fill = fct_rev(CCA_MI))) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_linerange(size = 0.5, position = position_dodge(width = 0.75), show.legend = FALSE) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    scale_fill_manual(values = c("red", "black", "blue", "green", "orange", "purple"), 
                      guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                      labels = c("MI efficacy (SMCFCS)", "MI efficacy (all ints)", 
                                 "MI actions (SMCFCS)", "MI actions (all ints)", 
                                 "MI (no interactions)", "CCA")) +
    scale_color_manual(values = c("red", "black", "blue", "green", "orange", "purple"), 
                       guide = guide_legend(reverse = TRUE, byrow = TRUE), name = "Model",
                       labels = c("MI efficacy (SMCFCS)", "MI efficacy (all ints)", 
                                  "MI actions (SMCFCS)", "MI actions (all ints)", 
                                  "MI (no interactions)", "CCA")) +
    scale_y_continuous(breaks = c(-3, -2, -1, 0, 1, 2, 3), limits = c(-3.5, 3.5)) +
    labs(x = "", y = "Mean difference in outcome (adjusted)") +
    coord_flip() +
    theme_bw() + 
    facet_wrap(Outcome ~ ., ncol = 1, labeller = as_labeller(c(Dep = "Depression (EPDS)", 
                                                               Anx = "Anxiety (GAD7)",
                                                               WB = "Well-being (WEMWBS)"))) +
    theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
          legend.text = element_text(size = 10), legend.title = element_text(size = 12),
          axis.title.x = element_text(size = 14), strip.background = element_blank(),
          strip.text = element_text(size = 14), legend.spacing.y = unit(0.25, 'cm')))

# Save as PDF
pdf("./Results/RQ2_mainEffects_adj.pdf", width = 10, height = 10)
p_mi_cca_adj
dev.off()


