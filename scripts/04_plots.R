source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "02_load_data.R"))

# Add color blind palette - The palette with grey:
cbPalette <- c("#009E73", "#ff6242", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Multinomial plots 

#### Distributions of probabilities 

level_order <- c('natural kind', 'artifact', 'abstract concept') 


three_choice_data %>%
  data_grid(kind) %>%
  add_fitted_draws(multinom_mod, dpar = TRUE, category = "response_corrected",
                   re_formula = NA) %>%
  filter(kind == "natural kind" | kind == "abstract concept" |
           kind == "artifact") %>% 
  ggplot(aes(y = factor(kind, level = level_order), x = .value, fill = response_corrected)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  theme_minimal() +
  xlab("Probability") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Response")) +
  ggsave(here("figs", "three_choice_posterior.png"))


three_choice_data %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>%
  ggplot(aes(response_corrected, fill = response_corrected)) +
  geom_histogram(stat = "count", color = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~kind) +
  guides(fill=guide_legend("Response")) +
  ggsave(here("figs", "multinom_plot_hist.png"))


data_grid_m = three_choice_data %>%
  data_grid(kind) %>%
  add_fitted_draws(multinom_mod, dpar = TRUE, category = "response_corrected",
                   re_formula = NA) 


plot_df_m = data_grid_m %>% 
  filter(kind == "natural kind" | kind == "abstract concept" |
           kind == "artifact") %>% 
  pivot_wider(names_from = response_corrected, values_from = .value) %>% 
  mutate(both_is = both - is) %>% 
  mutate(both_isnot = both - is_not) %>% 
  mutate(is_isnot = is - is_not)

plot_df_m_long = plot_df_m %>% 
  select(kind, is_isnot, both_is, both_isnot) %>% 
  pivot_longer(c(is_isnot, both_is, both_isnot), names_to = "effect", 
               values_to = "difference_in_probability")

# create df to pull HDIs for the plot 
both_is_hdi = plot_df_m %>% 
  group_by(kind) %>% 
  summarize(hdi_low = round(hdi(both_is)[,1], digits = 3),
            hdi_hi = round(hdi(both_is)[,2], digits = 3),
            difference_in_probability = mean(both_is)) %>% 
  mutate(effect = "both_is")

both_isnot_hdi = plot_df_m %>% 
  group_by(kind) %>% 
  summarize(hdi_low = round(hdi(both_isnot)[,1], digits = 3),
            hdi_hi = round(hdi(both_isnot)[,2], digits = 3),
            difference_in_probability = mean(both_isnot)) %>% 
  mutate(effect = "both_isnot")


is_isnot_hdi = plot_df_m %>% 
  group_by(kind) %>% 
  summarize(hdi_low = round(hdi(is_isnot)[,1], digits = 3),
            hdi_hi = round(hdi(is_isnot)[,2], digits = 3),
            difference_in_probability = mean(is_isnot)) %>% 
  mutate(effect = "is_isnot")

pct_pos_df = plot_df_m_long %>% 
  mutate(is_positve = case_when(
    difference_in_probability > 0 ~ 1,
    difference_in_probability < 0 ~ 0
  )) %>% 
  group_by(kind, effect) %>% 
  summarise(qty_positive = sum(is_positve)/4000)


all_eff_df = rbind(is_isnot_hdi, both_isnot_hdi, both_is_hdi) %>% 
  left_join(pct_pos_df, by = c("kind", "effect"))

plot_df_m_long %>% 
  ggplot(aes(x = difference_in_probability, y = effect, fill = after_stat(x < 0))) + 
  stat_halfeye() +
  theme_minimal() +
  geom_text(data = mutate_if(all_eff_df, is.numeric, round, 2),
            aes(label = paste0(difference_in_probability, " [", `hdi_low`, " - ", `hdi_hi`, "]")), 
            hjust = .5, vjust = 2, size = 2.5, family = "sans") +
  geom_text(data = mutate_if(all_eff_df, is.numeric, round, 2),
            aes(label = paste0(qty_positive)), 
            hjust = .5, vjust = -2.5, size = 2,
            family = "sans") +
  geom_vline(xintercept = 0, linetype = "dashed", 
             alpha = .4) +
  coord_cartesian(x = c(-1,1), clip = "off") +
  theme(panel.spacing = unit(1, "lines")) +
  scale_fill_manual(values=cbPalette) + theme(legend.position = "none") +
  facet_grid(~kind) +
  theme( axis.line = element_line(colour = "black", 
                                    size = .1, linetype = "solid")) +
  scale_y_discrete(breaks=c("is_isnot", "both_isnot", "both_is"),
                   labels=c("is not - is", "both - is not", 
                            "is - both")) +
  xlab("Difference in Probability") + 
  ylab("Effect") +
  ggsave(here("figs", "eff_size.png"))

#### Effect sizes 

rating_data %>% 
  ggplot(aes(x = rating, y = rating, color = prompt_type,
             alpha = .1)) + geom_jitter() + stat_ellipse() +
  facet_grid(~kind)

## Ordinal plots 
cond_df_ord = conditional_effects(ord_mod, categorical = TRUE)[["kind:cats__"]]

cond_df_ord %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>% 
  ggplot(aes(x = effect1__, y = estimate__, fill = effect2__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), shape = 21, 
                  position = position_dodge(width = .4)) +
  theme_minimal() + ylab("Probability") + xlab("Condition") + 
  labs(fill = "Rating") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave(here("figs", "ord_model.png"))

cond_df_ord %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>% 
  ggplot(aes(y = effect1__, x = estimate__, fill = effect2__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), shape = 21, 
                  position = position_dodge(width = .4)) +
  theme_minimal() + xlab("Probability") + ylab("Condition") + 
  labs(fill = "Rating") + 
  theme(axis.text.x = element_text(angle = 90))  +
  ggsave(here("plots", "ord_model_2.png"))


rating_data %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>% 
  ggplot(aes(rating, fill = as.factor(rating))) +
  geom_histogram(stat = "count", color = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~kind) +
  ggsave(here("plots", "ord_hist.png"))



