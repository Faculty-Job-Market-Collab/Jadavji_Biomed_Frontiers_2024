#Figure 2 PUI vs RI

#A. Submitted applicants----
fig2a_plot <- bio_app_outcomes %>% 
  filter(!is.na(R1_apps_submitted)) %>% 
  select(id, R1_apps_submitted, PUI_apps_submitted) %>% 
  gather(-id, key = question, value = response) %>% 
  mutate(response = get_small_bins(as.numeric(response)),
         question = if_else(str_detect(question, "R1"), "RI", "PUI")) %>% 
  distinct() %>% 
  filter(!is.na(response)) %>%
  count(question, response) %>% 
  mutate(percent = get_percent(n, 268)) %>%
  ggplot(aes(x=factor(response, bin_levels_small), y = percent))+
  geom_col()+
  facet_wrap(~question)+
  scale_y_continuous(expand = c(0,0), limits = c(0,60))+
  labs(y="Respondents (%)", x="Number of applications submitted\n")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig2a_", Sys.Date(), ".jpeg"))

#B. Offers----
fig2b_plot <- bio_tidy_data %>% 
  select(-question, -response, -section) %>% 
  filter(inst_type == "offer_institutions") %>% 
  filter(inst_key == "PUI_RI") %>% 
  filter(!is.na(inst_value)) %>% 
  distinct() %>% 
  count(id, inst_value) %>% 
  count(inst_value, n) %>%
  mutate(percent = get_percent(nn, 96)) %>% 
  ggplot(aes(x = n, y = percent))+
  geom_col()+
  facet_wrap(~inst_value)+
  scale_y_continuous(expand = c(0,0), limits = c(0,60))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6))+
  labs(y="Respondents (%)", x="Number of offers recieved\n")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig2b_", Sys.Date(), ".jpeg"))

#generate plot----
Fig2 <- plot_grid(fig2a_plot, fig2b_plot, 
                  labels = c('A', 'B'),
                  label_size = 18, nrow = 2)

ggsave(filename = paste0("Figure_2_", Sys.Date(),".png"), device = 'png', 
       units = "in", scale = 1.75,
       path = 'jadavji_biology/figures/', width = 6, height = 6)