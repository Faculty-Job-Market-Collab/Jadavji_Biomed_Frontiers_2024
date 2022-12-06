#Figure 3. Job application benchmarks by gender and offers (0, 1, >1 offers)

#A. Remote interviews----
fig3a_plot <- fig3_data %>% 
  filter(!is.na(apps_submitted)) %>%
  get_plot_summary(., "faculty_offers", 
                   "apps_submitted_binned", FALSE) %>% 
  ggplot(aes(x = factor(apps_submitted_binned, 
                        levels = bin_levels_small), y = percent))+
  geom_col(position = "dodge")+
  facet_wrap(~faculty_offers)+
  scale_y_continuous(limits = c(0,30), expand = c(0,0))+
  labs(x = "Number of applications submitted", 
       y = "% Respondents\n(by number of faculty offers)")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig3a_", Sys.Date(), ".jpeg"))

#B. Onsite interviews----
fig3b_plot <- fig3_data %>% 
  filter(!is.na(off_site_interviews)) %>%
  get_plot_summary(., "faculty_offers", 
                   "off_site_interviews", FALSE) %>%
  ggplot(aes(x = as.numeric(off_site_interviews), y=percent))+
  geom_col(position = "dodge")+
  facet_wrap(~faculty_offers)+
  scale_x_continuous(limits = c(0,20))+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of off-site interviews", 
       y = "% Respondents\n(by number of faculty offers)")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig3b_", Sys.Date(), ".jpeg"))

#C. Number of offers----
fig3c_plot <- fig3_data %>%  
  filter(!is.na(on_site_interviews)) %>%
  get_plot_summary(., "faculty_offers", 
                   "on_site_interviews", FALSE) %>%
  ggplot(aes(x = as.numeric(on_site_interviews), y = percent))+
  geom_col(position = "dodge")+
  facet_wrap(~faculty_offers)+
  scale_y_continuous(expand = c(0,0), limits = c(0,50))+
  labs(x = "Number of on-site interviews", 
       y = "% Responses\n(by number of faculty offers")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig3c_", Sys.Date(), ".jpeg"))

#generate plot----
fig3 <- plot_grid(fig3a_plot, fig3b_plot, fig3c_plot, 
                    labels = c('A', 'B', 'C'),
                    #rel_widths = c(.6, .6, 1),
                    label_size = 18, ncol = 1)

ggsave(filename = paste0("Figure_3_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'jadavji_biology/figures/', width = 8, height = 11)
