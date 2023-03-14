#Figure 2 PUI vs RI

#A. Submitted applicants----
fig2a_plot <- fig2a_data %>%
  ggplot(aes(x=factor(response, bin_levels_small), y = percent))+
  geom_col()+
  facet_wrap(~question)+
  scale_y_continuous(expand = c(0,0), limits = c(0,60))+
  labs(y="Respondents (%)", x="Number of applications submitted\n")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig2a_", Sys.Date(), ".jpeg"))

#B. Offers----
fig2b_plot <- fig2b_data %>% 
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