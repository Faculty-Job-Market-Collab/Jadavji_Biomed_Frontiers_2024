#Figure 4. Traditional measures of success by gender & offers

#A. ----
fig4a_table <- table(fig3_data$apps_submitted, fig3_data$adjusted_gender)

fig4a_chi <- chisq.test(fig4a_table)

fig4a_data <- fig3_data %>% 
  filter(!is.na(apps_submitted_binned)) %>%
  count(adjusted_gender, apps_submitted_binned) %>% 
  spread(key = apps_submitted_binned, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:13)), na.rm = TRUE)) %>% 
  gather(2:13, key = "apps_submitted_binned", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_gender = get_percent(n, total))

fig4a_plot_leg <- fig4a_data %>% 
  ggplot(aes(x = factor(apps_submitted_binned, 
                        levels = bin_levels_small), 
             y = percent_gender,
             fill = factor(adjusted_gender, levels = gender_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = gender_breaks, values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of applications submitted", 
       y = "Percent of responses\nby gender",
       fill = "Gender")+
  my_theme_leg

fig4a_plot <- fig4a_plot_leg+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig4a_", Sys.Date(), ".jpeg"))

#B. ----
fig4b_plot <- fig5_data %>% 
  filter(!is.na(inst_type)) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  select(id, adjusted_gender, inst_type_bin, values_binned) %>% 
  distinct() %>%
  count(adjusted_gender, inst_type_bin, values_binned) %>% 
  spread(key = values_binned, value = n) %>% 
  mutate(across(3:15, ~ replace_na(.x, replace = 0))) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(3:15)), na.rm = TRUE),
         across(3:15, ~ get_percent(.x, total))) %>% 
  gather(3:15, key = values_binned, value = percent) %>% 
  select(-total) %>% distinct() %>% 
  ggplot(aes(x = factor(values_binned, 
                        levels = bin_levels_small), 
             y = percent, 
             fill = factor(adjusted_gender, levels = gender_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = gender_breaks, values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  facet_wrap(~inst_type_bin)+
  labs(x = "Number of applications submitted", 
       y = "Percent of gender\n(grouped by institution type)",
       fill = "Gender")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig4b_", Sys.Date(), ".jpeg"))

#C. CNS paper y/n----
fig4c_gen_table <- table(fig4_data$CNS_status, fig4_data$adjusted_gender)

fig4c_chi <- chisq.test(fig4c_gen_table)

fig4c_plot <- fig4_data %>% 
  count(adjusted_gender, CNS_status) %>% 
  spread(key = CNS_status, value = n) %>% 
  mutate(total = No + Yes,
         percent = get_percent(Yes, total)) %>% 
  ggplot(aes(x = factor(adjusted_gender, levels = gender_breaks), y = percent,
             fill = factor(adjusted_gender, levels = gender_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = gender_breaks, values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Published in CNS (%)", x = '')+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig4c_", Sys.Date(), ".jpeg"))

#D. Number of applications submitted ----
fig4d_table <- table(fig3_data$faculty_offers, fig3_data$adjusted_gender)

fig4d_chi <- chisq.test(fig4d_table)

fig4d_data <- fig3_data %>% 
  filter(!is.na(faculty_offers)) %>%
  count(adjusted_gender, faculty_offers) %>% 
  spread(key = faculty_offers, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:4)), na.rm = TRUE)) %>% 
  gather(2:4, key = "faculty_offers", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_gender = get_percent(n, total),
         faculty_offers = factor(faculty_offers,
                                 levels = c("0", "1", ">1")))

fig4d_plot <- fig4d_data %>% 
  ggplot(aes(x = faculty_offers, 
             y = percent_gender,
             fill = factor(adjusted_gender, levels = gender_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = gender_breaks, values = gender_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of faculty offers", 
       y = "Percent of responses\nby gender")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig4d_", Sys.Date(), ".jpeg"))

#E. ----
fig4e_table <- table(fig3_data$on_site_interviews, fig3_data$adjusted_gender)

fig4e_chi <- chisq.test(fig4e_table)

fig4e_data <- fig3_data %>% 
  filter(!is.na(on_site_interviews)) %>%
  count(adjusted_gender, on_site_interviews) %>% 
  spread(key = on_site_interviews, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:20)), na.rm = TRUE)) %>% 
  gather(2:20, key = "on_site_interviews", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_gender = get_percent(n, total))

fig4e_plot <- fig4e_data %>% 
  ggplot(aes(x = as.numeric(on_site_interviews), 
             y = percent_gender,
             fill = factor(adjusted_gender, levels = gender_breaks)))+
  geom_col()+
  facet_wrap(~factor(adjusted_gender, levels = gender_breaks))+
  scale_fill_manual(breaks = gender_breaks, values = gender_color)+
  scale_y_continuous(expand = c(0,0), limits = c(0,50))+
  labs(x = "Number of on-site interviews", 
       y = "Percent of responses\nby gender")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig4e_", Sys.Date(), ".jpeg"))

#F. 1st author CNS y/n----
fig4f_table <- table(fig4_data$CNS_first_author, fig4_data$adjusted_gender)

fig4f_chi <- chisq.test(fig4f_table)

fig4f_plot <- fig4_data %>% 
  filter(!is.na(faculty_offers)) %>%
  #filter(adjusted_gender %in% c("Woman", "Man")) %>% 
  #mutate(CNS_first_author = replace_na(CNS_first_author, "0")) %>%
  count(adjusted_gender, CNS_first_author) %>% 
  spread(key = CNS_first_author, value = n) %>% 
  mutate(total = `0`+`1`+`2`,
         `0` = get_percent(`0`, total),
         `1` = get_percent(`1`, total),
         `2` = get_percent(`2`, total)) %>% 
  gather(2:4, key = CNS_first_author, value = percent) %>% 
  ggplot(aes(x = CNS_first_author, y = percent,
             fill = factor(adjusted_gender, levels = gender_breaks)))+
  geom_col(position = "dodge")+
  facet_wrap(~factor(adjusted_gender, levels = gender_breaks))+
  scale_fill_manual(breaks = gender_breaks, values = gender_color)+
  scale_y_continuous(expand = c(0,0), limits = c(0,90))+
  labs(x = "Number of first-author CNS papers", 
       y = "Percent of gender")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig4f_", Sys.Date(), ".jpeg"))

#compile chi stats (a,d,e)----
chi_list <- c("fig4a_chi", 
              "fig4c_chi", "fig4d_chi", 
              "fig4e_chi", "fig4f_chi")

plot_list <- c('4A', '4C', '4D', '4E', '4F')

fig4_chi_tbl_raw <- map2_df(chi_list, plot_list, get_wilcox_tbl) 

fig4_chi_tbl <- fig4_chi_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  select(figure, method, `statistic.X-squared`, p.value, parameter.df)

write_csv(fig4_chi_tbl, file = paste0("jadavji_biology/figures/fig4adef_chi_stats", 
                                      Sys.Date(),".csv"))

#generate plot----
fig4_leg <- get_legend_plot(fig4a_plot_leg)

ggsave("jadavji_biology/figures/fig4_legend.jpeg")

Fig4ab <- plot_grid(fig4a_plot, fig4b_plot, 
                    labels = c('A', 'B'),
                    rel_widths = c(.8, 1),
                    label_size = 18, nrow = 1)

Fig4cd <- plot_grid(fig4c_plot, fig4d_plot, fig4_leg,
                    labels = c('C', 'D', ''),
                    rel_widths = c(1, 1, .5),
                    label_size = 18, nrow = 1)


Fig4ef <- plot_grid(fig4e_plot, fig4f_plot,
                    labels = c('E', 'F'),
                    label_size = 18, nrow = 1)

Fig4 <- plot_grid(Fig4ab, Fig4cd, Fig4ef, nrow = 3)


ggsave(filename = paste0("Figure_4_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'jadavji_biology/figures/', width = 8, height = 8)
