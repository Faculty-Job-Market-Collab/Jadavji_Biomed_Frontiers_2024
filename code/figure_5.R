#Figure 5. Traditional measures of success by PEER status status & offers

#A. ----
fig5a_table <- table(fig3_data$apps_submitted, fig3_data$peer)

fig5a_chi <- chisq.test(fig5a_table, simulate.p.value = TRUE, B = 10000)

fig5a_data <- fig3_data %>% 
  filter(!is.na(apps_submitted_binned)) %>%
  filter(apps_submitted_binned != "NR") %>%
  filter(peer != "NR") %>% 
  count(peer, apps_submitted_binned) %>% 
  spread(key = apps_submitted_binned, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:13)), na.rm = TRUE)) %>% 
  gather(2:13, key = "apps_submitted_binned", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_peer = get_percent(n, total))

fig5a_plot_leg <- fig5a_data %>% 
  ggplot(aes(x = factor(apps_submitted_binned, 
                        levels = bin_levels_small), 
             y = percent_peer,
             fill = factor(peer, levels = peer_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of applications submitted", 
       y = "Percent of responses\nby PEER status",
       fill = "PEER Status")+
  my_theme_leg

fig5a_plot <- fig5a_plot_leg+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig5a_", Sys.Date(), ".jpeg"))

#B. ----
fig5b_data <- fig5_data %>% 
  filter(!is.na(inst_type)) %>% 
  filter(!is.na(peer)) %>% 
  filter(inst_type != "NR") %>% 
  filter(peer != "NR") %>% 
  select(id, peer, inst_type_bin, values_binned, values) %>% 
  distinct() %>%
  mutate(values = as.numeric(values),
         dummy_var = paste0(peer, ":", inst_type_bin))
#inst only
inst_5b <- fig5b_data %>% 
  select(id, inst_type_bin, values_binned) %>% distinct()

fig5b_inst_table <- table(fig5b_data$dummy_var, fig5b_data$values_binned)

fig5b_inst_chi <- chisq.test(fig5b_inst_table, 
                             simulate.p.value = TRUE, B = 10000)  
#peer only
fig5b_peer_table <- table(fig5b_data$peer, fig5b_data$values_binned)

fig5b_peer_chi <- chisq.test(fig5b_peer_table, 
                             simulate.p.value = TRUE, B = 10000)  
#inst+peer
fig5b_table <- table(fig5b_data$dummy_var, fig5b_data$values_binned)

fig5b_chi <- chisq.test(fig5b_table, simulate.p.value = TRUE, B = 10000)

fig5b_data <- fig5b_data %>% 
  count(peer, inst_type_bin, values_binned) %>% 
  spread(key = values_binned, value = n) %>% 
  mutate(across(3:15, ~ replace_na(.x, replace = 0))) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(3:15)), na.rm = TRUE),
         across(3:15, ~ get_percent(.x, total))) %>% 
  gather(3:15, key = values_binned, value = percent) %>% 
  select(-total) %>% distinct() 

fig5b_plot <- fig5b_data %>% 
  ggplot(aes(x = factor(values_binned, 
                        levels = bin_levels_small), 
             y = percent, 
             fill = factor(peer, levels = peer_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  facet_wrap(~inst_type_bin)+
  labs(x = "Number of applications submitted", 
       y = "Percent of peer\n(grouped by institution type)",
       fill = "peer")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig5b_", Sys.Date(), ".jpeg"),
       width = 10, height = 5.5)

#C. CNS paper y/n----
fig5c_peer_table <- table(fig4_data$CNS_status, fig4_data$peer)

fig5c_chi <- chisq.test(fig5c_peer_table, 
                        simulate.p.value = TRUE, B = 10000)

fig5c_plot <- fig4_data %>% 
  filter(peer != "NR") %>% 
  count(peer, CNS_status) %>% 
  spread(key = CNS_status, value = n) %>% 
  mutate(total = No + Yes,
         percent = get_percent(Yes, total)) %>% 
  ggplot(aes(x = factor(peer, levels = peer_breaks), y = percent,
             fill = factor(peer, levels = peer_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(y = "Published in CNS (%)", x = '')+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig5c_", Sys.Date(), ".jpeg"))

#D. Number of applications submitted ----
fig5d_table <- table(fig3_data$faculty_offers, fig3_data$peer)

fig5d_chi <- chisq.test(fig5d_table, simulate.p.value = TRUE, B = 10000)

fig5d_data <- fig3_data %>% 
  filter(!is.na(faculty_offers)) %>% 
  filter(faculty_offers != "NR") %>% 
  filter(peer != "NR") %>% 
  count(peer, faculty_offers) %>% 
  spread(key = faculty_offers, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:4)), na.rm = TRUE)) %>% 
  gather(2:4, key = "faculty_offers", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_peer = get_percent(n, total),
         faculty_offers = factor(faculty_offers,
                                 levels = c("0", "1", ">1")))

fig5d_plot <- fig5d_data %>% 
  ggplot(aes(x = faculty_offers, 
             y = percent_peer,
             fill = factor(peer, levels = peer_breaks)))+
  geom_col(position = "dodge")+
  scale_fill_manual(breaks = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0), limits = c(0,55))+
  labs(x = "Number of faculty offers", 
       y = "Percent of responses\nby PEER status")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig5d_", Sys.Date(), ".jpeg"))

#E. ----
fig5e_table <- table(fig3_data$on_site_interviews, fig3_data$peer)

fig5e_chi <- chisq.test(fig5e_table, simulate.p.value = TRUE, B = 10000)

fig5e_data <- fig3_data %>% 
  filter(!is.na(on_site_interviews)) %>%
  filter(on_site_interviews != "NR") %>%
  filter(peer != "NR") %>% 
  count(peer, on_site_interviews) %>% 
  spread(key = on_site_interviews, value = n) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(as.numeric(2:16)), na.rm = TRUE)) %>% 
  gather(2:16, key = "on_site_interviews", value = "n") %>% 
  mutate(n = replace_na(n, 0),
         percent_peer = get_percent(n, total))

fig5e_plot <- fig5e_data %>% 
  ggplot(aes(x = as.numeric(on_site_interviews), 
             y = percent_peer,
             fill = factor(peer, levels = peer_breaks)))+
  geom_col()+
  facet_wrap(~factor(peer, levels = peer_breaks))+
  scale_fill_manual(breaks = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of on-site interviews", 
       y = "Percent of responses\nby PEER status")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig5e_", Sys.Date(), ".jpeg"))

#F. 1st author CNS y/n----
fig5f_table <- table(fig4_data$CNS_first_author, fig4_data$peer)

fig5f_chi <- chisq.test(fig5f_table, simulate.p.value = TRUE, B = 10000)

fig5f_plot <- fig4_data %>% 
  filter(!is.na(faculty_offers)) %>%
  filter(faculty_offers != "NR") %>%
  filter(peer != "NR") %>% 
  count(peer, CNS_first_author) %>% 
  spread(key = CNS_first_author, value = n) %>% 
  mutate(`2` = if_else(is.na(`2`), 0, as.numeric(`2`)),
         total = `0`+`1`+`2`,
         `0` = get_percent(`0`, total),
         `1` = get_percent(`1`, total),
         `2` = get_percent(`2`, total)) %>% 
  gather(2:4, key = CNS_first_author, value = percent) %>% 
  ggplot(aes(x = CNS_first_author, y = percent,
             fill = factor(peer, levels = peer_breaks)))+
  geom_col(position = "dodge")+
  facet_wrap(~factor(peer, levels = peer_breaks))+
  scale_fill_manual(breaks = peer_breaks, values = peer_color)+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Number of first-author CNS papers", 
       y = "Percent by PEER Status")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig5f_", Sys.Date(), ".jpeg"))

#compile chi stats (a,d,e)----
chi_list <- c("fig5a_chi", "fig5b_chi", 
              "fig5b_inst_chi", "fig5b_peer_chi",
              "fig5c_chi", "fig5d_chi", 
              "fig5e_chi", "fig5f_chi")

plot_list <- c('5A', '5B PEER:Inst', '5B PUIvRI', '5B PEER',
               '5C', '5D', '5E', '5F')

fig5_chi_tbl_raw <- map2_df(chi_list, plot_list, get_wilcox_tbl) 

fig5_chi_tbl <- fig5_chi_tbl_raw %>% 
  spread(key = attribute, value = value) %>% 
  select(figure, method, `statistic.X-squared`, p.value, parameter.df)

write_csv(fig5_chi_tbl, file = paste0("jadavji_biology/figures/fig5adef_chi_stats", 
                                      Sys.Date(),".csv"))

#generate plot----
fig5_leg <- get_legend_plot(fig5a_plot_leg)

ggsave("jadavji_biology/figures/fig5_legend.jpeg")

Fig5ab <- plot_grid(fig5a_plot, fig5b_plot, 
                    labels = c('A', 'B'),
                    rel_widths = c(.8, 1),
                    label_size = 18, nrow = 1)

Fig5cd <- plot_grid(fig5c_plot, fig5d_plot, fig5_leg,
                    labels = c('C', 'D', ''),
                    rel_widths = c(1, 1, .5),
                    label_size = 18, nrow = 1)


Fig5ef <- plot_grid(fig5e_plot, fig5f_plot,
                    labels = c('E', 'F'),
                    label_size = 18, nrow = 1)

Fig5 <- plot_grid(Fig5ab, Fig5cd, Fig5ef, nrow = 3)


ggsave(filename = paste0("Figure_5_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'jadavji_biology/figures/', width = 8, height = 8)
