#Figure 1 -- Applicant scholarly metrics

#A. 1st author papers----
fig1a_plot <- bio_qualif_data %>% 
  filter(question == "first_author") %>% 
  filter(!is.na(response)) %>% 
  distinct() %>% count(response) %>% 
  mutate(percent = get_percent(n, 164),
         response = as.numeric(response)) %>% 
  ggplot(aes(x=response, y=percent))+
  geom_col(position = "dodge")+
  scale_y_continuous(expand = c(0,0))+ #ensure bars directly abut axis, no padding
  labs(y="Respondents (%)")+
  labs(x="Number of first-author papers")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig1a_", Sys.Date(), ".jpeg"))

#B. Total publications----
fig1b_plot <- bio_qualif_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  distinct() %>% count(response) %>% 
  mutate(percent = get_percent(n, 164),
         response = as.numeric(response)) %>% 
  ggplot(aes(x=response, y=percent))+
  geom_col(position = "dodge")+
  scale_y_continuous(expand = c(0,0))+ #ensure bars directly abut axis, no padding
  labs(y="Respondents (%)", x="Number of papers")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig1b_", Sys.Date(), ".jpeg"))

#C. Total citations----
fig1c_plot <- bio_qualif_data %>% 
  filter(question == "scholar_citations_all") %>% 
  distinct() %>%
  mutate(papers_bin = get_big_bins(as.numeric(response))) %>% 
  count(papers_bin) %>% 
  mutate(percent = get_percent(n, 148)) %>% 
  ggplot(aes(x=factor(papers_bin, 
                      levels = bin_levels_big), y=percent))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(x="Number of Google Scholar citations", y="Respondents (%)")+
  my_theme

ggsave(filename = paste0("jadavji_biology/figures/fig1c_", Sys.Date(), ".jpeg"))

#D. H-index----
fig1d_plot <- bio_qualif_data %>% 
  filter(question == "scholar_hindex") %>% 
  distinct() %>% 
  count(response) %>% 
  mutate(percent = get_percent(n, 148)) %>% 
  ggplot(aes(x=as.numeric(response), y = percent))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(limits = c(0,20))+
  labs(y="Respondents (%)", x="Google Scholar H-index")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig1d_", Sys.Date(), ".jpeg"))

#E. Number CNS papers----
fig1e_plot <- bio_qualif_data %>% 
  filter(question == "CNS_first_author") %>% 
  distinct() %>%
  plot_perc_resp(., 40)+
  labs(y="Respondents (%)\nwith a CNS Paper ", 
       x="First author on a CNS paper")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig1e_", Sys.Date(), ".jpeg"))

#F. CNS papers y/n----
fig1f_plot <- bio_qualif_data %>% 
  filter(question == "CNS_status") %>% 
  distinct() %>% 
  plot_perc_resp(., 163)+
  labs(x="Published a CNS paper")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig1f_", Sys.Date(), ".jpeg"))

#G. Post-doc Fellowships y/n----
fig1g_plot <- bio_qualifications %>% 
  select(id, postdoc_fellow) %>% 
  filter(!is.na(postdoc_fellow)) %>% 
  distinct() %>% 
  count(postdoc_fellow) %>% 
  mutate(percent = get_percent(n, 243)) %>% 
  ggplot(aes(x=postdoc_fellow, y = percent))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  labs(y="Respondents (%)", 
       x="Received a postdoctoral fellowship")+
  my_theme_horiz

ggsave(filename = paste0("jadavji_biology/figures/fig1g_", Sys.Date(), ".jpeg"))

#generate plot 1----

fig1 <- plot_grid(fig1a_plot, fig1b_plot, fig1c_plot, fig1d_plot, 
                  fig1e_plot, fig1f_plot, fig1g_plot, 
                  labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'), ncol = 2)


ggsave(filename = paste0("Figure_1_", Sys.Date(), ".png"), 
       device = 'png', units = "in", scale = 1.75,
       path = 'jadavji_biology/figures/', width = 12, height = 11)