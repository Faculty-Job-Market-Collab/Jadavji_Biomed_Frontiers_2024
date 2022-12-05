#Figure 1 -- Applicant scholarly metrics

#Applicant fields
#1a----
#fig1a_plot <- demo_data %>% 
#  filter(question == "research_category") %>% 
#  distinct() %>% 
#  count(response) %>% 
#  mutate(percent = get_percent(n, 765),
#         response = case_when(
#           response == "Social, Behavior, & Economic Sciences" ~ "Social, Behavior, &\nEconomic Sciences",
#           response == "Mathematical & Physical Sciences" ~ "Mathematical & Physical\nSciences",
#           response == "Computer & Information Sciences" ~ "Computer & Information\nSciences",
#           TRUE ~ response
#         )) %>% 
#  ggplot(aes(x=fct_reorder(response, desc(percent)),
#             y = percent))+
#  geom_col(position = "dodge") +
#  scale_y_continuous(expand = c(0,0)) +
#  labs(y="Responses (%)", x="Research field")+
#  my_theme

ggsave("nafisa/figures/fig1a_field.jpeg")

#B. Bio applicant location

#1b----
#fig1b_plot <- bio_demo_data %>% 
#  filter(question == "residence") %>% 
#  distinct() %>%
#  ggplot(aes(x=response))+
#  geom_bar()+
#  coord_flip()+
#  labs(y="Number of responses (n=332)", x="Country of residence")+
#  my_theme_horiz
#
#ggsave("nafisa/figures/fig1b_location.jpeg")

#C. Bio applicant position
#1c----
#fig1c_plot <- bio_demo_data %>% 
#  filter(question == "position") %>% 
#  distinct() %>%
#  ggplot(aes(x=response))+
#  geom_bar()+
#  coord_flip()+
#  labs(y="Number of responses (n=295)", x="Position")+
#  my_theme_horiz
#
#ggsave("nafisa/figures/fig1c_position.jpeg")

#D. Number of postdoc positions
#1d----
#fig1d_plot <- bio_network_data %>% 
#  filter(question == "number_postdocs") %>% 
#  distinct() %>% 
#  mutate(response = factor(response, levels = c("1", "2", "3", ">3"))) %>% 
#  ggplot(aes(x=response))+
#  geom_bar()+
#  coord_flip()+
#  labs(y="Number of responses (n=119)", x="Number of postdocs")+
#  my_theme_horiz
#
#ggsave("nafisa/figures/fig1d_num_postdocs.jpeg")

#E. Applicant metric medians
#1e----
#metric_data <- bio_qualifications %>% select(id, 1:5, 7, 9:14, 16) %>% 
#  gather(-id, key = question, value = response) %>% 
#  distinct()
#
#metric_medians <- metric_data %>% 
#  group_by(question) %>% 
#  summarise(med = median(as.numeric(response), na.rm = TRUE),
#            max = max(as.numeric(response), na.rm = TRUE))
#
#fig1e_plot <- metric_data %>% 
#  ggplot(aes(x = question, y=as.numeric(response), fill = question))+
#  #geom_dotplot()+
#  geom_boxplot()+
#  facet_wrap(~question, scales = "free")+
#  geom_text(data = metric_medians, aes(x = question, 
#                                       y = max*0.7, 
#                                       label = paste("median =", med)), 
#            size = 5)+
#  labs(x = "Applicant metric", y = "Response value")+
#  my_theme+
#  theme(axis.text.x = element_blank(), 
#        axis.ticks.x = element_blank())
#
#ggsave("nafisa/figures/fig1e_metric_medians.jpeg")
#
#F. 1st author papers
#1f----
fig1f_plot <- bio_qualif_data %>% 
  filter(question == "first_author") %>% 
  filter(!is.na(response)) %>% 
  distinct() %>%
  plot_perc_resp(., 164)+
  labs(x="Number of first-author papers")+
  my_theme_horiz

ggsave("nafisa/figures/fig1f_num_first_auth.jpeg")

#G. Total publications
#1g----
fig1g_plot <- bio_qualif_data %>% 
  filter(question == "peer-reviewed_papers") %>% 
  distinct() %>%
  plot_perc_resp(., 164)+
  labs(x="Number of papers")+
  my_theme_horiz

ggsave("nafisa/figures/fig1g_num_peer-review.jpeg")

#H. Total citations
#1h----
fig1h_plot <- bio_qualif_data %>% 
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

ggsave("nafisa/figures/fig1h_num_citations.jpeg")

#I. H-index
#1i----
fig1i_plot <- bio_qualif_data %>% 
  filter(question == "scholar_hindex") %>% 
  distinct() %>% 
  count(response) %>% 
  mutate(percent = get_percent(n, 148)) %>% 
  ggplot(aes(x=as.numeric(response), y = percent))+
  geom_col()+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(limits = c(0,30))+
  labs(y="Respondents (%)", x="Google Scholar H-index")+
  my_theme_horiz

ggsave("nafisa/figures/fig1i_hindex.jpeg")

#J. CNS papers y/n
#1j----
fig1j_plot <- bio_qualif_data %>% 
  filter(question == "CNS_status") %>% 
  distinct() %>% 
  plot_perc_resp(., 163)+
  labs(x="Published a CNS paper")+
  my_theme_horiz

ggsave("nafisa/figures/fig1j_cns.jpeg")

fig1j_plot <- bio_qualif_data %>% 
  filter(question == "CNS_first_author") %>% 
  distinct() %>%
  plot_perc_resp(., 40)+
  labs(y="Respondents (%)\nwith a CNS Paper ", 
       x="First author on a CNS paper")+
  my_theme_horiz

ggsave("nafisa/figures/fig1_num_cns.jpeg")

#K. Fellowships y/n
#1k----
fig1k_plot <- bio_qualifications %>% 
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

ggsave("nafisa/figures/fig1k_fellowship.jpeg")

#L. Career transition award y/n
#1l----
#fig1l_plot <- bio_qualif_data %>% 
#  filter(question == "transition_award") %>% 
#  distinct() %>% 
#  ggplot(aes(x=response))+
#  geom_bar()+
#  coord_flip()+
#  labs(y="Number of responses (n=189)", 
#       x="Recieved a career\ntransition award")+
#  my_theme_horiz
#
#ggsave("nafisa/figures/fig1l_career_transition.jpeg")

#M. Posted preprints y/n
#1m----
#fig1m_plot <- bio_qualif_data %>% 
#  filter(question == "preprint_status") %>% 
#  distinct() %>% 
#  ggplot(aes(x=response))+
#  geom_bar()+
#  coord_flip()+
#  labs(y="Number of responses (n=164)", x="Posted a preprint")+
#  my_theme_horiz
#
#ggsave("nafisa/figures/fig1m_preprint.jpeg")

#N. PUI vs RI applicants
#1n----
fig1n_plot <- bio_app_outcomes %>% 
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
  scale_y_continuous(expand = c(0,0))+
  labs(y="Respondents (%)", x="Number of applications submitted\n")+
  my_theme

ggsave("nafisa/figures/fig1n_pui_ri.jpeg")

bio_tidy_data %>% 
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

ggsave("nafisa/figures/fig1o_pui_ri.jpeg")
#generate plot 1----

#fig1 <- plot_grid(fig1a_plot, fig1b_plot, fig1c_plot, fig1d_plot, 
#                  fig1f_plot, fig1g_plot, fig1h_plot, fig1i_plot,
#                  fig1j_plot, fig1k_plot, fig1l_plot, fig1m_plot,
#                  fig1n_plot,
#                  labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
#                             'I', 'J', 'K', 'L', 'M'), ncol = 3)
#
#
#ggsave("Figure_1.png", device = 'png', units = "in", scale = 1.75,
#       path = 'nafisa/figures/', width = 12, height = 11)