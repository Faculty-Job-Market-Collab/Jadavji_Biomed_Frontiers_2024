#load data, functions, and packages----
source("code/load_data.R")

library(ggsignif)

# Identify biological science responses----
bio_clean <- clean_data %>% 
  filter(response == "Biological Sciences") %>% 
  distinct() 

bio_ids <- bio_clean %>% select(id) %>% distinct()

# Identify outliers----
metric_quals <- c("peer-reviewed_papers", "conference_abstracts", 
                  "corresponding_author", "first_author",
                  "scholar_citations_all", "scholar_hindex",
                  "apps_submitted")

metric_outliers <- map_dfr(.x = metric_quals, function(x) get_mad_outliers("bio_clean", x))

bio_tidy_data <- left_join(bio_ids, tidy_data, by = 'id') %>% 
  mutate(outlier = case_when(
    question == "peer-reviewed_papers" & as.numeric(response) > 33 ~ "yes",
    question == "conference_abstracts" & as.numeric(response) > 45 ~ "yes",
    question == "corresponding_author" & as.numeric(response) > 10 ~ "yes",
    question == "first_author" & as.numeric(response) > 15 ~ "yes",
    question == "scholar_citations_all" & as.numeric(response) > 2199 ~ "yes",
    question == "scholar_hindex" & as.numeric(response) > 19 ~ "yes",
    question == "apps_submitted" & as.numeric(response) > 91 ~ "yes",
    TRUE ~ "no"
  )) %>% 
  filter(outlier == "no") %>% 
  select(-outlier)

write_csv(bio_tidy_data, 
          "jadavji_biology/data/bio_tidy_data.csv") #tidy data file for lily w/out outliers

bio_clean_data <- bio_clean %>% 
  mutate(paper_outliers = if_else(as.numeric(`peer-reviewed_papers`) > 33, "yes", "no"),
         abstract_outliers = if_else(as.numeric(conference_abstracts) > 45, "yes", "no"),
         corres_outliers = if_else(as.numeric(corresponding_author) > 10, "yes", "no"),
         first_outliers = if_else(as.numeric(first_author) > 15, "yes", "no"),
         cites_outliers = if_else(as.numeric(scholar_citations_all) > 2199, "yes", "no"),
         hindex_outliers = if_else(as.numeric(scholar_hindex) > 19, "yes", "no"),
         apps_sub_outliers = if_else(as.numeric(apps_submitted) > 91, "yes", "no"))

write_csv(bio_clean_data, 
          "jadavji_biology/data/bio_clean_data.csv") #clean data file for lily


bio_demo_data <- left_join(bio_ids, demo_data, by = 'id') %>% distinct()

bio_network_data <- left_join(bio_ids, network_data, by = 'id') %>% distinct()

bio_qualif_data <- bio_tidy_data %>% 
  filter(section == "qualifications") %>% 
  select(1:3) %>% 
  distinct()

bio_app_outcomes <- left_join(bio_ids, app_outcomes, by = 'id') %>% distinct()
#Figure 1.----
fig1_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("CNS_status", "CNS_first_author", "scholar_hindex",
                         "scholar_citations_all", "peer-reviewed_papers",
                         "first_author")) %>% 
  filter(!is.na(response)) %>% 
  filter(response != "NR") %>% 
  distinct()

source("jadavji_biology/code/figure_1.R")

#Figure 2. ----
fig2a_data <- bio_clean_data %>% 
  filter(apps_sub_outliers == no) %>% 
  select(id, R1_apps_submitted, PUI_apps_submitted) %>% 
  filter(!is.na(R1_apps_submitted)) %>% 
  gather(-id, key = question, value = response) %>% 
  mutate(response = get_small_bins(as.numeric(response)),
         question = if_else(str_detect(question, "R1"), "RI", "PUI")) %>% 
  distinct() %>% 
  filter(!is.na(response)) %>%
  count(question, response) %>% 
  mutate(percent = get_percent(n, 268))

fig2b_data <- bio_tidy_data %>% 
  select(-question, -response, -section) %>% 
  filter(inst_type == "offer_institutions") %>% 
  filter(inst_key == "PUI_RI") %>% 
  filter(!is.na(inst_value)) %>% 
  distinct() %>% 
  count(id, inst_value) %>% 
  count(inst_value, n) %>%
  mutate(percent = get_percent(nn, 96))

source("jadavji_biology/code/figure_2.R")

#Figure 3.----
fig3_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("adjusted_gender", "off_site_interviews",
                         "on_site_interviews", "peer", "faculty_offers",
                         "apps_submitted_binned", "apps_submitted")) %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  filter(!is.na(faculty_offers)) %>% 
  filter(adjusted_gender != "NR") %>% 
  filter(faculty_offers != "NR") %>% 
  mutate(faculty_offers = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) == 1 ~ "1",
    as.numeric(faculty_offers) >= 1 ~ ">1"
  ),
  faculty_offers = factor(faculty_offers, 
                          levels = c("0", "1", ">1")))

source("jadavji_biology/code/figure_3.R")

# Figure 4. ----
fig4_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("adjusted_gender", "CNS_status",
                         "CNS_first_author", "peer", "faculty_offers")) %>% 
  distinct() %>% 
  spread(key = question, value = response)  %>% distinct() %>% 
  filter(!is.na(adjusted_gender)) %>% 
  filter(!is.na(CNS_status)) %>% 
  filter(adjusted_gender != "NR") %>% 
  filter(CNS_status != "NR") %>% 
  mutate(faculty_offers = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) == 1 ~ "1",
    as.numeric(faculty_offers) >= 1 ~ ">1"
  ),
  faculty_offers = factor(faculty_offers, levels = c("0", "1", ">1")),
  CNS_first_author = replace_na(CNS_first_author, "0"))

#Figure 5. ----
fig5_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question %in% c("adjusted_gender", "teaching_status",
                         "faculty_offers", "peer")) %>% 
  distinct() %>% 
  spread(key = question, value = response) %>% 
  filter(!is.na(adjusted_gender)) %>% 
  filter(adjusted_gender != "NR") %>% 
  mutate(faculty_offers = case_when(
    as.numeric(faculty_offers) == 0 ~ "0",
    as.numeric(faculty_offers) == 1 ~ "1",
    as.numeric(faculty_offers) >= 1 ~ ">1"
  ),
  faculty_yn = if_else(as.numeric(faculty_offers) == 0, "no", "yes"), 
  faculty_offers = factor(faculty_offers, levels = c("0", "1", ">1")))

fig5_inst_bin_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "PUI_apps_submitted_binned" |
           question == "R1_apps_submitted_binned") %>% 
  distinct() %>% 
  rename(inst_type_bin = question, values_binned = response)

fig5_inst_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "PUI_apps_submitted" |
           question == "R1_apps_submitted") %>% 
  distinct() %>% 
  rename(inst_type = question, values = response)

fig5_teaching_types_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(question == "teaching_types") %>% 
  select(-question) %>% 
  distinct() %>% rename(teaching_type = response)

fig5_data_join <- left_join(fig5_data, fig5_inst_data, by = "id") %>% 
  left_join(.,  fig5_teaching_types_data, by = "id") %>% 
  left_join(.,  fig5_inst_bin_data, by = "id") 

fig5_data <- fig5_data_join %>% 
  mutate(inst_type = str_remove_all(inst_type, "(?<=PUI|R1).*"),
         inst_type_bin = str_remove_all(inst_type_bin, "(?<=PUI|R1).*"),
         inst_type = str_replace_all(inst_type, "1", "I"),
         inst_type_bin = str_replace_all(inst_type_bin, "1", "I")) %>% 
  distinct()

source("jadavji_biology/code/figure_4.R")

source("jadavji_biology/code/figure_5.R")

#Figure 6.----
#fig6_data <- bio_tidy_data %>% 
#  filter(question %in% c("scholar_citations_all", "application_cycles",
#                         "peer-reviewed_papers", "scholar_hindex",
#                         "first_author", "corresponding_author",
#                         "CNS_first_author", "transition_award",
#                         "CNS_status", "postdoc_fellow", "predoc_fellow"
#                         ))

#data tables----

#demo table
demo_table <- bio_tidy_data %>% 
  filter(question %in% c("adjusted_gender", "age", "research_category", 
                         "residence", "peer", "dependents",
                         "position", "legal_status", "disability_status", 
                         "first_gen_undergrad", "first_gen_phd")) %>% 
  mutate(response = fct_collapse(response, 
                                 "Citizen/Resident" = c("Citizen", "Permanent resident"),
                                 "Visa" = c("Student visa", "Work visa"),
                                 "Other" = c("Choose not to disclose", "Outside the US or CA"),
                                 "Yes" = c("Yes, hidden", "Yes, visible"),
                                 "Yes, multiple children/adult(s)" = c("Yes, adult(s)", 
                                                                       "Yes, multiple children",
                                                                       "Yes, adult(s) and child(ren)")
  )) %>% 
  count(question, response) %>% 
  mutate(percent_total = get_percent(n, 332))

write_csv(demo_table, paste0("jadavji_biology/figures/demographics_",
                             Sys.Date(), ".csv"))

#table of application metrics
metrics_table <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author", "corresponding_author",
                         "peer-reviewed_papers", "scholar_hindex",
                         "scholar_citations_all", "CNS_status",
                         "application_cycles", "faculty_offers",
                         "apps_submitted")) %>% 
  filter(response != "NR") %>% 
  distinct() %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE), 
            std_dev = round(sd(as.numeric(response, na.rm = TRUE)), 
                            digits = 2),
            min_val = min(as.numeric(response, na.rm = TRUE)),
            max_val = max(as.numeric(response, na.rm = TRUE))) %>% 
  mutate(range = paste0("(", min_val, ", ", max_val, ")")) %>% 
  select(-min_val, -max_val)

write_csv(metrics_table, paste0("jadavji_biology/figures/metrics_",
                                Sys.Date(), ".csv"))

metrics_table2 <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>%  
  filter(question %in% c("postdoc_fellow", "predoc_fellow",
                         "grant_pi", "grant_copi")) %>% 
  distinct() %>% 
  count(question, response) %>% 
  mutate(percent = get_percent(n, 332))

write_csv(metrics_table2, paste0("jadavji_biology/figures/grants_",
                                 Sys.Date(), ".csv"))