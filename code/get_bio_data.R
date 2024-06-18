#load data, functions, and packages----
source("code/load_data.R")

library(ggsignif)

# Identify biological science responses----
bio_clean <- clean_data %>% 
  filter(research_category == "Biological Sciences") %>% 
  #filter(survey_year == "2021-2022") %>% 
  distinct() 

bio_ids <- bio_clean %>% select(id) %>% distinct()

# Identify outliers----
metric_quals <- c("peer-reviewed_papers", "conference_abstracts", 
                  "corresponding_author", "first_author",
                  "scholar_citations_all", "scholar_hindex",
                  "apps_submitted")

metric_outliers <- map_dfr(.x = metric_quals, function(x) get_mad_outliers("bio_clean", x))

bio_clean_data <- bio_clean %>% 
  mutate(paper_outliers = if_else(as.numeric(`peer-reviewed_papers`) > metric_outliers[[1,3]], "yes", "no"),
         abstract_outliers = if_else(as.numeric(conference_abstracts) > metric_outliers[[2,3]], "yes", "no"),
         corres_outliers = if_else(as.numeric(corresponding_author) > metric_outliers[[3,3]], "yes", "no"),
         first_outliers = if_else(as.numeric(first_author) > metric_outliers[[4,3]], "yes", "no"),
         cites_outliers = if_else(as.numeric(scholar_citations_all) > metric_outliers[[5,3]], "yes", "no"),
         hindex_outliers = if_else(as.numeric(scholar_hindex) > metric_outliers[[6,3]], "yes", "no"),
         apps_sub_outliers = if_else(as.numeric(apps_submitted) > metric_outliers[[7,3]], "yes", "no"))

#write_csv(bio_clean_data, 
#          paste0("jadavji_biology/data/bio_clean_data",
#          Sys.Date(), ".csv")) #clean data file for lily

bio_tidy_data <- bio_clean_data %>% 
  gather(-id, key = "question", value = "response")

#write_csv(bio_tidy_data, 
#          paste0("jadavji_biology/data/bio_tidy_data_", 
#                 Sys.Date(), ".csv")) #tidy data file for lily w/out outliers

bio_inst_data <- left_join(bio_ids, inst_data, by = "id")

#pull ids of respondent outliers----
paper_outlier_ids <- bio_clean_data %>% filter(paper_outliers == "yes") %>% 
  pull(id) %>% unique()
abstract_outlier_ids <- bio_clean_data %>% filter(abstract_outliers == "yes") %>% 
  pull(id) %>% unique()
corres_outlier_ids <- bio_clean_data %>% filter(corres_outliers == "yes") %>% 
  pull(id) %>% unique()
first_outlier_ids <- bio_clean_data %>% filter(first_outliers == "yes") %>% 
  pull(id) %>% unique()
cites_outlier_ids <- bio_clean_data %>% filter(cites_outliers == "yes") %>% 
  pull(id) %>% unique()
hindex_outlier_ids <- bio_clean_data %>% filter(hindex_outliers == "yes") %>% 
  pull(id) %>% unique()
apps_sub_outlier_ids <- bio_clean_data %>% filter(apps_sub_outliers == "yes") %>% 
  pull(id) %>% unique()
  
#get themed datasets----
#bio_demo_data <- bio_clean_data %>% 
#  select(id, position, disability_status, age, residence, legal_status,
#         first_gen_undergrad, first_gen_phd, relationship_status, 
#         dependents, primary_caregiver, partner_education, partner_occupation,
#         income, student_loan, financial_support, extra_income, research_category,
#         biomedical, sexual_orientation, num_ethnicities, simple_race_ethnicity,
#         peer, gender, lgbtqia, adjusted_gender, simple_gender) %>% 
#  distinct()
#
#bio_network_data <- left_join(bio_ids, network_data, by = 'id') %>% 
#  distinct()
#
#bio_qualif_data <- bio_tidy_data %>% 
#  filter(section == "qualifications") %>% 
#  select(1:3) %>% 
#  distinct()
#
#bio_app_outcomes <- left_join(bio_ids, app_outcomes, by = 'id') %>% distinct()

#Figure 1.----
fig1_outlier_ids <- c(hindex_outlier_ids, cites_outlier_ids, paper_outlier_ids,
                      first_outlier_ids)

fig1_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(id %not_in% fig1_outlier_ids) %>% 
  filter(question %in% c("CNS_status", "CNS_first_author", "scholar_hindex",
                         "scholar_citations_all", "peer-reviewed_papers",
                         "first_author")) %>% 
  filter(!is.na(response)) %>% 
  filter(response != "NR") %>% 
  distinct()

source("jadavji_biology/code/figure_1.R")

#Figure 2. ----
fig2a_resp <- bio_clean_data %>% 
  filter(apps_sub_outliers == "no") %>% 
  select(id, RI_apps_submitted_binned, PUI_apps_submitted_binned) %>% 
  filter(!is.na(RI_apps_submitted_binned)) %>% 
  pull(id) %>% unique() %>% length()

fig2a_data <- bio_clean_data %>% 
  filter(apps_sub_outliers == "no") %>% 
  select(id, RI_apps_submitted_binned, PUI_apps_submitted_binned) %>% 
  filter(!is.na(RI_apps_submitted_binned)) %>% 
  gather(-id, key = question, value = response) %>% 
  mutate(question = if_else(str_detect(question, "RI"), "RI", "PUI")) %>% 
  distinct() %>% 
  filter(!is.na(response)) %>%
  count(question, response) %>% 
  mutate(percent = get_percent(n, fig2a_resp))

fig2b_resp <- bio_inst_data %>% 
  filter(inst_type == "offer_institutions") %>% 
  filter(inst_key == "PUI_RI") %>% 
  filter(!is.na(inst_value)) %>% 
  pull(id) %>% 
  unique() %>% length()

fig2b_data <- bio_inst_data %>% 
  filter(inst_type == "offer_institutions") %>% 
  filter(inst_key == "PUI_RI") %>% 
  filter(!is.na(inst_value)) %>% 
  distinct() %>% 
  count(id, inst_value) %>% 
  count(inst_value, n) %>%
  mutate(percent = get_percent(nn, fig2b_resp))

source("jadavji_biology/code/figure_2.R")

#Figure 3.----
fig3_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(id %not_in% apps_sub_outlier_ids) %>% 
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
  filter(id %not_in% apps_sub_outlier_ids) %>% 
  filter(question == "PUI_apps_submitted_binned" |
           question == "RI_apps_submitted_binned") %>% 
  distinct() %>% 
  rename(inst_type_bin = question, values_binned = response)

fig5_inst_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(id %not_in% apps_sub_outlier_ids) %>% 
  filter(question == "PUI_apps_submitted" |
           question == "RI_apps_submitted") %>% 
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
  mutate(inst_type = str_remove_all(inst_type, "(?<=PUI|RI).*"),
         inst_type_bin = str_remove_all(inst_type_bin, "(?<=PUI|RI).*")) %>% 
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
n_bio_resp <- nrow(bio_ids)

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
  )) %>% distinct() %>% 
  count(question, response) %>% 
  mutate(percent_total = get_percent(n, n_bio_resp))

write_csv(demo_table, paste0("jadavji_biology/figures/demographics_",
                             Sys.Date(), ".csv"))

#table of application metrics
metrics_table_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>% 
  filter(question %in% c("first_author", "corresponding_author",
                         "peer-reviewed_papers", "scholar_hindex",
                         "scholar_citations_all",
                         "application_cycles", "faculty_offers",
                         "apps_submitted")) %>% 
  filter(response != "NR") %>% 
  mutate(response = if_else(response == ">5", "5", response)) %>% 
  distinct() 

metrics_list <- select(metrics_table_data, question) %>% distinct() %>% unlist()

metrics_mode <- map_dfr(.x = metrics_list, function(x) get_mode("metrics_table_data", x)) %>% 
  spread(key = rowid, value = mode_value) %>% #tidy mode values
  rename(mode_value_1 = `1`, mode_value_2 = `2`)

metrics_table <- metrics_table_data %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE), 
            std_dev = round(sd(as.numeric(response, na.rm = TRUE)), 
                            digits = 2),
            min_val = min(as.numeric(response, na.rm = TRUE)),
            max_val = max(as.numeric(response, na.rm = TRUE))) %>% 
  mutate(range = paste0("(", min_val, ", ", max_val, ")")) %>% 
  select(-min_val, -max_val) %>% 
  left_join(., metrics_mode, by = "question") #join mode data

write_csv(metrics_table, paste0("jadavji_biology/figures/metrics_",
                                Sys.Date(), ".csv"))

#drop outliers from standard application metrics
metric_outlier_ids <- c(paper_outlier_ids, hindex_outlier_ids, cites_outlier_ids, 
                      first_outlier_ids, corres_outlier_ids, apps_sub_outlier_ids)

metrics_no_outlier_table_data <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>% 
  filter(id %not_in% metric_outlier_ids) %>% 
  filter(question %in% c("first_author", "corresponding_author",
                         "peer-reviewed_papers", "scholar_hindex",
                         "scholar_citations_all",
                         "application_cycles", "faculty_offers",
                         "apps_submitted")) %>% 
  filter(response != "NR") %>% 
  mutate(response = if_else(response == ">5", "5", response)) %>% 
  distinct() 

metrics_no_outlier_mode <- map_dfr(.x = metrics_list, 
                                   function(x) get_mode("metrics_no_outlier_table_data", x)) %>% 
  spread(key = rowid, value = mode_value) %>% #tidy mode values
  rename(mode_value_1 = `1`, mode_value_2 = `2`)

metrics_no_outlier_table <- metrics_no_outlier_table_data %>% 
  group_by(question) %>% 
  summarise(n = n(), med = median(as.numeric(response), 
                                  na.rm = TRUE), 
            std_dev = round(sd(as.numeric(response, na.rm = TRUE)), 
                            digits = 2),
            min_val = min(as.numeric(response, na.rm = TRUE)),
            max_val = max(as.numeric(response, na.rm = TRUE))) %>% 
  mutate(range = paste0("(", min_val, ", ", max_val, ")")) %>% 
  select(-min_val, -max_val) %>% 
  left_join(., metrics_no_outlier_mode, by = "question")

write_csv(metrics_no_outlier_table, paste0("jadavji_biology/figures/metrics_minus_outliers_",
                                Sys.Date(), ".csv"))
#grant and CNS status metrics
metrics_table2 <- bio_tidy_data %>% 
  select(id, question, response) %>% 
  filter(!is.na(question)) %>%  
  filter(question %in% c("postdoc_fellow", "predoc_fellow",
                         "grant_pi", "grant_copi", "CNS_status")) %>% 
  distinct() %>% 
  count(question, response) %>% 
  mutate(percent = get_percent(n, n_bio_resp))

write_csv(metrics_table2, paste0("jadavji_biology/figures/grants_CNS_",
                                 Sys.Date(), ".csv"))