source("0_source.R")
library(googlesheets4)
gs4_auth(email = "sifan1121@gmail.com")
# file_url = "https://docs.google.com/spreadsheets/d/1Q-2XE_SIzR9o-kb3DJVzeXT47kUfYDUFjAR6OFwCZ_s"
file_url = "https://docs.google.com/spreadsheets/d/1W-n9AxVkKjgq_n09LpVdwDq1fj6HQXA22JDEidUx62s"

# 1. EDA -----
task_openai |> skim()

# Exposure ====
occ_task <- task_openai |> 
  select(-`Task ID`) |>
  mutate(occ_2 = paste0(str_sub(`O*NET-SOC Code`, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") 
  
# 2. ANALYZE -----
occ_openai_updated <- occ_task |> 
  group_by(occ_group, short_name, `O*NET-SOC Code`, Title) |> 
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))
  
# occupation groups exposure distribution
occ_openai_updated |> 
  select(-gpt4_automation) |> 
  pivot_longer(where(is.numeric)) |> 
  separate(name, into = c("name", "type"), sep = "_(?!.*_)") |>
  ggplot(aes(x = reorder(occ_group, value), y = value, fill = type)) +
  geom_boxplot() +
  guides(color = F, fill = F) +
  # scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  # facet_wrap(name~type) +
  labs(x = "Occupation group", y = "Exposure", fill = "Exposure type") +
  facet_wrap(~name)+
  theme_minimal()

occ_score <- occ_openai_updated |>
  mutate(SOC = str_sub(`O*NET-SOC Code`, 1, -4)) |>
  inner_join(us_occ_emp, by = "SOC") |>
  select(SOC, occ_group, short_name, Jobs = `2022 Jobs`, `Avg. Hourly Earnings`, mean_rating_human_alpha:gpt4_rubric1_gamma) |>
  group_by(short_name) |>
  summarise(across(`Avg. Hourly Earnings`:gpt4_rubric1_gamma, \(x) weighted.mean(x, Jobs, na.rm = T)),
            Jobs = sum(Jobs))

# occ_score |>
#   pivot_longer(mean_rating_human_alpha:gpt4_rubric1_gamma) |> 
#   ggplot(aes(x = reorder(short_name, value), y = value, color = name, group = name))+
#   geom_point()+
#   geom_line()+
#   coord_flip()+
#   labs(x = "Occupation group", y = "Exposure")+
#   theme_minimal()
# 
# occ_score |>
#   ggplot(aes(x = reorder(short_name, gpt4_rubric1_gamma), 
#              y = gpt4_rubric1_gamma))+
#   geom_col(fill = "#3182bd")+
#   geom_text(aes(label = scales::percent(gpt4_rubric1_gamma, accuracy = 0.1)),
#             hjust = -0.1)+
#   coord_flip()+
#   scale_y_continuous(labels = scales::percent, limits = c(0,1))+
#   labs(x = "Occupation group", y = "Percentage of tasks exposed to AI")+
#   theme_minimal()

score_emp <- task_openai |> 
  mutate(SOC = case_when(
    str_starts(`O*NET-SOC Code`, "25-1") ~ "25-1099", # xwalk to clean up
    T ~ str_sub(`O*NET-SOC Code`, 1, -4)
  )) |> 
  mutate(has_exposure = ifelse(gpt4_rubric1_beta > 0, 1, 0)) |>
  group_by(SOC) |> 
  summarise(across(where(is.numeric), mean, na.rm = T))|> 
  inner_join(us_occ_emp, by = "SOC") 

score_emp |> 
  # mutate(cutoff = has_exposure >= 0.7) |>
  # mutate(cutoff = has_exposure >= 0.5) |>
  # mutate(cutoff = gpt4_rubric1_alpha >= 0.7) |>
  # mutate(cutoff = gpt4_rubric1_beta >= 0.5) |>
  mutate(cutoff = gpt4_rubric1_beta >= 0.5) |>
  group_by(cutoff) |> 
  summarise(Count = n(), 
            Jobs = sum(`2022 Jobs`, na.rm = T)) |> 
  mutate(pct_count = Count/sum(Count),
         pct_jobs = Jobs/sum(Jobs, na.rm = T))

score_emp |> 
  mutate(cutoff = gpt4_rubric1_beta >= 0.1) |>
  # mutate(cutoff =  has_exposure == 1) |>
  group_by(cutoff) |> 
  summarise(Count = n(), 
            male_jobs = sum(`Current Year Males`, na.rm = T),
            female_jobs = sum(`Current Year Females`, na.rm = T)) |> 
  mutate(pct_count = Count/sum(Count),
         pct_male_jobs = male_jobs/sum(male_jobs, na.rm = T),
         pct_female_jobs = female_jobs/sum(female_jobs, na.rm = T))

# Table 1 Gender -----
score_emp |> 
  filter(`2022 Jobs` >= 1000000 & gpt4_rubric2_beta >= 0.5) |> 
  # filter(`2022 Jobs` >= 1000000 & has_exposure >= 0.5) |> 
  arrange(desc(has_exposure)) |>
  select(Description, `2022 Jobs`, `Median Hourly Earnings`, has_exposure, everything()) |> 
  write_sheet(file_url, sheet = "gender")

# Chart 1 distribution ---- 
score_emp |> 
  mutate(occ_2 = paste0(str_sub(SOC, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  filter(!is.na(`2022 Jobs`)) |> 
  # filter(`2022 Jobs` >= 1000000 & gpt4_rubric1_gamma >= 0.5) |> 
  ggplot(aes(x = reorder(occ_group, gpt4_rubric1_beta), y = gpt4_rubric1_beta, size = `2022 Jobs`))+
  geom_point(color = "#3182bd", alpha = 0.5, guide = F)+
  coord_flip()+
  scale_size_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Occupation group", y = "Exposure")+
  theme_classic()

# Chart 3 Occupation groups: bar chart on beta ----
fig1 <- score_emp |> 
  mutate(occ_2 = paste0(str_sub(SOC, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  filter(!is.na(`2022 Jobs`)) |> 
  select(SOC, occ_group, short_name, Jobs = `2022 Jobs`, `Avg. Hourly Earnings`, gpt4_rubric1_beta) |>
  group_by(short_name) |>
  summarise(across(`Avg. Hourly Earnings`:gpt4_rubric1_beta, \(x) weighted.mean(x, Jobs, na.rm = T))) 

write_sheet(fig1, file_url, sheet = "figure 1")

fig1|> 
  ggplot(aes(x = reorder(short_name, gpt4_rubric1_beta), y = gpt4_rubric1_beta))+
  geom_col(fill = "#3182bd")+
  # add label to bar end
  geom_text(aes(label = scales::percent(gpt4_rubric1_beta, accuracy = 0.1)), hjust = -0.1)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  labs(x = "Occupation group", y = "Exposure")+
  theme_minimal()

# Earnings ----
us_occ_emp |>
  mutate(earnings_cat = cut(`Avg. Hourly Earnings`, 
                            breaks = Hmisc::wtd.quantile(`Avg. Hourly Earnings`, 
                                                         probs = seq(0, 1, 0.25),
                                                         weights = `2022 Jobs`)), na.rm = TRUE) |> 
  count(earnings_cat)

score_emp|> 
  filter(!is.na(`2022 Jobs`)) |> 
  select(SOC, `2022 Jobs`, `Avg. Hourly Earnings`, mean_rating_human_alpha:gpt4_rubric1_gamma) |>
  mutate(earnings_cat = case_when(
    `Avg. Hourly Earnings` < 18 ~ "<18",
    `Avg. Hourly Earnings` >= 18 & `Avg. Hourly Earnings` < 23 ~ "18-23",
    `Avg. Hourly Earnings` >= 23 & `Avg. Hourly Earnings` < 37 ~ "23-37",
    `Avg. Hourly Earnings` >= 37 ~ "37+"
  )) |>
  # mutate(earnings_cat = cut(`Avg. Hourly Earnings`, 
  #                           breaks = quantile(`Avg. Hourly Earnings`, probs = seq(0, 1, 0.2)))) |>
  group_by(earnings_cat) |>
  summarise(across(`Avg. Hourly Earnings`:gpt4_rubric1_gamma, \(x) weighted.mean(x, `2022 Jobs`, na.rm = T))) |>
  pivot_longer(mean_rating_human_alpha:gpt4_rubric1_gamma) |> 
  ggplot(aes(x = earnings_cat, y = value, color = name, group = name))+
  geom_point()+
  geom_line()+
  coord_flip()+
  labs(x = "Average Hourly Earning (quartiles)", y = "Exposure")+
  theme_minimal()

score_emp|> 
  filter(!is.na(`2022 Jobs`)) |> 
  select(SOC, `2022 Jobs`, `Avg. Hourly Earnings`, mean_rating_human_alpha:gpt4_rubric1_gamma) |>
  mutate(earnings_cat = case_when(
    `Avg. Hourly Earnings` < 18 ~ "<18",
    `Avg. Hourly Earnings` >= 18 & `Avg. Hourly Earnings` < 23 ~ "18-23",
    `Avg. Hourly Earnings` >= 23 & `Avg. Hourly Earnings` < 37 ~ "23-37",
    `Avg. Hourly Earnings` >= 37 ~ "37+"
  )) |>
  # mutate(earnings_cat = cut(`Avg. Hourly Earnings`, 
  #                           breaks = quantile(`Avg. Hourly Earnings`, probs = seq(0, 1, 0.2)))) |>
  group_by(earnings_cat) |>
  summarise(across(`Avg. Hourly Earnings`:gpt4_rubric1_gamma, \(x) weighted.mean(x, `2022 Jobs`, na.rm = T))) |>
  ggplot(aes(x = earnings_cat, y = gpt4_rubric1_gamma))+
  geom_col(fill="#3182bd")+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Average Hourly Earning (quartiles)",
       y = "Percentage of tasks exposed to AI")+
  theme_minimal()

# Chart 2 Earning vs Exposure curve by occupation group ----
fig2 <- score_emp |> 
  mutate(occ_2 = paste0(str_sub(SOC, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  filter(!is.na(`2022 Jobs`)) |> 
  group_by(short_name) |>
  summarise(gpt4_rubric1_beta = weighted.mean(gpt4_rubric1_beta,  `2022 Jobs`, na.rm = T),
            `Avg. Hourly Earnings` = weighted.mean(`Avg. Hourly Earnings`, `2022 Jobs`, na.rm = T),
            `2022 Jobs` = sum(`2022 Jobs`)) |> 
  select(short_name, `2022 Jobs`, `Avg. Hourly Earnings`, gpt4_rubric1_beta) 

write_sheet(data = fig2, ss = file_url, 'figure 2')

fig2 |> 
  ggplot(aes(x = `Avg. Hourly Earnings`, y = gpt4_rubric1_beta, 
             size = `2022 Jobs`))+
  # geom_point(aes(color = short_name), alpha = 0.5)+
  geom_text(aes(label = short_name), size = 3, vjust = 1.5, check_overlap = T)+
  geom_point(color = "#3182bd", alpha = 0.5, guide = F)+
  # ggrepel::geom_label_repel(aes(label = short_name))+
  # geom_smooth(method = )+
  # linear line using geom_smooth
  geom_smooth(method = "lm", se = F)+
  
  scale_size_continuous(labels = scales::comma)+
  labs(x = "Average Hourly Earning", y = "Exposure")+
  theme_classic()

# Education ----
score_emp|> 
  filter(!is.na(`2022 Jobs`))|> 
  select(`Typical Entry Level Education`, `2022 Jobs`, `Avg. Hourly Earnings`, mean_rating_human_alpha:gpt4_rubric1_gamma) |>
  mutate(edu_cat = case_when(
    `Typical Entry Level Education` %in% c("Master's degree", "Doctoral or professional degree") ~ "4. Advanced Degree",
    `Typical Entry Level Education` %in% c("Bachelor's degree", "Associate's degree") ~ "3. Bachelor's or Associate's",
    `Typical Entry Level Education` %in% c("High school diploma or equivalent", "Some college, no degree") ~ "2. Some College",
    `Typical Entry Level Education` %in% c("No formal educational credential", "Postsecondary nondegree award") ~ "1. High School or below",
    TRUE ~ "Other"
  )) %>%
  group_by(edu_cat) |>
  summarise(across(`Avg. Hourly Earnings`:gpt4_rubric1_gamma, \(x) weighted.mean(x, `2022 Jobs`, na.rm = T))) |>
  pivot_longer(mean_rating_human_alpha:gpt4_rubric1_gamma) |> 
  ggplot(aes(x = edu_cat, y = value, color = name, group = name))+
  geom_point()+
  geom_line()+
  coord_flip()+
  labs(x = "Typical Entry Level Education", y = "Exposure")+
  theme_minimal()

# Training  ----
score_emp|> 
  filter(!is.na(`2022 Jobs`))|> 
  select(`Typical On-The-Job Training`, `2022 Jobs`, `Avg. Hourly Earnings`, mean_rating_human_alpha:gpt4_rubric1_gamma) |>
  mutate(train_cat = case_when(
    `Typical On-The-Job Training` %in% c("Apprenticeship", "Internship/residency") ~ "Apprenticeship/Internship",
    `Typical On-The-Job Training` %in% c("Long-term on-the-job training", "Moderate-term on-the-job training") ~ "Long/Moderate-term Training",
    `Typical On-The-Job Training` == "Short-term on-the-job training" ~ "Short-term Training",
    `Typical On-The-Job Training` == "None" ~ "No On-The-Job Training",
    `Typical On-The-Job Training` == "N/A" ~ "Not Applicable",
    TRUE ~ "Other"
  )) %>%
  mutate(train_cat = fct_relevel(train_cat, "No On-The-Job Training", "Short-term Training", "Long/Moderate-term Training", "Apprenticeship/Internship", "Not Applicable", "Other")) |>
  group_by(train_cat) |>
  summarise(across(`Avg. Hourly Earnings`:gpt4_rubric1_gamma, \(x) weighted.mean(x, `2022 Jobs`, na.rm = T))) |>
  pivot_longer(mean_rating_human_alpha:gpt4_rubric1_gamma) |> 
  ggplot(aes(x = train_cat, y = value, color = name, group = name))+
  geom_point()+
  geom_line()+
  coord_flip()+
  labs(x = "On-The-Job Training", y = "Exposure")+
  theme_minimal()

# Demographics: Gender ----
score_emp|> 
  filter(!is.na(`2022 Jobs`))|> 
  select(contains("Current"), `2022 Jobs`, `Avg. Hourly Earnings`, 
         mean_rating_human_alpha:has_exposure) |>
  pivot_longer(contains("Current"), names_to = "demo") |>
  mutate(demo = str_sub(demo, 14, -1)) |> 
  mutate(type = ifelse(demo %in% c("Females", "Males"), 'gender', 'race')) |> 
  group_by(type, demo) |>
  summarise(across(`Avg. Hourly Earnings`:has_exposure, \(x) weighted.mean(x, value/`2022 Jobs`, na.rm = T))) |>
  select(demo, has_exposure, gpt4_rubric1_beta) 
  # pivot_longer(mean_rating_human_alpha:gpt4_rubric1_gamma) |> 
  # ggplot(aes(x = reorder(demo, value), y = value, color = name, group = name))+
  # geom_point()+
  # geom_line()+
  # coord_flip()+
  # labs(x = "Gender", y = "Exposure")+
  # theme_minimal()+
  # facet_grid(type~., scales = "free")

# Industry groups ----

staffing |> 
  filter(`2022 Jobs` != "SOC") |>
  pivot_longer(3:949) |> 
  select(Occupation = `2022 Jobs`, naics6_code = name, Jobs = value) |> 
  mutate(Jobs = as.numeric(Jobs))

exposure_soc <- occ_openai_updated |> 
  mutate(Occupation = str_sub(`O*NET-SOC Code`, 1, -4)) |>
  inner_join(staffing, by = "Occupation") |> 
  mutate(naics2_code = str_sub(naics6_code, 1, 2)) |>
  mutate(naics2_code = case_when(
    naics2_code %in% c("31", "32", "33") ~ "31-33",
    naics2_code %in% c("44", "45") ~ "44-45",
    naics2_code %in% c("48", "49") ~ "48-49",
    naics2_code == "90" ~ "92",
    T ~ naics2_code
  )) |>
  left_join(naics_xwalk, by = "naics2_code") |>
  filter(!is.na(sector)) |>
  group_by(naics2_code, naics_short_name) |>
  summarise(across(mean_rating_human_alpha:gpt4_rubric1_gamma, \(x) weighted.mean(x, Jobs, na.rm = T))) 

exposure_soc |> 
  pivot_longer(mean_rating_human_alpha:gpt4_rubric1_gamma) |> 
  ggplot(aes(x = reorder(naics_short_name, value), y = value, color = name, group = name))+
  geom_point()+
  geom_line()+
  coord_flip()+
  labs(x = "Sector", y = "Exposure")+
  theme_minimal()

# metros ====

# Download the file
mm23_path <- "https://c24215cec6c97b637db6-9c0895f07c3474f6636f95b6bf3db172.ssl.cf1.rackcdn.com/interactives/2023/metro-monitor/assets/data/meta.json"
download.file(mm23_path, destfile = 'mm23_path.json', mode = "wb")

# Read the JSON file into R
library(jsonlite)
json_data <- fromJSON("mm23_path.json")
cbsa_18 <- as_tibble(json_data$geos) |> 
  mutate(cbsa_code = as.character(cbsa))

msa_score <- occ_openai_updated |> 
  mutate(Occupation = str_sub(`O*NET-SOC Code`, 1, -4)) |>
  inner_join(msa_occ_emp, by = "Occupation") |> 
  select(Area, `Area Name`, Jobs, mean_rating_human_alpha:gpt4_rubric1_gamma) |>
  mutate(cbsa_code = as.character(Area)) |>
  inner_join(cbsa_18, by = "cbsa_code") |> 
  group_by(cbsa_code, cbsa_name, size_class) |>
  summarise(across(mean_rating_human_alpha:gpt4_rubric1_gamma, \(x) weighted.mean(x, Jobs, na.rm = T)),
            Jobs = sum(Jobs, na.rm = T)) 

msa_score |> 
  filter(!is.na(size_class)) |> 
  group_by(size_class) |>
  summarise(across(mean_rating_human_alpha:gpt4_rubric1_gamma, \(x) weighted.mean(x, Jobs, na.rm = T))) |> 
  pivot_longer(mean_rating_human_alpha:gpt4_rubric1_gamma) |> 
  ggplot(aes(x = reorder(size_class, value), y = value, color = name, group = name))+
  geom_point()+
  geom_line()+
  coord_flip()+
  labs(x = "Metro size", y = "Exposure")+
  theme_minimal()


library(tmap)
# create a dot map using tmap package on msa_score
library(metro.data)
library(tigris)

st <- states(cb = TRUE, resolution = "20m") %>% shift_geometry()
cbsa <- core_based_statistical_areas(resolution = "20m") |> tigris::shift_geometry()
cbsa_map <- merge(cbsa, msa_score |> rename(GEOID = cbsa_code))

# tm_shape(st, projection = 2163) +
#   tm_polygons(alpha = 0.5, border.col = 'white') +
#   tm_layout(frame = F) +
#   tm_shape(cbsa_map) +
#   # tm_polygons(col = "gpt4_rubric1_gamma", palette = "RdYlBu", 
#   #             # border.col = "black", 
#   #             style = "quantile", n = 5) +
#   tm_bubbles(size = "Jobs",
#              col = "gpt4_rubric1_beta",
#              border.lwd = 0,
#              # remove border
#              
#              # border.col = "black",
#              # palette = "BuYlRd",
#              # alpha = 0.7,
#              # legend.format = list(fun = function(x) scales::percent(x, accuracy = 1)),
#              title.size = "2022 Jobs",
#              title.col = "Exposure to AI") +
#   tm_layout(legend.position = c("right", "bottom"))+
#   tm_format("legend", format = list(fun = function(x) scales::percent(x, accuracy = 1)))

ggplot() +
  geom_sf(data = st, fill = 'grey', color = "white") +
  geom_point(data = cbsa_map, aes(geometry = geometry, color = gpt4_rubric1_beta, size = Jobs), stat = "sf_coordinates") +
  scale_color_viridis_c(labels = scales::percent, name = "Exposure to AI") +
  # make color legend label percant format
  scale_size_continuous(labels = scales::comma, name = "2022 Employment") +
  theme_void(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5))
  
# save ggplot object as a png file, with transparent background
ggsave("msa_ai_exposure.png", width = 8, height = 6, units = "in", bg = "transparent")
ggsave("msa_ai_exposure.pdf", width = 8, height = 6, units = "in", bg = "transparent")
# 3. UPLOAD =====
# Write to google sheets

master_score <- score_emp |>
  select(SOC, Description, `2022 Jobs`, gpt4_rubric1_beta) |> 
  left_join(occ_ai_cat |> 
              mutate(SOC = case_when(
                str_starts(`O*NET-SOC Code`, "25-1") ~ "25-1099", # xwalk to clean up
                T ~ str_sub(`O*NET-SOC Code`, 1, -4)
              )) |>
              mutate(pct_weighted = ifelse(is.na(pct_weighted), 0, pct_weighted)) |>
              group_by(SOC, ai_cat) |> 
              summarise(pct_weighted = mean(pct_weighted)) |>
              pivot_wider(names_from = ai_cat, values_from = pct_weighted, values_fill = 0), 
            by = "SOC")

master_score_group <- master_score |> 
  mutate(occ_2 = paste0(str_sub(SOC, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  group_by(occ_2, occ_group) |>
  summarise(across(gpt4_rubric1_beta:`more likely automated`, mean), 
            `2022 Jobs` = sum(`2022 Jobs`))

write_sheet(data = master_score, ss = file_url, sheet = "Occupation")
write_sheet(data = master_score_group, ss = file_url, sheet = "Occupation Groups")
write_sheet(data = msa_score, ss = file_url, sheet = "exposure_Metro")
write_sheet(data = exposure_soc, ss = file_url, sheet = "exposure_Industry")

