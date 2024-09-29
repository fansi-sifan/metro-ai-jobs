source("0_source.R")
# Create categories ====
automation_threshold <- 0.5
augmentation_threshold <- 1

# assign categories to each task by automation score and exposure score
# automation: 0, 0.25, 0.5, 0.75, 0.1
# exposure: 0, 1
task_ai_cat <- task_openai |> 
  left_join(task_stat, by = c("O*NET-SOC Code", "Task ID", "Task")) |>
  mutate(ai_cat = case_when(
    gpt4_automation > automation_threshold ~ "more likely automated",
    gpt4_rubric1_gamma == augmentation_threshold ~ "more likely augmented",
    T ~ "minimal impact"
  )) |> 
  # filter(`O*NET-SOC Code` == "23-1011.00") |> 
  select(`O*NET-SOC Code`, Title, Task, ai_cat, `Task Type`) |> 
  mutate(task_weights = case_when(
    `Task Type` == "Core" ~ 2,
    `Task Type` == "Supplemental" ~ 0.5,
    T ~ 1
  )) |> 
  arrange(Title, ai_cat) 

# task level categories
task_ai_cat |> 
  count(`Task Type`, ai_cat) |> 
  group_by(`Task Type`) |>
  mutate(pct = n/sum(n)) |> 
  ggplot(aes(x = "", y = pct, fill = ai_cat, label = pct)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(pct)), position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  theme_void()+
  theme(legend.position = "bottom") +
  labs(title = "task level categories", 
       fill = "AI impact")+
  facet_wrap(vars(`Task Type`))

# map to occupations
occ_ai_cat <- task_ai_cat |>
  mutate(occ_2 = paste0(str_sub(`O*NET-SOC Code`, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  group_by(occ_group, `O*NET-SOC Code`, Title, ai_cat) |> 
  summarise(n = n(),
            n_weighted = sum(task_weights)) |>
  mutate(pct_tasks = n/sum(n),
         pct_weighted = n_weighted/sum(n_weighted))

# occupation groups with highest mean automation probability
mean_occ_ai_cat <- occ_ai_cat |> 
  select(-n, -n_weighted) |>
  pivot_wider(names_from = ai_cat, values_from = c(pct_tasks, pct_weighted))|>
  # view()
  group_by(occ_group) |>
  mutate(mean_automated = mean(`pct_tasks_more likely automated`, na.rm = T),
         mean_automated_weighted = mean(`pct_weighted_more likely automated`, na.rm = T)) 

mean_occ_ai_cat |> 
  ungroup() |> 
  arrange(desc(`pct_weighted_more likely automated`)) |>
  mutate(rank = row_number()) |> 
  select(Title, `pct_weighted_more likely automated`, rank) |> 
  view()

library(ggridges)
# ridge plot
occ_ai_cat |> 
  ggplot(aes(x = pct_weighted, 
             # y = reorder(occ_group, pct_tasks),
             y = ai_cat,
             fill = ai_cat
             )) +
  geom_density_ridges(alpha = 0.5) +
  scale_x_continuous(labels = scales::percent, limits = c(0,1))+
  labs(x = "Percentage of tasks in each category at occupation level", y = '')+
  guides(color = FALSE, fill = FALSE) +
  theme_minimal()


# all three categories
library(tidytext)
occ_ai_cat |> 
  group_by(occ_group, ai_cat) |>
  mutate(mean_value = mean(pct_tasks, na.rm = T)) |>
  mutate(group = tidytext::reorder_within(occ_group, pct_tasks, within = ai_cat)) %>% 
  ggplot(aes(x = reorder(group, pct_tasks), y = pct_tasks, fill = occ_group)) +
  geom_boxplot() +
  guides(color = F, fill = F) +
  coord_flip() +
  labs(x = "Occupation Groups",  y= "Percentage of tasks in the corresponding category")+
  tidytext::scale_x_reordered() +
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(vars(ai_cat), scales = "free_y")+
  theme_minimal()

occ_ai_cat |> 
  ungroup() |> 
  # filter(ai_cat == "more likely automated") |>
  # mutate(task_automation_pct = cut(pct_weighted, breaks = seq(0,1,0.1))) |>
  # count(task_automation_pct) 
  # arrange(pct_weighted) |> 
  # mutate(rank = row_number()) |> 
  # ggplot(aes(x = pct_weighted, y = rank))+
  # geom_point()
 # density plot
  ggplot(aes(x = pct_weighted, fill = ai_cat)) +
  geom_histogram(alpha = 0.5) +
  theme_minimal()+
  facet_wrap(~ai_cat)

mean_occ_ai_cat |>
  ggplot(aes(x = reorder(occ_group, mean_automated_weighted), 
             # y = `pct_tasks_more likely automated`, 
             y = `pct_weighted_more likely automated`, 
             fill = occ_group)) +
  geom_boxplot() +
  guides(color = FALSE, fill = FALSE) +
  scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  theme_minimal()+
  labs(x = "occupation groups", y = "Percentage of tasks in 'more likely automated'")

# occupation groups with highest mean augmentation probability
occ_ai_cat |> 
  select(-n) |> 
  pivot_wider(names_from = ai_cat, values_from = c(pct_tasks, pct_weighted)) |>
  # view()
  group_by(occ_group) |>
  mutate(mean_augmented = mean(`pct_tasks_more likely augmented`, na.rm = T),
         mean_augmented_weighted = mean(`pct_weighted_more likely augmented`, na.rm = T)) |>
  ggplot(aes(x = reorder(occ_group, mean_augmented_weighted), 
             # y = `more likely augmented`, 
             y = `pct_weighted_more likely augmented`, 
             fill = occ_group)) +
  geom_boxplot() +
  guides(color = F, fill = F) +
  scale_y_continuous(labels = scales::percent)+
  coord_flip() +
  theme_minimal()+
  labs(x = "occupation groups", y = "Percentage of tasks in 'more likely augmented'")


# correlations between two categories
occ_ai_cat |> 
  group_by(occ_group, ai_cat) |>
  summarise(mean_value = mean(pct_tasks, na.rm = T)) |>
  pivot_wider(names_from = ai_cat, values_from = mean_value) |>
  ggplot(aes(x = `more likely automated`, y = `more likely augmented`, label = occ_group)) +
  # ggrepel::geom_label_repel(size = 3)+
  scale_x_continuous(limits = c(0, 1))+
  scale_y_continuous(limits = c(0, 1))+
  # geom_text()+
  geom_point()


# 2. DEMOGRAPHIC =====
occ_ai_cat |> 
  select(-n, -n_weighted, -pct_tasks) |>
  pivot_wider(names_from = ai_cat, values_from = c(pct_weighted)) |>
  mutate(SOC = str_sub(`O*NET-SOC Code`, 1, -4)) |>
  inner_join(us_occ_emp, by = "SOC") |> 
  select(contains("Current"), `2022 Jobs`, `Avg. Hourly Earnings`, 
         `minimal impact`:`more likely automated`) |>
  pivot_longer(contains("Current"), names_to = "demo") |>
  mutate(demo = str_sub(demo, 14, -1)) |> 
  mutate(type = ifelse(demo %in% c("Females", "Males"), 'gender', 'race')) |> 
  group_by(type, demo) |>
  summarise(across(`Avg. Hourly Earnings`:`more likely automated` , \(x) weighted.mean(x, value/`2022 Jobs`, na.rm = T))) |>
  pivot_longer( `minimal impact`:`more likely automated`) |> 
  filter(name!="minimal impact") |> 
  ggplot(aes(x = reorder(demo, value), y = value, 
             fill = name,
             # color = name,
             group = name))+
  geom_col(position = "dodge")+
  # geom_text(aes(label = scales::percent(value)), 
  #           position = position_dodge(width = 0.9), vjust = -0.5)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("#b2df8a", "#fb9a99"))+
  labs(x = "Demographics", y = "Average % of tasks in each category")+
  theme_minimal()+
  facet_grid(type~., scales = "free")

# Industry groups ----

staffing |> 
  filter(`2022 Jobs` != "SOC") |>
  pivot_longer(3:949) |> 
  select(SOC = `2022 Jobs`, naics6_code = name, Jobs = value) |> 
  mutate(Jobs = as.numeric(Jobs))

occ_ai_cat |> 
  select(-n, -n_weighted, -pct_tasks) |>
  pivot_wider(names_from = ai_cat, values_from = c(pct_weighted)) |>
  mutate(SOC = str_sub(`O*NET-SOC Code`, 1, -4))|>
  inner_join(staffing, by = "SOC") |> 
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
  summarise(across( `minimal impact`:`more likely automated`, \(x) weighted.mean(x, Jobs, na.rm = T))) |> 
  pivot_longer( `minimal impact`:`more likely automated`) |> 
  filter(name!="minimal impact") |> 
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

msa_automation <- occ_ai_cat |> 
  select(-n, -n_weighted, -pct_tasks) |>
  pivot_wider(names_from = ai_cat, values_from = c(pct_weighted)) |>
  mutate(Occupation = str_sub(`O*NET-SOC Code`, 1, -4))|>
  inner_join(msa_occ_emp, by = "Occupation") |> 
  select(Area, `Area Name`, Jobs,  `minimal impact`:`more likely automated`) |>
  mutate(cbsa_code = as.character(Area)) |>
  inner_join(cbsa_18, by = "cbsa_code") |> 
  group_by(cbsa_code, cbsa_name, size_class) |>
  summarise(across( `minimal impact`:`more likely automated`, \(x) weighted.mean(x, Jobs, na.rm = T)),
            Jobs = sum(Jobs, na.rm = T)) 

msa_automation |> 
  filter(!is.na(size_class)) |> 
  group_by(size_class) |>
  summarise(across( `minimal impact`:`more likely automated`, \(x) weighted.mean(x, Jobs, na.rm = T))) |> 
  pivot_longer( `minimal impact`:`more likely automated`) |> 
  filter(name!="minimal impact") |> 
  ggplot(aes(x = reorder(size_class, value), y = value, color = name, group = name))+
  geom_point()+
  geom_line()+
  coord_flip()+
  labs(x = "Metro size", y = "Exposure")+
  theme_minimal()


library(tmap)
# create a dot map using tmap package on msa_score
library(metro.data)

st <- tigris::states() |> tigris::shift_geometry()
cbsa <- tigris::core_based_statistical_areas() |> tigris::shift_geometry()
cbsa_map <- merge(cbsa, msa_automation |> 
                    rename(GEOID = cbsa_code) |> 
                    pivot_longer( `minimal impact`:`more likely automated`) |>
                    filter(name != "minimal impact"))

tm_shape(st, projection = 2163) +
  tm_polygons(alpha = 0.5, border.col = "white") +
  tm_layout(frame = F) +
  tm_shape(cbsa_map) +
  # tm_polygons(col = "gpt4_rubric1_gamma", palette = "RdYlBu", 
  #             # border.col = "black", 
  #             style = "quantile", n = 5) +
  tm_bubbles(size = "Jobs",
             col = "value",
             # border.col = "black",
             palette = "-RdYlGn",
             # alpha = 0.7,
             # legend.format = list(fun = function(x) scales::percent(x, accuracy = 1)),
             title.size = "2022 Jobs") + 
  tm_facets(by = "name", 
            nrow = 2,
            free.scales.symbol.col = T)+
  tm_layout(outer.margins = 0, 
            inner.margins = 0, 
            legend.position = c("left", "bottom"))


# 3. UPLOAD =====
library(googlesheets4)
gs4_auth(email = "sifan1121@gmail.com")
automation_url <- "https://docs.google.com/spreadsheets/d/1LkNu4n1NpF1_wlPy_m5HaEbtLX-FRjHEIOt38eBxQWg/edit#gid=0"

# Write to google sheets
write_sheet(data = gpt4_automation, ss = automation_url, sheet = "automation_mean_score_occupation")
write_sheet(data = mean_occ_ai_cat, ss = automation_url, sheet = "automation_category_occupation")
write_sheet(data = msa_automation, ss = automation_url , sheet = "automation_Metro")


# SANDBOX ======
# check outliers
task_openai |> 
  filter(gpt4_automation == 1 & gpt4_rubric1_gamma == 0  & mean_rating_human_gamma == 0) |> 
  view()

gpt4_automation <- task_openai |>
  # skim(gpt4_automation)
  mutate(occ_2 = paste0(str_sub(`O*NET-SOC Code`, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  group_by(occ_group, `O*NET-SOC Code`, Title) |> 
  summarise(mean_rating = mean(gpt4_automation),
            sd_rating = sd(gpt4_automation)) 

ggplot() +
  geom_point(aes(x = mean_rating, y = sd_rating, color = occ_group, label = Title))+
  theme_classic()

score_compare <- task_openai |>
  mutate(occ_2 = paste0(str_sub(`O*NET-SOC Code`, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  # select(occ_group, `O*NET-SOC Code`, Title) |> 
  pivot_longer(mean_rating_human_alpha:gpt4_automation) |>
  group_by(occ_group, `O*NET-SOC Code`, Title, name) |> 
  summarise(mean_rating = mean(value),
            sd_rating = sd(value)) 

chart_score_compare <- score_compare|> 
  # filter(name == "mean_rating_human_alpha") |>
  # filter(name == "mean_rating_human_beta") |> 
  # filter(Title == "Graphic Designers")
  ggplot() +
  geom_point(aes(x = mean_rating, y = sd_rating, color = name, label = Title))+
  # facet_wrap(~name, scales = "free") +
  theme_classic()

plotly::ggplotly(gpt4_automation)
plotly::ggplotly(chart_score_compare)

# % tasks automated
score_compare |> 
  select(-sd_rating) |>
  pivot_wider(names_from = name, values_from = mean_rating) |> 
  ggplot(aes(x = mean_rating_human_gamma, 
             # y = gpt4_rubric1_gamma, 
             y = gpt4_automation,
             # color = occ_group,
             label = Title)) +
  geom_point() +
  theme_classic()


task_openai |>
  mutate(occ_2 = paste0(str_sub(`O*NET-SOC Code`, 1, 2), "-0000")) |> 
  left_join(occ_xwalk, by = "occ_2") |> 
  # select(occ_group, `O*NET-SOC Code`, Title) |> 
  pivot_longer(mean_rating_human_alpha:gpt4_automation) |>
  group_by(occ_group, name) |> 
  summarise(mean_rating = mean(value),
            sd_rating = sd(value)) |> 
  filter(name == "gpt4_automation") |> 
  ggplot(aes(x = mean_rating, y = sd_rating, color = occ_group, label = occ_group)) +
  geom_point() +
  geom_text()+
  theme_classic()

score_compare |> 
  filter(name == "gpt4_automation") |>
  view()

