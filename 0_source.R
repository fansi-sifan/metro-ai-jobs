library(tidyverse)
library(tidylog)
library(skimr)

task_openai <- read_csv("data_raw/task ratings.csv")
occ_openai <- read_csv("data_raw/occ_level_abc (1).csv")
us_occ_emp <- read_csv("data_raw/us_occ_data.csv")
msa_occ_emp <- read_csv("data_raw/msa_occ_data.csv")

task_stat <- read_delim("data_raw/db_27_2_text/Task Statements.txt")
task_ratings <- read_delim("data_raw/db_27_2_text/Task Ratings.txt")
staffing <- read_csv("data_raw/us_staffing_matrix.csv") 

# onet_occ_xwalk <- read_csv("data_raw/2019_to_SOC_Crosswalk.csv")
# occ_xwalk <- openxlsx::read.xlsx("https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/2018-occupation-code-list-and-crosswalk.xlsx", sheet = 'sheet2')
occ_xwalk <- tibble::tribble(
  ~occ_2,            ~occ_group,                                    ~short_name,
  "11-0000",          "Management",                                  "Management",
  "13-0000",          "Business and Financial Operations",          "Business and Financial",
  "15-0000",          "Computer and Mathematical",                  "Computer and Mathematical",
  "17-0000",          "Architecture and Engineering",              "Architecture and Engineering",
  "19-0000",          "Life, Physical, and Social Science",         "Science",
  "21-0000",          "Community and Social Service",               "Community and Social Service",
  "23-0000",          "Legal",                                      "Legal",
  "25-0000",          "Educational Instruction and Library",        "Education and Library",
  "27-0000",          "Arts, Design, Entertainment, Sports, and Media", "Arts and Media",
  "29-0000",          "Healthcare Practitioners and Technical",     "Healthcare Practitioners and Technical",
  "31-0000",          "Healthcare Support",                         "Healthcare Support",
  "33-0000",          "Protective Service",                         "Protective Service",
  "35-0000",          "Food Preparation and Serving Related",       "Food Service",
  "37-0000",          "Building and Grounds Cleaning and Maintenance", "Cleaning and Maintenance",
  "39-0000",          "Personal Care and Service",                  "Personal Care and Service",
  "41-0000",          "Sales and Related",                          "Sales",
  "43-0000",          "Office and Administrative Support",         "Office and Administrative Support",
  "45-0000",          "Farming, Fishing, and Forestry",             "Farming and Forestry",
  "47-0000",          "Construction and Extraction",               "Construction and Extraction",
  "49-0000",          "Installation, Maintenance, and Repair",      "Installation and Repair",
  "51-0000",          "Production",                                 "Production",
  "53-0000",          "Transportation and Material Moving",         "Transportation and Moving"
)

naics_xwalk <- tibble::tribble(
  ~naics2_code,                  ~sector,                                      ~naics_short_name,
  "11",                 "Agriculture, Forestry, Fishing and Hunting",      "Agriculture",
  "21",                 "Mining, Quarrying, and Oil and Gas Extraction",   "Mining",
  "22",                 "Utilities",                                     "Utilities",
  "23",                 "Construction",                                 "Construction",
  "31-33",                 "Manufacturing",                                "Manufacturing",
  "42",                 "Wholesale Trade",                              "Wholesale Trade",
  "44-45",                 "Retail Trade",                                 "Retail Trade",
  "48-49",                 "Transportation and Warehousing",              "Transportation",
  "51",                 "Information",                                  "Information",
  "52",                 "Finance and Insurance",                        "Finance",
  "53",                 "Real Estate and Rental and Leasing",          "Real Estate",
  "54",                 "Professional, Scientific, and Technical Services", "Professional Services",
  "55",                 "Management of Companies and Enterprises",      "Management",
  "56",                 "Administrative and Support and Waste Management and Remediation Services", "Administrative Services",
  "61",                 "Educational Services",                         "Education",
  "62",                 "Health Care and Social Assistance",            "Health Care",
  "71",                 "Arts, Entertainment, and Recreation",         "Arts",
  "72",                 "Accommodation and Food Services",             "Accommodation",
  "81",                 "Other Services (except Public Administration)", "Other Services",
  "92",                 "Public Administration",                       "Public Administration"
)


