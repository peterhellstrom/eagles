library(eagles)

x <- tribble(
  ~folder_path,
  "B006_Daven_2023",
  "B022C_Björkö_2022",
  "G.Grisslehamn")

x %>% 
  slice(1:2) %>% 
  mutate(jwd_folder_names(folder_path))
