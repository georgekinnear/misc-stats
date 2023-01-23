library(tidyverse)
library(janitor)

data = read.csv("simd_numeracy_acel202122.csv", header = TRUE, stringsAsFactors = FALSE)

names(data) = c("stage", "outcome", "simd", "1617", "1718", "1819", "1920", "2021", "2122")

data_clean = data %>% 
  select(-outcome) %>% 
  filter(str_detect(simd, "SIMD")) %>% 
  mutate(simd = as.factor(parse_number(simd)),
         stage = str_remove(stage, " or better"),
         stage = as.factor(stage),
         stage = fct_relevel(stage, "S3 - Fourth level", after = Inf)) %>% 
  # deal with the [x] values used to indicate missing data
  mutate(across(-c("stage", "simd"), ~ na_if(. , "[x]") %>% as.numeric)) %>% 
  pivot_longer(cols = -c("stage", "simd"), names_to = "year", values_to = "value") %>% 
  mutate(year = paste0("20", str_sub(year, 1, 2), "/", str_sub(year, 3, 4)))

data_clean %>% 
  ggplot(aes(x = stage, y = value)) +
  geom_point(aes(colour = simd), size = 3) +
  facet_grid(cols = vars(year)) +
  #scale_colour_viridis_d("SIMD", option = "inferno") +
  scale_color_brewer("SIMD quintile", type = "seq", palette = "RdYlBu") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Percentage of pupils attaining expected levels",
    x = "SIMD Quintile (1 = most deprived, 5 = least deprived)",
    y = "Percentage of pupils"
  )

data_clean %>% 
  ggplot(aes(x = year, y = value)) +
  geom_point(aes(colour = simd), size = 3) +
  facet_grid(cols = vars(stage)) +
  #scale_colour_viridis_d("SIMD", option = "inferno") +
  scale_color_brewer("SIMD quintile", type = "seq", palette = "RdYlBu") +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Percentage of pupils attaining expected levels",
    x = "SIMD Quintile (1 = most deprived, 5 = least deprived)",
    y = "Percentage of pupils"
  )
ggsave("SIMD_allyears_allstages.png", width = 10, height = 5)

data_clean %>% 
  filter(str_detect(stage, "P7"),
         year == "2021/22") %>% 
  ggplot(aes(x = simd, y = value)) +
  geom_bar(stat = "identity", aes(fill = simd)) + 
  scale_fill_brewer("SIMD quintile", type = "seq", palette = "RdYlBu") +
  theme_minimal(base_size = 16) +
  labs(
    title = "Percentage of P7 pupils attaining Second Level numeracy in 2021/22",
    x = "SIMD Quintile (1 = most deprived, 5 = least deprived)",
    y = "Percentage of pupils"
  )
ggsave("SIMD_2122_P7.png", width = 10, height = 5)

           