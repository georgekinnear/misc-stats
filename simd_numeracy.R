library(tidyverse)
library(janitor)

data = read.csv("simd_numeracy_acel201819.csv", header = TRUE, stringsAsFactors = FALSE)

names(data) = c("stage", "simd", "1617", "1718", "1819")

data_clean = data %>% 
  filter(str_detect(simd, "SIMD")) %>% 
  mutate(simd = as.factor(parse_number(simd)),
         stage = as.factor(stage),
         stage = fct_relevel(stage, "S3 (Fourth Level)", after = Inf)) %>% 
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
         year == "2018/19") %>% 
  ggplot(aes(x = simd, y = value)) +
  geom_bar(stat = "identity", aes(fill = simd)) + 
  scale_fill_brewer("SIMD quintile", type = "seq", palette = "RdYlBu") +
  theme_minimal(base_size = 16) +
  labs(
    title = "Percentage of P7 pupils attaining Second Level numeracy in 2018/19",
    x = "SIMD Quintile (1 = most deprived, 5 = least deprived)",
    y = "Percentage of pupils"
  )
ggsave("SIMD_1819_P7.png", width = 10, height = 5)

           