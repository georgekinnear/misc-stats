# Inglis et al. (2024)

``` r
library(tidyverse)
```

## Data

``` r
ref2021 <- read_csv(here::here("inglis-et-al-2024/REF2021_UnitResults35.csv"), show_col_types = FALSE)
```

Topics:

``` r
topic_names <- ref2021 %>% 
  select(matches("^T(\\d+)")) %>% 
  names()

topic_summary <- topic_names %>% 
  enframe() %>%  
  select(-name) %>% 
  separate_wider_delim(value, delim = " - ", names = c("code", "description"))

topic_summary
```

| code | description                                     |
|:-----|:------------------------------------------------|
| T1   | Communication and Interaction                   |
| T2   | Philosophy of Education                         |
| T3   | Methodological Depth                            |
| T4   | Citizenship and Culture                         |
| T5   | Developmental Psychology                        |
| T6   | Health and Medicine                             |
| T7   | Mathematics                                     |
| T8   | Gender                                          |
| T9   | Regional issues and international development   |
| T10  | Psychiatry and Psychopathology                  |
| T11  | Non-English Components                          |
| T12  | Language and Linguistics                        |
| T13  | Technology Enhanced Learning                    |
| T14  | Critical and Theory                             |
| T15  | History, Religion and Race                      |
| T16  | Early Childhood and Families                    |
| T17  | Leadership and Management                       |
| T18  | New Materialism                                 |
| T19  | Clinical Psychology and Developmental Disorders |
| T20  | Claims of Significance                          |
| T21  | Sports and Health                               |
| T22  | Analysing large-scale data                      |
| T23  | Schooling Systems                               |
| T24  | Affective Factors                               |
| T25  | Children’s Social Care                          |
| T26  | Education Policy                                |
| T27  | Higher Education                                |
| T28  | Training and Employment                         |
| T29  | Special Educational Needs and Disabilities      |
| T30  | Teacher Education and Professional Development  |
| T31  | Science Education                               |
| T32  | Interviews and Focus Groups                     |
| T33  | Systematic Reviews and Meta-Analyses            |
| T34  | Cognitive Processing                            |
| T35  | Reading                                         |

``` r
ref2021_clean <- ref2021 %>%
  rename_with(.cols = matches("^T(\\d+)"),
              .fn = ~ str_extract(.x, "^(T(\\d+))")) %>%
  select(-starts_with("Log2"))
```

Here we reconstruct the transformed columns, where each of the “Tx”
columns produces a corresponding “Log2(Tx)\_Log2(T3)” column, with the
values being log<sub>2</sub>(*T**x*/*T*3).

``` r
ref2021_Log2T3 <- ref2021_clean %>%
  mutate(across(
    matches("^T(\\d+)"),
    .fns = ~ log2(.x / T3),
    .names = "Log2({.col})_Log2(T3)"
  ))
```

## Regression

``` r
ref2021_Log2T3_regression_data <- ref2021_Log2T3 %>% 
  select(OutputsGPA, starts_with("Log2")) %>% 
  select(-starts_with("Log2(T3)"))

ref2021_Log2T3_regression <-
  lm(OutputsGPA ~ ., data = ref2021_Log2T3_regression_data)

summary(ref2021_Log2T3_regression)
```


    Call:
    lm(formula = OutputsGPA ~ ., data = ref2021_Log2T3_regression_data)

    Residuals:
         Min       1Q   Median       3Q      Max 
    -0.49830 -0.11718  0.00284  0.11365  0.43260 

    Coefficients:
                          Estimate Std. Error t value Pr(>|t|)    
    (Intercept)           2.953042   0.226521  13.036  < 2e-16 ***
    `Log2(T1)_Log2(T3)`   0.046596   0.032221   1.446 0.154635    
    `Log2(T2)_Log2(T3)`  -0.003847   0.050319  -0.076 0.939377    
    `Log2(T4)_Log2(T3)`  -0.008567   0.019649  -0.436 0.664777    
    `Log2(T5)_Log2(T3)`  -0.015352   0.021302  -0.721 0.474594    
    `Log2(T6)_Log2(T3)`  -0.004457   0.017136  -0.260 0.795921    
    `Log2(T7)_Log2(T3)`   0.010059   0.017766   0.566 0.573896    
    `Log2(T8)_Log2(T3)`   0.034541   0.021018   1.643 0.106829    
    `Log2(T9)_Log2(T3)`  -0.028843   0.023952  -1.204 0.234420    
    `Log2(T10)_Log2(T3)`  0.019354   0.022010   0.879 0.383605    
    `Log2(T11)_Log2(T3)`  0.019687   0.014803   1.330 0.189820    
    `Log2(T12)_Log2(T3)` -0.018152   0.015416  -1.177 0.244823    
    `Log2(T13)_Log2(T3)` -0.024220   0.023935  -1.012 0.316655    
    `Log2(T14)_Log2(T3)`  0.092485   0.055735   1.659 0.103562    
    `Log2(T15)_Log2(T3)`  0.029487   0.023822   1.238 0.221812    
    `Log2(T16)_Log2(T3)` -0.049569   0.027359  -1.812 0.076275 .  
    `Log2(T17)_Log2(T3)` -0.031249   0.035811  -0.873 0.387225    
    `Log2(T18)_Log2(T3)` -0.003802   0.031545  -0.121 0.904560    
    `Log2(T19)_Log2(T3)` -0.024091   0.013764  -1.750 0.086469 .  
    `Log2(T20)_Log2(T3)`  0.080790   0.035341   2.286 0.026710 *  
    `Log2(T21)_Log2(T3)` -0.007894   0.017099  -0.462 0.646399    
    `Log2(T22)_Log2(T3)`  0.104263   0.027700   3.764 0.000456 ***
    `Log2(T23)_Log2(T3)`  0.068284   0.040898   1.670 0.101504    
    `Log2(T24)_Log2(T3)` -0.020541   0.029229  -0.703 0.485586    
    `Log2(T25)_Log2(T3)` -0.001937   0.036440  -0.053 0.957827    
    `Log2(T26)_Log2(T3)` -0.017849   0.046660  -0.383 0.703753    
    `Log2(T27)_Log2(T3)` -0.101716   0.047504  -2.141 0.037362 *  
    `Log2(T28)_Log2(T3)` -0.014312   0.030448  -0.470 0.640456    
    `Log2(T29)_Log2(T3)`  0.002781   0.018679   0.149 0.882261    
    `Log2(T30)_Log2(T3)` -0.164368   0.048698  -3.375 0.001468 ** 
    `Log2(T31)_Log2(T3)`  0.002816   0.022797   0.124 0.902222    
    `Log2(T32)_Log2(T3)` -0.076661   0.087156  -0.880 0.383467    
    `Log2(T33)_Log2(T3)` -0.017836   0.022615  -0.789 0.434167    
    `Log2(T34)_Log2(T3)` -0.003968   0.026170  -0.152 0.880124    
    `Log2(T35)_Log2(T3)`  0.024070   0.016396   1.468 0.148624    
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.2387 on 48 degrees of freedom
    Multiple R-squared:  0.8412,    Adjusted R-squared:  0.7288 
    F-statistic: 7.481 on 34 and 48 DF,  p-value: 2.95e-10

This replicates the *R*<sup>2</sup> value of 0.8412415.

Regression coefficients:

``` r
ref2021_Log2T3_regression_coefficients <-
  ref2021_Log2T3_regression$coefficients %>% 
  as_tibble(rownames = "predictor") %>% 
  rename(coefficient = value) %>% 
  arrange(-coefficient) %>% 
  mutate(topic = case_when(
    predictor == "(Intercept)" ~ "Intercept",
    .default = str_extract(predictor, "Log2\\((T\\d+)\\)", group = 1)
  )) %>% 
  left_join(topic_summary, by = c("topic" = "code")) %>% 
  mutate(description = if_else(topic == "Intercept", "Intercept", description)) %>% 
  select(topic, description, coefficient)

ref2021_Log2T3_regression_coefficients
```

| topic     | description                                     | coefficient |
|:----------|:------------------------------------------------|------------:|
| Intercept | Intercept                                       |   2.9530423 |
| T22       | Analysing large-scale data                      |   0.1042627 |
| T14       | Critical and Theory                             |   0.0924845 |
| T20       | Claims of Significance                          |   0.0807902 |
| T23       | Schooling Systems                               |   0.0682843 |
| T1        | Communication and Interaction                   |   0.0465959 |
| T8        | Gender                                          |   0.0345414 |
| T15       | History, Religion and Race                      |   0.0294866 |
| T35       | Reading                                         |   0.0240703 |
| T11       | Non-English Components                          |   0.0196867 |
| T10       | Psychiatry and Psychopathology                  |   0.0193542 |
| T7        | Mathematics                                     |   0.0100592 |
| T31       | Science Education                               |   0.0028156 |
| T29       | Special Educational Needs and Disabilities      |   0.0027812 |
| T25       | Children’s Social Care                          |  -0.0019371 |
| T18       | New Materialism                                 |  -0.0038024 |
| T2        | Philosophy of Education                         |  -0.0038470 |
| T34       | Cognitive Processing                            |  -0.0039678 |
| T6        | Health and Medicine                             |  -0.0044565 |
| T21       | Sports and Health                               |  -0.0078940 |
| T4        | Citizenship and Culture                         |  -0.0085675 |
| T28       | Training and Employment                         |  -0.0143119 |
| T5        | Developmental Psychology                        |  -0.0153525 |
| T33       | Systematic Reviews and Meta-Analyses            |  -0.0178365 |
| T26       | Education Policy                                |  -0.0178491 |
| T12       | Language and Linguistics                        |  -0.0181521 |
| T24       | Affective Factors                               |  -0.0205414 |
| T19       | Clinical Psychology and Developmental Disorders |  -0.0240909 |
| T13       | Technology Enhanced Learning                    |  -0.0242204 |
| T9        | Regional issues and international development   |  -0.0288429 |
| T17       | Leadership and Management                       |  -0.0312492 |
| T16       | Early Childhood and Families                    |  -0.0495690 |
| T32       | Interviews and Focus Groups                     |  -0.0766610 |
| T27       | Higher Education                                |  -0.1017158 |
| T30       | Teacher Education and Professional Development  |  -0.1643683 |

This is missing the coefficient for T3. We can reconstruct that as the
negative of the sum of the other topic coefficients.

``` r
ref2021_Log2T3_regression_coefficients %>% 
  filter(topic != "Intercept") %>% 
  summarise(T3 = -sum(coefficient))
```

|        T3 |
|----------:|
| 0.1040203 |

(This makes sense, based on equation (7) of Coenders, G., &
Pawlowsky-Glahn, V. (2020). On interpretations of tests and effect sizes
in regression models with a compositional predictor. *SORT, 44*,
201–220. <https://doi.org/10.2436/20.8080.02.100> )

## Why T3?

Do we get the same results if we use a different topic in the
denominator instead of T3?

Let’s try T1…

``` r
ref2021_Log2T1 <- ref2021_clean %>%
  mutate(across(
    matches("^T(\\d+)"),
    .fns = ~ log2(.x / T1),
    .names = "Log2({.col})_Log2(T1)"
  ))

ref2021_Log2T1_regression_data <- ref2021_Log2T1 %>% 
  select(OutputsGPA, starts_with("Log2")) %>% 
  select(-starts_with("Log2(T1)"))

ref2021_Log2T1_regression <-
  lm(OutputsGPA ~ ., data = ref2021_Log2T1_regression_data)
```

The *R*<sup>2</sup> value is 0.8412415.

Regression coefficients:

``` r
ref2021_Log2T1_regression_coefficients <-
  ref2021_Log2T1_regression$coefficients %>% 
  as_tibble(rownames = "predictor") %>% 
  rename(coefficient = value) %>% 
  arrange(-coefficient) %>% 
  mutate(topic = case_when(
    predictor == "(Intercept)" ~ "Intercept",
    .default = str_extract(predictor, "Log2\\((T\\d+)\\)", group = 1)
  )) %>% 
  left_join(topic_summary, by = c("topic" = "code")) %>% 
  mutate(description = if_else(topic == "Intercept", "Intercept", description)) %>% 
  select(topic, description, coefficient)

ref2021_Log2T1_regression_coefficients
```

| topic     | description                                     | coefficient |
|:----------|:------------------------------------------------|------------:|
| Intercept | Intercept                                       |   2.9530423 |
| T22       | Analysing large-scale data                      |   0.1042627 |
| T3        | Methodological Depth                            |   0.1040203 |
| T14       | Critical and Theory                             |   0.0924845 |
| T20       | Claims of Significance                          |   0.0807902 |
| T23       | Schooling Systems                               |   0.0682843 |
| T8        | Gender                                          |   0.0345414 |
| T15       | History, Religion and Race                      |   0.0294866 |
| T35       | Reading                                         |   0.0240703 |
| T11       | Non-English Components                          |   0.0196867 |
| T10       | Psychiatry and Psychopathology                  |   0.0193542 |
| T7        | Mathematics                                     |   0.0100592 |
| T31       | Science Education                               |   0.0028156 |
| T29       | Special Educational Needs and Disabilities      |   0.0027812 |
| T25       | Children’s Social Care                          |  -0.0019371 |
| T18       | New Materialism                                 |  -0.0038024 |
| T2        | Philosophy of Education                         |  -0.0038470 |
| T34       | Cognitive Processing                            |  -0.0039678 |
| T6        | Health and Medicine                             |  -0.0044565 |
| T21       | Sports and Health                               |  -0.0078940 |
| T4        | Citizenship and Culture                         |  -0.0085675 |
| T28       | Training and Employment                         |  -0.0143119 |
| T5        | Developmental Psychology                        |  -0.0153525 |
| T33       | Systematic Reviews and Meta-Analyses            |  -0.0178365 |
| T26       | Education Policy                                |  -0.0178491 |
| T12       | Language and Linguistics                        |  -0.0181521 |
| T24       | Affective Factors                               |  -0.0205414 |
| T19       | Clinical Psychology and Developmental Disorders |  -0.0240909 |
| T13       | Technology Enhanced Learning                    |  -0.0242204 |
| T9        | Regional issues and international development   |  -0.0288429 |
| T17       | Leadership and Management                       |  -0.0312492 |
| T16       | Early Childhood and Families                    |  -0.0495690 |
| T32       | Interviews and Focus Groups                     |  -0.0766610 |
| T27       | Higher Education                                |  -0.1017158 |
| T30       | Teacher Education and Professional Development  |  -0.1643683 |

``` r
ref2021_Log2T1_regression_coefficients %>% 
  filter(topic != "Intercept") %>% 
  summarise(T1 = -sum(coefficient))
```

|        T1 |
|----------:|
| 0.0465959 |
