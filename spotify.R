# Pete Schultz
# -
# What Makes Top Songs More Popular?
# Data from Spotify API

library(performance)
library(see)
library(tidyverse)
library(dplyr)
library(magrittr)
library(olsrr)
library(car)
library(broom) 
library(kableExtra)
library(scales)
library(gridExtra)
library(stargazer)
library(webshot)
library(performance)
bold.14.text <- element_text(face = "bold", size = 14)

# Songs with popularity 70-90 (inclusive) / remove duplicated songs
songs <- read_csv("spotify_May15.csv")
songs %<>% drop_na(release_date)
songs %<>% filter(popularity >= 70 & popularity <= 90)
songs %<>% arrange(desc(popularity))
songs <- distinct(songs, track_id, .keep_all = TRUE)

# Remove tracks for sleeping
songs %<>% filter(!grepl('White Noise', name))
songs %<>% filter(!grepl('White Noise', artist))
songs %<>% filter(!grepl('Deep Sleep', name))
songs %<>% filter(!grepl('Deep Sleep', artist))
songs %<>% filter(!grepl('sleep', artist_genres))

# Keep useful variables
songs %<>% select(-c(track_id, artist_id, artist_popularity, artist_genres, album_id,
                     album, album_type, track_number, bpm, time_signature, loudness, 
                     speechiness, acousticness, instrumentalness, liveness))
glimpse(songs)


# Prepare variables for regression
# Duration
time_cuts <- c(-Inf, 120, 150, 180, 210, 240, Inf)
time_labs <- c("< 2:00", "2:00-2:30", "2:30-3:00", "3:00-3:30", '3:30-4:00', '> 4:00')
songs %<>% mutate(Duration = cut((duration_ms/1000), br = time_cuts, labels = time_labs))
songs %<>% select(-duration_ms)
table(songs$Duration)

# Month
month_cuts <- c(-Inf, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, Inf)
month_labs <- c("January", "February", "March", "April", 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
songs %<>% mutate(Month = cut(as.numeric(format(as.Date(release_date), format = '%m')), br = month_cuts, labels = month_labs))
songs %<>% select(-release_date)

# Artist Followers
songs %<>% mutate(artist_followers = artist_followers/50000)

# Modality
songs$mode <- factor(songs$mode)
songs$mode <- fct_collapse(songs$mode, 'Major' = '1', 'Minor' = '0')

# Key
songs$key <- factor(songs$key)
songs$key <- fct_collapse(songs$key,
                        'C' = '0',
                        '(C#, Db)' = '1',
                        'D' = '2',
                        '(D#, Eb)' = '3',
                        'E' = '4',
                        'F' = '5',
                        '(F#, Gb)' = '6',
                        'G' = '7',
                        '(G#, Ab)' = '8',
                        'A' = '9',
                        '(A#, Bb)' = '10',
                        'B' = '11')

# Content
songs$explicit <- factor(songs$explicit)
songs$explicit <- fct_collapse(songs$explicit,
                          'Clean' = 'FALSE',
                          'Explicit' = 'TRUE')
songs %<>% rename(content = explicit)

# Energy, Danceability, Valence
songs %<>% mutate(energy = energy * 10,
                  danceability = danceability * 10,
                  valence = valence * 10)


glimpse(songs)

# View correlations in dataset
cor(select_if(songs, is.numeric))
# Danceability and Artist Followers have some correlation

# Start regression
# Pre-model Fitting
pre_mod1 <- songs %>% ggplot(aes(x=danceability, y=popularity)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x = "Danceability", y = "Popularity")
pre_mod1
ggsave('pre_mod1.jpg', pre_mod1)

# Fitting the model
# Model 1
mod1 <- lm(popularity ~ danceability, data = songs)
summary(mod1)
# Model Summary in PQ
tidy_mod1 <- tidy(mod1)
tidy_mod1$term <- c('Intercept', 'Danceability')
tidy_mod1$estimate <- round(tidy_mod1$estimate, 3)
tidy_mod1$std.error <- round(tidy_mod1$std.error, 3)
tidy_mod1$statistic <- round(tidy_mod1$statistic, 2)
tidy_mod1 %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                          p.value >= 0.001 & p.value < 0.01 ~ 
                                            sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.01 & p.value < 0.05 ~
                                            sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.05 & p.value < 0.1 ~
                                            sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(tidy_mod1) <- c("Predictor", "Estimate", "Std. Error", "t-statistic", "p-value")
tname <- "Model 1: Characteristics associated with Song Popularity"
titlehead <- c(tname = 5)
names(titlehead) <- tname
mod_foot <- paste0("n = ", nrow(songs),
                   ". r-squared = ", round(summary(mod1)$adj.r.squared, 2),
                   ", F(", summary(mod1)$fstatistic[2], ",", summary(mod1)$fstatistic[3],
                   ") = ", round(summary(mod1)$fstatistic[1], 2), ".")
ref_foot <- ""
tidy_mod1 %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(tidy_mod1), extra_css = "border-bottom: solid;")  %>% 
  footnote(general = c(ref_foot, mod_foot)) %>% 
  save_kable("mod1.jpg")

# Check Assumptions
jpeg('mod1_assumptions_check.jpg', width = 730, height = 900)
check_model(mod1)
dev.off()

# Model 2
# Pre-model Fitting
pre_mod2 <- songs %>% ggplot(aes(x = content, y = popularity, fill = content)) +
  geom_boxplot() +
  labs(x = "Content", y = "Popularity") +
  theme(legend.title = element_blank(), ## remove legend title
        legend.position="bottom", ## move legend 
        axis.text.x = element_blank(), ## remove x-axis tick text
        axis.ticks = element_blank())  ## remove x-axis ticks
pre_mod2
ggsave('pre_mod2.jpg', pre_mod2)

# Fitting the model
mod2 <- lm(popularity ~ danceability + content, data = songs)
summary(mod2)

# Model Summary in PQ
tidy_mod2 <- tidy(mod2)
tidy_mod2$term <- c('Intercept', 'Danceability', 'Explicit')
tidy_mod2$estimate <- round(tidy_mod2$estimate, 3)
tidy_mod2$std.error <- round(tidy_mod2$std.error, 3)
tidy_mod2$statistic <- round(tidy_mod2$statistic, 2)
tidy_mod2 %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                          p.value >= 0.001 & p.value < 0.01 ~ 
                                            sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.01 & p.value < 0.05 ~
                                            sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.05 & p.value < 0.1 ~
                                            sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(tidy_mod2) <- c("Predictor", "Estimate", "Std. Error", "t-statistic", "p-value")
tname <- "Model 2: Characteristics associated with Song Popularity"
titlehead <- c(tname = 5)
names(titlehead) <- tname
mod_foot <- paste0("n = ", nrow(songs),
                   ". r-squared = ", round(summary(mod2)$adj.r.squared, 2),
                   ", F(", summary(mod2)$fstatistic[2], ",", summary(mod2)$fstatistic[3],
                   ") = ", round(summary(mod2)$fstatistic[1], 2), ".")
ref_foot <- ""
tidy_mod2 %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(tidy_mod2), extra_css = "border-bottom: solid;")  %>% 
  footnote(general = c(ref_foot, mod_foot)) %>% 
  save_kable("mod2.jpg")

# Model Comparison
mod_comp <- tidy(anova(mod1, mod2))
mod_comp <- mod_comp[-1,]
mod_comp %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                          p.value >= 0.001 & p.value < 0.01 ~ 
                                            sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.01 & p.value < 0.05 ~
                                            sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.05 & p.value < 0.1 ~
                                            sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(mod_comp) <- c("Residuals Degrees of Freedom", "Residual Sum of Squares", 
                         "Degrees of Freedom", "Sum of Squares", "T Statistic", "p-value")
tname <- "Comparison: Model 1 vs. Model 2"
titlehead <- c(tname = 6)
names(titlehead) <- tname
mod_comp %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(mod_comp), extra_css = "border-bottom: solid;")  %>% 
  save_kable("comparison_mod1_mod2.jpg")

# Check Assumptions
jpeg('mod2_assumptions_check.jpg', width = 730, height = 900)
check_model(mod2)
dev.off()

# Model 3
# Pre-model Fitting
pre_mod3 <- songs %>% ggplot(aes(x = Duration, y = popularity, fill = Duration)) +
  geom_boxplot() +
  labs(x = "Duration", y = "Popularity") +
  theme(legend.title = element_blank(), ## remove legend title
        legend.position="bottom", ## move legend 
        axis.text.x = element_blank(), ## remove x-axis tick text
        axis.ticks = element_blank())  ## remove x-axis ticks
pre_mod3
ggsave('pre_mod3.jpg', pre_mod3)

# Fitting the model
mod3 <- lm(popularity ~ danceability + content + relevel(Duration, ref = '> 4:00'), data = songs)
summary(mod3)

# Model Summary Output PQ
tidy_mod3 <- tidy(mod3)
tidy_mod3$term <- c('Intercept', 'Danceability', 'Explicit', '< 2:00', '2:00-2:30',
                    '2:30-3:00', '3:00-3:30', '3:30-4:00')
tidy_mod3$estimate <- round(tidy_mod3$estimate, 3)
tidy_mod3$std.error <- round(tidy_mod3$std.error, 3)
tidy_mod3$statistic <- round(tidy_mod3$statistic, 2)
tidy_mod3 %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                          p.value >= 0.001 & p.value < 0.01 ~ 
                                            sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.01 & p.value < 0.05 ~
                                            sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.05 & p.value < 0.1 ~
                                            sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(tidy_mod3) <- c("Predictor", "Estimate", "Std. Error", "t-statistic", "p-value")
tname <- "Model 3: Characteristics associated with Song Popularity"
titlehead <- c(tname = 5)
names(titlehead) <- tname
mod_foot <- paste0("n = ", nrow(songs),
                   ". r-squared = ", round(summary(mod3)$adj.r.squared, 2),
                   ", F(", summary(mod3)$fstatistic[2], ",", summary(mod3)$fstatistic[3],
                   ") = ", round(summary(mod3)$fstatistic[1], 2), ".")
ref_foot <- ""
tidy_mod3 %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(tidy_mod3), extra_css = "border-bottom: solid;")  %>% 
  footnote(general = c(ref_foot, mod_foot)) %>% 
  save_kable("mod3.jpg")

# Model Comparison
mod_comp <- tidy(anova(mod2, mod3))
mod_comp <- mod_comp[-1,]
mod_comp %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                         p.value >= 0.001 & p.value < 0.01 ~ 
                                           sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         p.value >= 0.01 & p.value < 0.05 ~
                                           sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         p.value >= 0.05 & p.value < 0.1 ~
                                           sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(mod_comp) <- c("Residuals Degrees of Freedom", "Residual Sum of Squares", 
                        "Degrees of Freedom", "Sum of Squares", "T Statistic", "p-value")
tname <- "Comparison: Model 2 vs. Model 3"
titlehead <- c(tname = 6)
names(titlehead) <- tname
mod_comp %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(mod_comp), extra_css = "border-bottom: solid;")  %>% 
  save_kable("comparison_mod2_mod3.jpg")

# Check Assumptions
jpeg('mod3_assumptions_check.jpg', width = 730, height = 900)
check_model(mod3)
dev.off()

# Model 4
# Pre-model Fitting
pre_mod4 <- songs %>% ggplot(aes(x = Month, y = popularity, fill = Month)) +
  geom_boxplot() +
  labs(x = "Month", y = "Popularity") +
  theme(legend.title = element_blank(), ## remove legend title
        legend.position="bottom", ## move legend 
        axis.text.x = element_blank(), ## remove x-axis tick text
        axis.ticks = element_blank())  ## remove x-axis ticks
pre_mod4
ggsave('pre_mod4.jpg', pre_mod4)

# Fitting the model
mod4 <- lm(popularity ~ danceability + content + relevel(Duration, ref = '> 4:00') + Month, data = songs)
summary(mod4)

# Model Summary in PQ
tidy_mod4 <- tidy(mod4)
tidy_mod4$term <- c('Intercept', 'Danceability', 'Explicit', '< 2:00', '2:00-2:30', '2:30-3:00', '3:00-3:30', '3:30-4:00',
                    'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
tidy_mod4$estimate <- round(tidy_mod4$estimate, 3)
tidy_mod4$std.error <- round(tidy_mod4$std.error, 3)
tidy_mod4$statistic <- round(tidy_mod4$statistic, 2)
tidy_mod4 %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                          p.value >= 0.001 & p.value < 0.01 ~ 
                                            sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.01 & p.value < 0.05 ~
                                            sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.05 & p.value < 0.1 ~
                                            sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(tidy_mod4) <- c("Predictor", "Estimate", "Std. Error", "t-statistic", "p-value")
tname <- "Model 4: Characteristics associated with Song Popularity"
titlehead <- c(tname = 5)
names(titlehead) <- tname
mod_foot <- paste0("n = ", nrow(songs),
                   ". r-squared = ", round(summary(mod4)$adj.r.squared, 2),
                   ", F(", summary(mod4)$fstatistic[2], ",", summary(mod4)$fstatistic[3],
                   ") = ", round(summary(mod4)$fstatistic[1], 2), ".")
ref_foot <- ""
tidy_mod4 %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(tidy_mod4), extra_css = "border-bottom: solid;")  %>% 
  footnote(general = c(ref_foot, mod_foot)) %>% 
  save_kable("mod4.jpg")

# Model Comparison
mod_comp <- tidy(anova(mod3, mod4))
mod_comp <- mod_comp[-1,]
mod_comp %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                         p.value >= 0.001 & p.value < 0.01 ~ 
                                           sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         p.value >= 0.01 & p.value < 0.05 ~
                                           sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         p.value >= 0.05 & p.value < 0.1 ~
                                           sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(mod_comp) <- c("Residuals Degrees of Freedom", "Residual Sum of Squares", 
                        "Degrees of Freedom", "Sum of Squares", "T Statistic", "p-value")
tname <- "Comparison: Model 3 vs. Model 4"
titlehead <- c(tname = 6)
names(titlehead) <- tname
mod_comp %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(mod_comp), extra_css = "border-bottom: solid;")  %>% 
  save_kable("comparison_mod3_mod4.jpg")

# Check Assumptions
jpeg('mod4_assumptions_check.jpg', width = 730, height = 900)
check_model(mod4)
dev.off()

# Model 5
# Pre-model Fitting
pre_mod5 <- songs %>% ggplot(aes(x=artist_followers, y=popularity)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x = "Artist Followers", y = "Popularity")
pre_mod5
ggsave('pre_mod5.jpg', pre_mod5)

# Fitting the model
mod5 <- lm(popularity ~ danceability + content + relevel(Duration, ref = '> 4:00') + Month + artist_followers, data = songs)
summary(mod5)

# Model Summary in PQ
tidy_mod5 <- tidy(mod5)
tidy_mod5$term <- c('Intercept', 'Danceability', 'Explicit', '< 2:00', '2:00-2:30', '2:30-3:00', '3:00-3:30', '3:30-4:00',
                    'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December',
                    'Artist Followers')
tidy_mod5$estimate <- round(tidy_mod5$estimate, 3)
tidy_mod5$std.error <- round(tidy_mod5$std.error, 3)
tidy_mod5$statistic <- round(tidy_mod5$statistic, 2)
tidy_mod5 %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                          p.value >= 0.001 & p.value < 0.01 ~ 
                                            sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.01 & p.value < 0.05 ~
                                            sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          p.value >= 0.05 & p.value < 0.1 ~
                                            sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                          TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(tidy_mod5) <- c("Predictor", "Estimate", "Std. Error", "t-statistic", "p-value")
tname <- "Model 5: Characteristics associated with Song Popularity"
titlehead <- c(tname = 5)
names(titlehead) <- tname
mod_foot <- paste0("n = ", nrow(songs),
                   ". r-squared = ", round(summary(mod5)$adj.r.squared, 2),
                   ", F(", summary(mod5)$fstatistic[2], ",", summary(mod5)$fstatistic[3],
                   ") = ", round(summary(mod5)$fstatistic[1], 2), ".")
ref_foot <- ""
tidy_mod5 %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(tidy_mod5), extra_css = "border-bottom: solid;")  %>% 
  footnote(general = c(ref_foot, mod_foot)) %>% 
  save_kable("mod5.jpg")

# Model Comparison
mod_comp <- tidy(anova(mod4, mod5))
mod_comp <- mod_comp[-1,]
mod_comp %<>% mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001 ***",
                                         p.value >= 0.001 & p.value < 0.01 ~ 
                                           sprintf('%.3f **', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         p.value >= 0.01 & p.value < 0.05 ~
                                           sprintf('%.3f *', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         p.value >= 0.05 & p.value < 0.1 ~
                                           sprintf('%.3f .', as.numeric(format(p.value, scientific = FALSE, nsmall = 3, digits = 0))),
                                         TRUE ~ format(p.value, scientific = FALSE, nsmall = 3, digits = 0)))
colnames(mod_comp) <- c("Residuals Degrees of Freedom", "Residual Sum of Squares", 
                        "Degrees of Freedom", "Sum of Squares", "T Statistic", "p-value")
tname <- "Comparison: Model 4 vs. Model 5"
titlehead <- c(tname = 6)
names(titlehead) <- tname
mod_comp %>% kable(booktabs = T, align = "rcccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(mod_comp), extra_css = "border-bottom: solid;")  %>% 
  save_kable("comparison_mod4_mod5.jpg")

# Check Assumptions
jpeg('mod5_assumptions_check.jpg', width = 730, height = 900)
check_model(mod5)
dev.off()

# Models Finished
# Summary Statistics for Numerical Variables
songs_nums <- songs %>% select(popularity, danceability, artist_followers)
stargazer(as.data.frame(songs_nums), 
          #note you have to specify type html
          type = "html",
          #note that the argument is "out" not "file"
          out = "songs_numdesc.html",
          title = "Summary Statistics of Numerical Variables", # descriptive overall table title
          # relabel variable names to descriptive names
          covariate.labels = c('Popularity', "Danceability", "Artist Followers"),
          digits = 2) # round values to two decimal places
webshot("songs_numdesc.html", "songs_numdesc.png")


# Univariate Statistics for Categorical Variables
df_content <- songs %>% group_by(content) %>% 
  summarize(Frequency = n(),
            Percent = paste0(round(n()/dim(songs)[1]*100, digits = 1), "%"))
df_duration <- songs %>% group_by(Duration) %>% 
  summarize(Frequency = n(),
            Percent = paste0(round(n()/dim(songs)[1]*100, digits = 1), "%"))
df_month <- songs %>% group_by(Month) %>% 
  summarize(Frequency = n(),
            Percent = paste0(round(n()/dim(songs)[1]*100, digits = 1), "%"))
colnames(df_content)[1] <- "Category"
colnames(df_duration)[1] <- "Category"
colnames(df_month)[1] <- "Category"
df_cat <- rbind(df_content, df_duration, df_month)
tname <- "Univariate Statistics for Categorical Variables"
titlehead <- c(tname = 3)
names(titlehead) <- tname
unicat <- df_cat %>% kable(booktabs = T, align = "lcc") %>% 
  kable_styling(full_width = FALSE) %>% 
  pack_rows("Content", 1, 2) %>% pack_rows("Duration", 3, 8) %>% pack_rows("Month", 9, 20) %>%
  add_header_above(header = titlehead, align = "l",
                   extra_css = "border-top: solid; border-bottom: double;") %>%
  row_spec(0, extra_css = "border-bottom: solid;") %>% 
  row_spec(nrow(df_cat), extra_css = "border-bottom: solid;") %>%
  save_kable("unicat.pdf")


saveRDS(songs,"spotify_data.rds")
