### ANALYSE WHICH ARE THE MOST IMPORTANT CROPS AT THE NATIONAL LEVEL
# Rank area of crops using FAOSTAT
tab_area_rank_FAOSTAT <- FAOSTAT %>%
  filter(year %in% c(2000, 2010), variable == "area") %>%
  group_by(year) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, share, year) %>%
  spread(year, share) %>%
  arrange(desc(`2010`))

# aggregate adm1 tot adm0
ag_stat_crop_adm0 <- ag_stat %>%
  filter(variable %in% c("area", "production")) %>%
  group_by(id, year, adm_level, short_name, unit, variable, source) %>%
  summarize( value = sum(value, na.rm = T))

# Rank area of crops using all sources in base year
tab_area_rank_adm0 <- ag_stat_crop_adm0 %>%
  filter(variable == "area") %>%
  group_by(year, id) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, id, share, year) %>%
  filter(year %in% c(2000)) %>%
  spread(id, share) %>%
  arrange(desc(`FAOSTAT_0`))

### COMPARE DATA AT ADM1 LEVEL
# Area data
ag_stat_area_adm1 <- ag_stat %>%
  filter(variable %in% c("area"), adm_level == 1)

# Comparison of adm2 regions
fig_area_crop_adm1 <- ggplot(data = ag_stat_area_adm1, aes(x = year, y = value, colour = short_name, shape = id)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  facet_wrap(~adm, scales = "free") +
  labs(title = "Area comparison between FAOSTAT, am, as and cs",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm1

# Rank area of crops using all sources in base year
tab_area_rank_adm1 <- ag_stat_area_adm1 %>%
  group_by(year, adm, id) %>%
  mutate(share = round(value/sum(value, na.rm=T)*100, 2)) %>%
  arrange(desc(share)) %>%
  dplyr::select(short_name, id, share, year, adm) %>%
  filter(year %in% c(2000, 2007)) %>%
  spread(adm, share)

# Maps of adm2 with largest share of crop
# TO ADD

# Area comparison between crops in key years
fig_area_crop_adm0_2 <- ggplot(data = filter(ag_stat_crop_adm0, id == "FAOSTAT_0", variable == "area", 
                                             year %in% c(2000, 2010)), aes(x = factor(year), y = value, fill = short_name)) +
  geom_col() +
  facet_wrap(~ short_name, scales = "free") +
  labs(title = "Area comparison between crops FAOSTAT",
       y = "ha",
       x ="") +
  scale_y_continuous(labels=comma) +
  theme_bw() +
  theme(text = element_text(size=10))

fig_area_crop_adm0_2
