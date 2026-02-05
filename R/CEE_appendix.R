
# Communications Earth and Environment
# Wang, Mameno, Owake, Aikoh, and Shoji
# Quantifying the loss of recreational value derived urban green spaces under extreme heat and humidity
# Appendixes

pacman::p_load(readxl, tidyverse, modelsummary, flextable, officer,writexl,plotly,htmltools,patchwork,scales, ggsci, gridExtra, ggplot2,googleway)

source(here::here("CEE_code.R"))

# Appendix S1
write_xlsx(sheet_list, path = "AppendixS1.xlsx")
cat("📁 出力完了: AppendixS1.xlsx")

# Appendix S6
AppendixS6　<- RatioVis

# lnVP_ の列名を変換（例: lnVP_3450 → 34℃, 50%）
new_names <- names(AppendixS6) %>%
  map_chr(~ {
    if (str_detect(.x, "^lnVP_\\d{4}")) {
      temp <- substr(.x, 6, 7)
      hum  <- substr(.x, 8, 9)
      paste0(temp, "℃, ", hum, "%")
    } else {
      .x
    }
  })

# 列名を置換
names(AppendixS6) <- new_names

# Excelファイルに保存
write_xlsx(AppendixS6, path = "AppendixS6.xlsx")
cat("📁 出力完了: RatioData_named.xlsx\n")



# sensitivity tests
## Appendix S3
fuel_base  <- 160
speed_base <- 20
vot_base   <- 545.7

make_wage_hourly <- function(vot_base, work_ratio = 1/3) {
  vot_base * 10000 * (1/52) * (1/5) * (1/8) * work_ratio
}

# 1) 基準（1/3）
wage_hourly_base    <- make_wage_hourly(vot_base, work_ratio = 1/3)

# 2) Appendix S4（1/2）
wage_hourly_half    <- make_wage_hourly(vot_base, work_ratio = 0.5)

# 3) Appendix S4（1/4）
wage_hourly_quarter <- make_wage_hourly(vot_base, work_ratio = 0.25)

unit_cost <- function(fuel, speed, wage_hourly) {
  fuel/10 + wage_hourly / speed
}

# 基準
base_uc <- unit_cost(160, 20, wage_hourly_base)

# Appendix S4
uc_vot_half    <- unit_cost(160, 20, wage_hourly_half)
uc_vot_quarter <- unit_cost(160, 20, wage_hourly_quarter)

ratio_vot_half    <- uc_vot_half / base_uc
ratio_vot_quarter <- uc_vot_quarter / base_uc


df_cs_S4_half <- df_cs %>%
  mutate(across(-ugs, ~ .x * ratio_vot_half))　%>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S4_half <- df_value %>%
  mutate(across(-ugs, ~ .x * ratio_vot_half))　%>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )

df_combined_half <-df_value_S4_half %>%
  left_join(df_cs_S4_half, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_half = sum(value, na.rm = TRUE),
    cs_half = mean(cs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )

df_cs_S4_quarter <- df_cs %>%
  mutate(across(-ugs, ~ .x * ratio_vot_quarter))　%>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S4_quarter <- df_value %>%
  mutate(across(-ugs, ~ .x * ratio_vot_quarter))　%>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )

df_combined_quarter　<-df_value_S4_quarter %>%
  left_join(df_cs_S4_quarter, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_quarter = sum(value, na.rm = TRUE),
    cs_quarter = mean(cs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )

df_appendixS3 <- df_combined_quarter %>% 
  select(temp, hum, value_quarter, cs_quarter) %>% 
  left_join(
    df_combined_half %>% 
      select(temp, hum, value_half, cs_half),
    by = c("temp", "hum")
  )

write_xlsx(df_appendixS3,"AppendixS3.xlsx")

# 共通テーマを関数化すると楽
make_fig_S3 <- function(df, value_var, title_lab) {
  ggplot(df, aes(x = temp, y = hum, fill = {{ value_var }})) +
    geom_tile(color = "white") +
    geom_text(
      aes(
        label = paste0("USD\n", comma(round({{ value_var }}, 0))),
        color = text_color
      ),
      size = 4
    ) +
    scale_color_identity() +
    scale_fill_distiller(palette = "OrRd", direction = -1) +
    scale_x_continuous(breaks = c(24, 26, 28, 30, 32, 34)) +
    scale_y_continuous(breaks = c(50, 65, 80)) +
    labs(
      title = title_lab,
      x = "Temperature (℃)",
      y = "Humidity (%)",
      fill = "Value (USD)"
    ) +
    theme_minimal(base_family = "Hiragino Maru Gothic Pro") +
    theme(
      plot.title  = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y  = element_text(size = 10)
    )
}

fig_half    <- make_fig_S3(df_combined_half,    value_half,    "Value of time: 1/2")
fig_quarter <- make_fig_S3(df_combined_quarter, value_quarter, "Value of time: 1/4")

fig_S3 <- fig_half + fig_quarter + 
  plot_layout(nrow = 2, guides = "collect")

fig_S3

ggsave(
  filename = "AppendixS3_fig.png",
  plot = fig_S3,
  width = 10,
  height = 8,
  dpi = 500
)

## Appendix S4
uc_speed25 <- unit_cost(160, 25, wage_hourly_base)
uc_speed35 <- unit_cost(160, 35, wage_hourly_base)

ratio_speed25 <- uc_speed25 / base_uc
ratio_speed35 <- uc_speed35 / base_uc

df_cs_S4_25 <- df_cs %>%
  mutate(across(-ugs, ~ .x * ratio_speed25)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum  = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S4_25 <- df_value %>%
  mutate(across(-ugs, ~ .x * ratio_speed25)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum  = as.integer(str_extract(model, "[0-9]+$"))
  )

df_combined_S4_25 <- df_value_S4_25 %>%
  left_join(df_cs_S4_25, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_25 = sum(value, na.rm = TRUE),
    cs_25    = mean(cs,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )


df_cs_S4_35 <- df_cs %>%
  mutate(across(-ugs, ~ .x * ratio_speed35)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum  = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S4_35 <- df_value %>%
  mutate(across(-ugs, ~ .x * ratio_speed35)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum  = as.integer(str_extract(model, "[0-9]+$"))
  )
df_combined_S4_35 <- df_value_S4_35 %>%
  left_join(df_cs_S4_35, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_35 = sum(value, na.rm = TRUE),
    cs_35    = mean(cs,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )

df_appendixS4 <- df_combined_S4_25 %>%
  select(temp, hum, value_25, cs_25) %>%
  left_join(
    df_combined_S4_35 %>% select(temp, hum, value_35, cs_35),
    by = c("temp", "hum")
  )

KLA_walk <- KLA_raw %>%
  mutate(
    ugs = case_when(
      UGS == "(a)Takino" ~ "Takino",
      TRUE ~ UGS
    ),
    time_hours_walk = case_when(
      Dist <= 3~ Dist / 4,          
      TRUE~ Dist / speed_base  # 車（20 km/h）
    ),
    TC_walk = case_when(
      Dist <= 1 ~ wage_hourly_base * time_hours_walk,                                  # 徒歩
      TRUE      ~ fuel_base * Dist/10 + wage_hourly_base * time_hours_walk             # 車
    ),
    UC_walk = case_when(
      Dist > 0 ~ TC_walk / Dist,
      TRUE     ~ NA_real_
    ),
    ratio_walk = UC_walk / base_uc
  )

ratio_walk_ugs <- KLA_walk %>%
  group_by(ugs) %>%
  summarise(ratio_walk = mean(ratio_walk, na.rm = TRUE))

df_cs_S4_walk <- df_cs %>%
  left_join(ratio_walk_ugs, by = c("ugs")) %>% 
  mutate(across(-ugs, ~ .x * ratio_walk)) %>% 
  select(-ratio_walk) %>% 
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum  = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S4_walk <- df_value %>%
  left_join(ratio_walk_ugs, by = c("ugs")) %>% 
  mutate(across(-ugs, ~ .x * ratio_walk)) %>%
  select(-ratio_walk) %>% 
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum  = as.integer(str_extract(model, "[0-9]+$"))
  )
df_combined_S4_walk <- df_value_S4_walk %>%
  left_join(df_cs_S4_walk, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_walk = sum(value, na.rm = TRUE),
    cs_walk    = mean(cs,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )

df_appendixS4 <- df_appendixS4 %>%
  left_join(
    df_combined_S4_walk  %>% select(temp, hum, value_walk, cs_walk),
    by = c("temp", "hum")
  )

write_xlsx(df_appendixS4,"AppendixS4.xlsx")

# Figure S4
value_range <- range(
  df_combined_S4_25$value_25,
  df_combined_S4_35$value_35,
  df_combined_S4_walk$value_walk,
  na.rm = TRUE
)
make_fig_S4 <- function(df, value_var, title_lab, limits_vec) {
  ggplot(df, aes(x = temp, y = hum, fill = {{ value_var }})) +
    geom_tile(color = "white") +
    geom_text(
      aes(
        label = paste0("USD\n", comma(round({{ value_var }}, 0))),
        color = text_color
      ),
      size = 3
    ) +
    scale_color_identity() +
    scale_fill_distiller(
      palette   = "OrRd",
      direction = -1,
      limits    = limits_vec,
      oob       = scales::squish
    ) +
    scale_x_continuous(breaks = c(24, 26, 28, 30, 32, 34)) +
    scale_y_continuous(breaks = c(50, 65, 80)) +
    labs(
      title = title_lab,
      x     = "Temperature (℃)",
      y     = "Humidity (%)",
      fill  = "Value (USD)"
    ) +
    theme_minimal(base_family = "Hiragino Maru Gothic Pro") +
    theme(
      plot.title  = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x  = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y  = element_text(size = 10)
    )
}
fig_25   <- make_fig_S4(df_combined_S4_25,   value_25,   "Speed:25",        value_range)
fig_35   <- make_fig_S4(df_combined_S4_35,   value_35,   "Speed:35",        value_range)
fig_walk <- make_fig_S4(df_combined_S4_walk, value_walk, "including Walk",  value_range)

fig_S4 <- (fig_25 | fig_35) /
  (fig_walk | plot_spacer()) +   # 右下は空白パネル
  plot_layout(
    guides = "keep"              # それぞれ別の凡例なら keep
  )

ggsave("AppendixS4_fig.png",
       fig_S4, width = 15, height = 8, dpi = 500)


## Appendix S5
uc_fuel150 <- unit_cost(150, 20, wage_hourly_base)
uc_fuel170 <- unit_cost(170, 20, wage_hourly_base)
uc_fuel180 <- unit_cost(180, 20, wage_hourly_base)

ratio_fuel150 <- uc_fuel150 / base_uc
ratio_fuel170 <- uc_fuel170 / base_uc
ratio_fuel180 <- uc_fuel180 / base_uc

df_cs_S5_150 <- df_cs %>%
  mutate(across(-ugs, ~ .x * ratio_fuel150)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp  = as.integer(str_extract(model, "^[0-9]+")),
    hum   = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S5_150 <- df_value %>%
  mutate(across(-ugs, ~ .x * ratio_fuel150)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp  = as.integer(str_extract(model, "^[0-9]+")),
    hum   = as.integer(str_extract(model, "[0-9]+$"))
  )
df_combined_S5_150 <- df_value_S5_150 %>%
  left_join(df_cs_S5_150, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_150 = sum(value, na.rm = TRUE),
    cs_150 = mean(cs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(text_color = if_else(temp > 30 & hum >= 65, "white", "black"))


df_cs_S5_170 <- df_cs %>%
  mutate(across(-ugs, ~ .x * ratio_fuel170)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp  = as.integer(str_extract(model, "^[0-9]+")),
    hum   = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S5_170 <- df_value %>%
  mutate(across(-ugs, ~ .x * ratio_fuel170)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp  = as.integer(str_extract(model, "^[0-9]+")),
    hum   = as.integer(str_extract(model, "[0-9]+$"))
  )
df_combined_S5_170 <- df_value_S5_170 %>%
  left_join(df_cs_S5_170, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_170 = sum(value, na.rm = TRUE),
    cs_170 = mean(cs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(text_color = if_else(temp > 30 & hum >= 65, "white", "black"))

df_cs_S5_180 <- df_cs %>%
  mutate(across(-ugs, ~ .x * ratio_fuel180)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp  = as.integer(str_extract(model, "^[0-9]+")),
    hum   = as.integer(str_extract(model, "[0-9]+$"))
  )
df_value_S5_180 <- df_value %>%
  mutate(across(-ugs, ~ .x * ratio_fuel180)) %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp  = as.integer(str_extract(model, "^[0-9]+")),
    hum   = as.integer(str_extract(model, "[0-9]+$"))
  )
df_combined_S5_180 <- df_value_S5_180 %>%
  left_join(df_cs_S5_180, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    value_180 = sum(value, na.rm = TRUE),
    cs_180 = mean(cs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(text_color = if_else(temp > 30 & hum >= 65, "white", "black"))


df_appendixS5 <- df_combined_S5_150 %>%
  select(temp, hum, value_150, cs_150) %>%
  left_join(
    df_combined_S5_170 %>% select(temp, hum, value_170, cs_170),
    by = c("temp", "hum")
  ) %>%
  left_join(
    df_combined_S5_180 %>% select(temp, hum, value_180, cs_180),
    by = c("temp", "hum")
  )

write_xlsx(df_appendixS5, "AppendixS5.xlsx")


fig_fuel150 <- make_fig_S5(df_combined_S5_150, value_150, "Fuel price: 150", value_range)
fig_fuel170 <- make_fig_S5(df_combined_S5_170, value_170, "Fuel price: 170", value_range)
fig_fuel180 <- make_fig_S5(df_combined_S5_180, value_180, "Fuel price: 180", value_range)

fig_S5 <- fig_fuel150 | fig_fuel170
fig_S5 <- fig_S5 / (fig_fuel180 | plot_spacer()) +
  plot_layout(guides = "collect")

fig_S5

ggsave(
  filename = "AppendixS5_fig.png",
  plot = fig_S5,
  width = 15,
  height = 8,
  dpi = 500
)


## Appendix S9
raw_reuslt_i <- read_excel(here::here("data","CE_KLA_ZTCM","RPL_Parameters.xlsx"),sheet="Beta_I") %>% 
  rename("id"="...1","mt26"="1","mt28"="2","mt30"="3","mt32"="4","mt34"="5","hum65"="6","hum80"="7",
         "wpf"="8","ac"="9","mt"="10") %>% 
  mutate(time = -0.04108,
         ASC = -5.59134)



did <- read_excel(here::here("data","data_appendixS9.xlsx"), sheet="did")
ugs_loc <- read_excel(here::here("data","data_appendixS9.xlsx"), sheet="ugs")

origins <- did$location
destinations <- ugs_loc$location

api_key <- "XXXXXXXX"
set_key(api_key)

max_elements <- 100L
n_o <- length(origins)
n_d <- length(destinations)

dist_mat <- matrix(NA_real_, nrow = n_o, ncol = n_d)
time_mat <- matrix(NA_real_, nrow = n_o, ncol = n_d)

# for(i in seq_len(n_o)) {
#   
#   res_i <- google_distance(
#     origins      = origins[i],
#     destinations = destinations,
#     key          = api_key,
#     mode         = "driving",
#     simplify     = TRUE
#   )
#   
#   if (res_i$status != "OK") {
#     stop(paste("origin", i, "で status が", res_i$status, "です"))
#   }
#   
#   elems <- res_i$rows$elements[[1]]
#   
#   # 距離メートルと時間秒を行列に代入
#   dist_mat[i, ] <- elems$distance$value
#   time_mat[i, ] <- elems$duration$value
# }

rownames(dist_mat) <- did_coord$Cities
colnames(dist_mat) <- ugs_coord$ugs

dist_mat[1:3, 1:3]

dist_df <- as.data.frame(dist_mat)

dist_long <- dist_df %>% 
  dplyr::mutate(Cities = rownames(dist_df)) %>% 
  dplyr::relocate(Cities) %>% 
  tidyr::pivot_longer(
    cols      = -Cities,
    names_to  = "UGS",
    values_to = "distance_m"
  )　%>% 
  mutate(nDist = distance_m/1000*2)

dist <- read_excel(here::here("data","CE_KLA_ZTCM","KLA_Rawdata.xlsx")) %>% 
  select(ID, UGS,Year, Cities="Citis", Pop,  Vis, ln_VisPop="ln(Vis/Pop)", Dist="Dist...8",Chl,Eld) %>% 
  left_join(
    dist_long, by=c("UGS","Cities")
  ) %>%  
  mutate(nDist = case_when(UGS == "Moere"~Dist,
                           UGS == "(a)Takino"~Dist,
                           UGS == "Makoma"~Dist,
                           UGS == "SapporoArt"~Dist,
                           TRUE~nDist)) %>% select(-Dist) %>% 
  mutate(
    TC =  160*nDist/10 + 545.7*10000*(1/52)*(1/5)*(1/8)*(1/3)*nDist/20,
    TC = case_when(
      UGS == "(a)Takino"~TC+950,
      TRUE~TC
    )
  )

distance_aps9 <- dist  %>% 
  select(UGS, Year, Cities, nDist) %>% 
  pivot_wider(names_from = Cities, values_from = nDist) %>% 
  filter(Year == 2018) %>% 
  select(-Year)

park_aps9 <- distance_aps9 %>% 
  left_join(
    UGS_info, by = c("UGS")
  ) %>% 
  mutate(
    ugs = case_when(UGS == "(a)Takino" ~ "Takino",
                    TRUE ~ UGS),
    name_wpf = case_when(WPF == 1 ~ "wpf",
                         TRUE ~ "Nwpf"),
    name_ac = case_when(AC == 1 ~ "ac",
                        TRUE ~ "Nac")
  ) %>% 
  select(ugs, everything(), -c(UGS, WPF, AC))

park_rpm_aps9 <- df_reuslt_i %>% 
  left_join(
    park_aps9,
    by = c("name_wpf", "name_ac")
  )

park_rpmL_aps9 <- park_rpm_aps9 %>%
  pivot_longer(
    cols = c("Chuo", "Kita", "Higashi", "Shiroishi", "Atsubetsu",
             "Toyohira", "Kiyota", "Minami", "Nishi", "Teine", "Ishikari", 
             "Ebetsu", "Kitahiroshima"),
    names_to = "residence", values_to = "dist"
  ) %>%   
  mutate(v1 = v1_ext + (mt + time) * dist / 20)  %>% 
  select(-name_wpf, -name_ac)

result_ce_aps9 <- park_rpmL_aps9 %>% 
  group_by(ugs, residence, condition) %>%
  summarise(
    m_v1 = mean(v1),
    m_v0 = mean(v0),
    .groups = "drop"
  ) %>% 
  mutate(
    pr1 = 1 / (1 + exp(m_v0 - m_v1)),
    pr0 = 1 - pr1,
    condition = str_sub(condition, start = 1, end = 10)
  )  %>% 
  select(ugs, residence, condition, pr1) %>% 
  pivot_wider(names_from = condition, values_from = pr1)

ce_kla_aps9 <- result_ce_aps9 %>% 
  left_join(
    dist %>%  
      mutate(
        ugs = case_when(UGS == "(a)Takino" ~ "Takino",
                        TRUE ~ UGS)
      ),
    by = c("ugs", "residence" = "Cities")
  )

RatioVis_aps9 <- ce_kla_aps9 %>%
  mutate(
    lnVP_3450 = log(Vis * (mt34_hum50 / mt24_hum50) / Pop),
    lnVP_3250 = log(Vis * (mt32_hum50 / mt24_hum50) / Pop),
    lnVP_3050 = log(Vis * (mt30_hum50 / mt24_hum50) / Pop),
    lnVP_2850 = log(Vis * (mt28_hum50 / mt24_hum50) / Pop),
    lnVP_2650 = log(Vis * (mt26_hum50 / mt24_hum50) / Pop),
    lnVP_2450 = ln_VisPop,
    
    lnVP_3465 = log(Vis * (mt34_hum65 / mt24_hum50) / Pop),
    lnVP_3265 = log(Vis * (mt32_hum65 / mt24_hum50) / Pop),
    lnVP_3065 = log(Vis * (mt30_hum65 / mt24_hum50) / Pop),
    lnVP_2865 = log(Vis * (mt28_hum65 / mt24_hum50) / Pop),
    lnVP_2665 = log(Vis * (mt26_hum65 / mt24_hum50) / Pop),
    lnVP_2465 = log(Vis * (mt24_hum65 / mt24_hum50) / Pop),
    
    lnVP_3480 = log(Vis * (mt34_hum80 / mt24_hum50) / Pop),
    lnVP_3280 = log(Vis * (mt32_hum80 / mt24_hum50) / Pop),
    lnVP_3080 = log(Vis * (mt30_hum80 / mt24_hum50) / Pop),
    lnVP_2880 = log(Vis * (mt28_hum80 / mt24_hum50) / Pop),
    lnVP_2680 = log(Vis * (mt26_hum80 / mt24_hum50) / Pop),
    lnVP_2480 = log(Vis * (mt24_hum80 / mt24_hum50) / Pop)
  ) %>% 
  select(
    ID, ugs, residence, Year, contains("lnVP_"),
    nDist, TC, Chl, Eld
  ) %>% 
  filter(lnVP_3450 != -Inf)

ugs_list_aps9 <- unique(park_aps9$ugs)
sheet_list_aps9 <- list()  
all_models_aps9 <- list()

set.seed(123)
B <- 10000  

for (k in ugs_list_aps9) {
  models <- list()
  
  for (i in c(24, 26, 28, 30, 32, 34)) {
    for (j in c(50, 65, 80)) {
      a <- paste0("lnVP_", i, j)
      formula_str <- paste(a, "~ TC")
      
      df <- RatioVis_aps9 %>% filter(ugs == k)
      
      if (a %in% colnames(df) && sum(!is.na(df[[a]])) > 5) {
        
        boot_coefs <- replicate(B, {
          df_boot <- df %>% slice_sample(n = nrow(.), replace = TRUE)
          tryCatch({
            coef(lm(as.formula(formula_str), data = df_boot))
          }, error = function(e) rep(NA, 2))
        })
        
        boot_coefs <- t(boot_coefs)
        
        if (all(!is.na(boot_coefs))) {
          coef_mean <- colMeans(boot_coefs, na.rm = TRUE)
          coef_se <- apply(boot_coefs, 2, sd, na.rm = TRUE)
          coef_ci_low <- apply(boot_coefs, 2, quantile, probs = 0.025, na.rm = TRUE)
          coef_ci_high <- apply(boot_coefs, 2, quantile, probs = 0.975, na.rm = TRUE)
          
          p_values <- apply(boot_coefs, 2, function(x) {
            p_upper <- mean(x >= 0, na.rm = TRUE)
            p_lower <- mean(x <= 0, na.rm = TRUE)
            2 * min(p_upper, p_lower + .Machine$double.eps)
          })
          
          sig_flag <- p_values < 0.05
          stars <- cut(
            p_values,
            breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
            labels = c("***", "**", "*", ".", ""),
            right = FALSE
          )
          
          model_name <- paste0("lnVP_", i, "_", j)
          
          models[[model_name]] <- data.frame(
            term = names(coef_mean),
            estimate = coef_mean,
            std_error = coef_se,
            CI_low = coef_ci_low,
            CI_high = coef_ci_high,
            p_value = p_values,
            sig_flag = sig_flag,
            stars = as.character(stars)
          )
        }
      }
    }
  }
  
  if (length(models) > 0) {
    model_table <- bind_rows(models, .id = "model_name")
    sheet_list_aps9[[k]] <- model_table
  }
  all_models_aps9[[k]] <- models
}

coeff_df_aps9 <- do.call(
  rbind,
  lapply(names(all_models_aps9), function(ugs_name) {
    model_list <- all_models_aps9[[ugs_name]]
    
    do.call(
      rbind,
      lapply(names(model_list), function(model_name) {
        model_df <- model_list[[model_name]]
        
        intercept <- model_df %>% filter(term == "(Intercept)") %>% pull(estimate)
        tc <- model_df %>% filter(term == "TC") %>% pull(estimate)
        
        data.frame(
          ugs = ugs_name,
          model = model_name,
          intercept = intercept,
          TC = tc,
          stringsAsFactors = FALSE
        )
      })
    )
  })
)

cs_result_aps9 <- coeff_df_aps9 %>% 
  mutate(
    cs = (-1 / TC) / 150
  )

df_cs_aps9 <- cs_result_aps9 %>% 
  select(ugs, model, cs) %>% 
  pivot_wider(names_from = model, values_from = cs)

n_vis_aps9 <- dist %>% 
  mutate(
    ugs = case_when(UGS == "(a)Takino" ~ "Takino",
                    TRUE ~ UGS)
  ) %>% 
  group_by(ugs, Year) %>% 
  summarise(Vis = sum(Vis), .groups = "drop") %>% 
  group_by(ugs) %>% 
  summarise(vis = mean(Vis), .groups = "drop")

df_value_aps9 <- df_cs_aps9 %>% 
  left_join(n_vis_aps9, by = c("ugs")) %>% 
  mutate(
    across(
      starts_with("lnVP_"),
      ~ .x * vis,
      .names = "value_{.col}"
    )
  ) %>% 
  select(ugs, starts_with("value_"))

df_cs_long_aps9 <- df_cs_aps9 %>%
  pivot_longer(
    cols = -ugs,
    names_to = "model",
    values_to = "cs"
  ) %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )

df_value_long_aps9 <- df_value_aps9 %>%
  pivot_longer(
    cols = -ugs,
    names_to = "model",
    values_to = "value"
  ) %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )

df_combined_aps9 <- df_value_long_aps9 %>%
  left_join(df_cs_long_aps9, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    mean_cs = mean(cs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )

write_xlsx(df_combined_aps9, "AppendixS9.xlsx")

figS9 <- ggplot(df_combined_aps9, aes(x = temp, y = hum, fill = total_value)) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = paste0("USD           \n", comma(round(total_value, 0))),
        color = text_color),
    size = 4
  ) +
  scale_color_identity() +
  scale_fill_distiller(palette = "OrRd", direction = -1) +
  scale_x_continuous(breaks = c(24, 26, 28, 30, 32, 34)) + 
  scale_y_continuous(breaks = c(50, 65, 80)) + 
  labs(
    x = "Temperature (℃)", y = "Humidity (%)", fill = "Value (USD)"
  ) +
  theme_minimal(base_family = "Hiragino Maru Gothic Pro") +
  theme(
    plot.title = element_text(size = 16, face = "bold"), 
    axis.title.x  = element_text(size = 16),
    axis.title.y  = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

ggsave("figS09.png", plot = figS9, width = 9, height = 7, dpi = 500)
