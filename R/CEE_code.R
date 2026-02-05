
# Communications Earth and Environment
# Wang, Mameno, Owake, Aikoh, and Shoji
# Quantifying the loss of recreational value derived urban green spaces under extreme heat and humidity
# Main results

pacman::p_load(readxl, tidyverse, modelsummary, flextable, officer,writexl,plotly,htmltools,patchwork,scales, ggsci, gridExtra, ggplot2)

# data import
raw_reuslt_i <- read_excel(here::here("data","data_R","CE_KLA_ZTCM","RPL_Parameters.xlsx"),sheet="Beta_I") %>% 
  rename("id"="...1","mt26"="1","mt28"="2","mt30"="3","mt32"="4","mt34"="5","hum65"="6","hum80"="7",
         "wpf"="8","ac"="9","mt"="10") %>% 
  mutate(time = -0.04108,
         ASC = -5.59134)

KLA_raw <- read_excel(here::here("data","data_R","CE_KLA_ZTCM","KLA_Rawdata.xlsx")) %>% 
  select(ID, UGS,Year, Citis, Pop,  Vis, ln_VisPop="ln(Vis/Pop)", Dist="Dist...8",Chl,Eld) %>% 
  mutate(
    TC =  160*Dist/10 + 545.7*10000*(1/52)*(1/5)*(1/8)*(1/3)*Dist/20,
    TC = case_when(
      UGS == "(a)Takino"~TC+950,
      TRUE~TC
    )
  ) 

# calculate utility for each condition
df_reuslt_i <- raw_reuslt_i %>% 
  mutate(mt24 = -mt26-mt28-mt30-mt32-mt34,
         hum50 = -hum65-hum80,
         Nwpf = -wpf,
         Nac = -ac,
         v0 = mt24 + hum50 + Nwpf + ac + ASC)　%>% 
  # reshape wide to long
  pivot_longer(cols = c("mt24","mt26", "mt28", "mt30", "mt32","mt34"),
               names_to = "name_temp", values_to = "temp")  %>% 
  pivot_longer(cols = contains("hum"),
               names_to = "name_hum", values_to = "hum")  %>% 
  pivot_longer(cols = c("wpf","Nwpf"),
               names_to = "name_wpf", values_to = "wpf") %>% 
  pivot_longer(cols = c("ac","Nac"),
               names_to = "name_ac", values_to = "ac") %>% 
  # calculate mt and v1 (excluding mt and time due to relation with dist)
  mutate(
    mt = mt * (str_sub(name_temp, start=3) %>% as.numeric()),
    v1_ext =  temp + hum +  wpf  +  ac,
    condition = paste(name_temp, name_hum, name_wpf, name_ac,sep="_")
  )  %>% 
  select(id, condition, v1_ext, v0, mt, time, name_wpf, name_ac)


# creating distance data 
distance <- KLA_raw  %>% select(UGS,Year, Citis, Dist) %>% 
  pivot_wider(names_from = Citis, values_from = Dist) %>% 
  filter(Year==2018) %>% select(-Year)

# connecting data of distance and characteristics of park
UGS_info <- read_excel(here::here("data","data_R","UGS_info.xlsx")) %>% select(-ID)
park <- distance %>% 
  left_join(
    UGS_info,   by = c("UGS")
  ) %>% 
  mutate(
    ugs = case_when(UGS =="(a)Takino"~"Takino",
                    TRUE ~ UGS),
    name_wpf= case_when(WPF==1~"wpf",
                        TRUE~"Nwpf"),
    name_ac= case_when(AC==1~"ac",
                       TRUE~"Nac")
  ) %>% 
  select(ugs,everything(),-c(UGS,WPF,AC))


# connecting data of park and results of rpl 
park_rpm <- df_reuslt_i %>% left_join(
  park,
  by = c("name_wpf", "name_ac")
)

# time * utility
park_rpmL <- park_rpm %>%
  pivot_longer(cols = c("Chuo", "Kita", "Higashi", "Shiroishi", "Atsubetsu",
                        "Toyohira", "Kiyota", "Minami", "Nishi", "Teine", "Ishikari", 
                        "Ebetsu", "Kitahiroshima"),
               names_to = "residence", values_to = "dist") %>%   
  mutate(v1 = v1_ext + (mt + time)*dist/20)  %>% 
  select(-name_wpf, -name_ac)

# probability 
result_ce <- park_rpmL　%>% 
  group_by(ugs,residence,condition)　%>%
  summarise(m_v1 = mean(v1),
            m_v0 = mean(v0)
  ) %>% 
  mutate(
    pr1 = 1/(1+exp(m_v0-m_v1)),
    pr0 = 1- pr1,
    condition = str_sub(condition, start=1, end=10)
  )  %>% 
  select(ugs,residence, condition, pr1) %>% 
  pivot_wider(names_from = condition, values_from = pr1) 


# connecting data of ce and kla 
ce_kla <- result_ce %>% 
  left_join(
    KLA_raw %>%  
      mutate(
        ugs = case_when(UGS =="(a)Takino"~"Takino",
                        TRUE ~ UGS))
    , by=c("ugs","residence"="Citis")
  )


RatioVis <- ce_kla %>%
  mutate(
    lnVP_3450 = log(Vis*(mt34_hum50/mt24_hum50)/Pop),
    lnVP_3250 = log(Vis*(mt32_hum50/mt24_hum50)/Pop),
    lnVP_3050 = log(Vis*(mt30_hum50/mt24_hum50)/Pop),
    lnVP_2850 = log(Vis*(mt28_hum50/mt24_hum50)/Pop),
    lnVP_2650 = log(Vis*(mt26_hum50/mt24_hum50)/Pop),
    lnVP_2450 = ln_VisPop,
    
    lnVP_3465 = log(Vis*(mt34_hum65/mt24_hum50)/Pop),
    lnVP_3265 = log(Vis*(mt32_hum65/mt24_hum50)/Pop),
    lnVP_3065 = log(Vis*(mt30_hum65/mt24_hum50)/Pop),
    lnVP_2865 = log(Vis*(mt28_hum65/mt24_hum50)/Pop),
    lnVP_2665 = log(Vis*(mt26_hum65/mt24_hum50)/Pop),
    lnVP_2465 = log(Vis*(mt24_hum65/mt24_hum50)/Pop),
    
    lnVP_3480 = log(Vis*(mt34_hum80/mt24_hum50)/Pop),
    lnVP_3280 = log(Vis*(mt32_hum80/mt24_hum50)/Pop),
    lnVP_3080 = log(Vis*(mt30_hum80/mt24_hum50)/Pop),
    lnVP_2880 = log(Vis*(mt28_hum80/mt24_hum50)/Pop),
    lnVP_2680 = log(Vis*(mt26_hum80/mt24_hum50)/Pop),
    lnVP_2480 = log(Vis*(mt24_hum80/mt24_hum50)/Pop)
  ) %>% 
  select(ID, ugs, residence, Year, contains("lnVP_"),Dist, TC, Chl, Eld) %>% 
  filter(lnVP_3450!=-Inf)

ugs_list <- unique(park$ugs)
sheet_list <- list()  # Excelのシート用リスト
all_models <- list()

# TCMの結果出力 boostrap
set.seed(123)
B <- 10000  # ブートストラップ回数

for (k in ugs_list) {
  models <- list()
  
  for (i in c(24, 26, 28, 30, 32, 34)) {
    for (j in c(50, 65, 80)) {
      a <- paste0("lnVP_", i, j)
      formula_str <- paste(a, "~ TC")
      
      df <- RatioVis %>% filter(ugs == k)
      
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
          
          # p値計算（ε補正あり）
          p_values <- apply(boot_coefs, 2, function(x) {
            p_upper <- mean(x >= 0, na.rm = TRUE)
            p_lower <- mean(x <= 0, na.rm = TRUE)
            2 * min(p_upper, p_lower + .Machine$double.eps)
          })
          
          # ★ 有意フラグと有意記号を追加
          sig_flag <- p_values < 0.05
          stars <- cut(p_values,
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
    sheet_list[[k]] <- model_table
  }
  all_models[[k]] <- models   # 公園名をキーに登録
}



coeff_df <- do.call(rbind, lapply(names(all_models), function(ugs_name) {
  model_list <- all_models[[ugs_name]]
  
  do.call(rbind, lapply(names(model_list), function(model_name) {
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
  }))
}))

# cs (USD; 150JPY = 1 USD)
cs_result <-　coeff_df %>% 
  mutate(
    cs = (-1/TC)/150
  )

df_cs <- cs_result %>% select(ugs, model, cs) %>% 
  pivot_wider(names_from = model, values_from =  cs)

# current cs and value
n_vis <- KLA_raw %>% mutate(
  ugs = case_when(UGS =="(a)Takino"~"Takino",
                  TRUE ~ UGS)
) %>% 
  group_by(ugs,Year) %>% 
  summarise(Vis = sum(Vis)) %>% 
  group_by(ugs) %>% 
  summarise(vis = mean(Vis)
  )

df_value <- df_cs %>% 
  left_join(n_vis,
            by=c("ugs")) %>% 
  mutate(
    across(
      starts_with("lnVP_"),
      ~ .x * vis,
      .names = "value_{.col}"
    )
  ) %>% 
  select(ugs, starts_with("value_"))

df_cs_long <- df_cs %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "cs") %>%
  mutate(
    model = sub("^lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )

df_value_long <- df_value %>%
  pivot_longer(cols = -ugs, names_to = "model", values_to = "value") %>%
  mutate(
    model = sub("^value_lnVP_", "", model),
    temp = as.integer(str_extract(model, "^[0-9]+")),
    hum = as.integer(str_extract(model, "[0-9]+$"))
  )

df_combined <- df_value_long %>%
  left_join(df_cs_long, by = c("ugs", "temp", "hum")) %>%
  group_by(temp, hum) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    mean_cs = mean(cs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )

# figure 3
fig3 <- ggplot(df_combined, aes(x = temp, y = hum, fill = total_value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0("USD           \n", comma(round(total_value, 0))), color = text_color), size = 4) +
  scale_color_identity() +
  scale_fill_distiller(palette = "OrRd", direction = -1) +
  scale_x_continuous(breaks = c(24, 26, 28, 30, 32, 34)) + 
  scale_y_continuous(breaks = c(50, 65, 80)) + 
  labs(
    x = "Temperature (℃)", y = "Humidity (%)", fill = "Value (USD)"
  ) +
  theme_minimal(base_family = "Hiragino Maru Gothic Pro") +
  theme(plot.title = element_text(size = 16, face = "bold"), 
        axis.title.x  = element_text(size = 16),
        axis.title.y  = element_text(size = 16),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

# 並べて表示
ggsave("fig3.png", plot = fig3, width = 9, height = 7, dpi = 500)


# cs differenceを計算する
diff_cs <- df_cs %>%
  mutate(across(
    .cols = -ugs,  # ugs列以外すべて
    .fns = ~ .x - lnVP_24_50
  ))

diff_long <- diff_cs %>% mutate(
  ugs = case_when(
    ugs == "Moere"~"Moerenuma",
    ugs == "Makoma"~"Makomanai",
    ugs == "SapporoArt"~"Sapporo Art",
    ugs == "Makoma"~"Makomanai",
    TRUE~ugs
  )
) %>% 
  select(-lnVP_24_50) %>%
  pivot_longer(-ugs, names_to = "model", values_to = "cs_diff") %>% 
  mutate(model = sub("^lnVP_", "", model))  %>%
  separate(model, into = c("temp", "hum"), sep = "_", convert = TRUE) %>% 
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  ) 

fig4 <- ggplot(diff_long, aes(x = temp, y = hum, fill = cs_diff)) +
  geom_tile(color = "white") +
  # scale_color_identity() +
  # geom_text(aes(label = round(cs_diff, 2), color = text_color), size = 2) +
  scale_x_continuous(breaks = c(24, 26, 28, 30, 32, 34)) + 
  scale_y_continuous(breaks = c(50, 65, 80)) + 
  facet_wrap(~ ugs) +
  # scale_fill_distiller(palette = "PuBuGn", direction = -1) + 
  scale_fill_viridis_c(option = "D") +
  labs(
    x = "Temperature (℃)", y = "Humidity (%)", fill = "CS change \n (USD)"
  ) +
  theme_minimal(base_family = "Hiragino Maru Gothic Pro") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 12), 
        axis.title.x  = element_text(size = 12),
        axis.title.y  = element_text(size = 12),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8)
  )
ggsave("fig4.png", plot = fig4, width = 8, height = 6, dpi = 500)



# value difference を計算する
diff_value <- df_value %>% mutate(
  ugs = case_when(
    ugs == "Moere"~"Moerenuma",
    ugs == "Makoma"~"Makomanai",
    ugs == "SapporoArt"~"Sapporo Art",
    ugs == "Makoma"~"Makomanai",
    TRUE~ugs
  ) 
) %>% 
  mutate(across(
    .cols = -ugs,  # ugs列以外すべて
    .fns = ~ .x - value_lnVP_24_50
  ))

diff_v_long <- diff_value %>%
  select(-value_lnVP_24_50) %>%
  pivot_longer(-ugs, names_to = "model", values_to = "value_diff") %>% 
  mutate(model = sub("^value_lnVP_", "", model))  %>%
  separate(model, into = c("temp", "hum"), sep = "_", convert = TRUE) %>% 
  mutate(
    text_color = if_else(temp > 30 & hum >= 65, "white", "black")
  )


key_ugs <- c("Nakajima", "Maruyama","Odori")
# A. 指定3公園のみのデータ
df_key <- diff_v_long %>% filter(ugs %in% key_ugs)　%>%
  mutate(ugs = factor(ugs, levels = c("Odori", "Maruyama", "Nakajima")))
# B. それ以外の公園
df_other <- diff_v_long %>% filter(!ugs %in% key_ugs)

# A. 上段（Odori, Nakajima, Maruyama）
plot_key <- ggplot(df_key, aes(x = temp, y = hum, fill = value_diff)) +
  geom_tile(color = "white") +
  # geom_text(aes(label = round(value_diff, ), color = text_color), size = 2) +
  scale_x_continuous(breaks = c(24, 26, 28, 30, 32, 34)) +
  scale_y_continuous(breaks = c(50, 65, 80)) +
  facet_wrap(~ ugs) +
  scale_fill_viridis_c(
    option = "C",    # 明るい色から
    limits = c(-3000000, 0),
    oob = scales::squish,
    labels = scales::label_comma()
  ) +
  labs(
    title = "a) Changes in recreational value among high-impact parks",
    x = "Temperature (℃)", y = "Humidity (%)", fill = "Value change\n(USD)"
  ) +
  theme_minimal(base_family = "Hiragino Maru Gothic Pro") +
  theme(
    plot.title   = element_text(size = 14, face = "bold"),
    strip.text   = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x  = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 8),
    legend.position = "right"
  )

# B. 下段（Other parks）
plot_other <- ggplot(df_other, aes(x = temp, y = hum, fill = value_diff)) +
  geom_tile(color = "white") +
  # geom_text(aes(label = round(value_diff, ), color = text_color), size = 2) +
  scale_x_continuous(breaks = c(24, 26, 28, 30, 32, 34)) +
  scale_y_continuous(breaks = c(50, 65, 80)) +
  facet_wrap(~ ugs, ncol = 5) +
  scale_fill_viridis_c(
    option = "B",
    labels = scales::label_comma()
  )+
  labs(
    title = "b) Changes in recreational value among other parks",
    x = "Temperature (℃)", y = "Humidity (%)", fill = "Value change\n(USD)"
  ) +
  theme_minimal(base_family = "Hiragino Maru Gothic Pro") +
  theme(
    plot.title   = element_text(size = 14, face = "bold"),
    strip.text   = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x  = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 8),
    legend.position = "right"
  )

final_plot <- (plot_key / plot_other) + 
  plot_layout(heights = c(1, 3))
final_plot

# 表示 or 保存
ggsave("fig5.png", plot = final_plot, width = 12, height = 10, dpi = 500)



# mean, sd を計算
df_value %>% select(value_lnVP_24_50) %>% 
  summarise(
    mean = mean(value_lnVP_24_50),
    sd = sd(value_lnVP_24_50),
    sum = sum(value_lnVP_24_50)
  )

df_cs %>% 
  summarise(
    mean = mean(lnVP_24_50),
    sd = sd(lnVP_24_50),
    sum = sum(lnVP_24_50)
  )

# 差額のvalue, mean, sd を計算
diff_value %>% select(value_lnVP_34_80) %>% 
  summarise(
    mean = mean(value_lnVP_34_80),
    sd = sd(value_lnVP_34_80),
    sum = sum(value_lnVP_34_80)
  )

diff_value %>% select(value_lnVP_30_50) %>% 
  summarise(
    mean = mean(value_lnVP_30_50),
    sd = sd(value_lnVP_30_50),
    sum = sum(value_lnVP_30_50)
  )

