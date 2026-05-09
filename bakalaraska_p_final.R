
library(dplyr) # pro práci s tabulkami, fce mutate(), group_by(), summarise(),
# sample_n(), filter(), .groups = "drop", case_when() nebo %>% (řetězí za sebe příkazy)
library(tidyr) # transformace tcaru tabulek, plot_wider() apod.
library(ggplot2) # funguje na principu vrstev, které přidávám pomocí +,
# fce jako geom_density, geom_ribbon nebo geom_line
library(MASS) # glm.nb(), obecný lineární model pro negativně binomické rozdělení
library(car) # pro hodnocení regresních modelů, vif(), Anova()
library(corrplot) # korelační matice

citation()
citation("dplyr")
citation("tidyr")
citation("ggplot2")
citation("MASS")
citation(("car"))
citation("corrplot")

# středník jako oddělovač, čárka jako desetinná značka- csv2
data_vysypky <- read.csv2("C:/Users/honza/Downloads/data_bakalarka.csv", sep = ";", header = TRUE)

# přejmenování roku
data_vysypky <- data_vysypky %>%
  rename(year = X.a.nah)

data_vysypky <- data_vysypky %>%
  mutate(
    # pro jistotu
    area = as.numeric(area),
    
    # přeřazení do kategorií, byly tam nějaké překlepy
    area_category = case_when(
      area <= 20 ~ "<20",
      area <= 100 ~ "<100",
      area <= 500 ~ "<500",
      area <= 5000 ~ "<5000",
      area > 5000 ~ ">5000",
      TRUE ~ NA  # pokud u nějaké tůně rozloha chybí, dá tam NA
    ),
    
    # aby Rko neřadilo kategorie abecedně, ale logicky
    area_category = factor(area_category, 
                           levels = c("<20", "<100", "<500", "<5000", ">5000"))
  )

# summary pro základní představu o datech
summary(data_vysypky)

#########################################################################################
# příprava trénovacího datasetu
data_training_roky <- data_vysypky %>%
  filter(year <= 2020) %>%     # do r. 2020
  filter(drought != "n") %>%   # některá jezírka měla "n" jako nenalezeno apod., vyhodíme je
  mutate(across(c(depth_max, depth_prev, area, n), as.numeric))  # kvantitativní proměnné numeric

# pravděpodobnost vysychání- jelikož průměruji údaje jen pomocí zvodnělých jezírek, potřebuji
# nějak zachytit fakt, že některá jezírka často vysychají
drought_stats <- data_training_roky %>%
  group_by(pond) %>%
  summarise(
    drought_prob = mean(drought == "d", na.rm = TRUE),   # "d" bere R jako 1, "f" jako 0, dddfd -> 0,8
    years_total = n(),
    .groups = "drop"    # jistota, data mi nezůstanou v těchto skupinách, asi zbytečné
  ) %>%
  filter(years_total > 2) # ponechá jen jezírka s aspoň 3 validními roky

# výpočet průměrů pro kvantitativní proměnné, abych měl každé jezírko jen jednou a co nejlépe popsané
traits_numeric <- data_training_roky %>%
  filter(pond %in% drought_stats$pond & drought == "f") %>%    #aspoň 3 záznamy a jen zvodnělé r.
  filter(area > 0) %>%   # vyhodí řádky s area == 0
  group_by(pond) %>%
  summarise(
    n_mean = mean(n, na.rm = TRUE),           # pokud někde bude chybět údaj, R počítá průměr ze zbylých
    area_mean_log = mean(log10(area), na.rm = TRUE),
    depth_max_mean = mean(depth_max, na.rm = TRUE),
    depth_prev_mean = mean(depth_prev, na.rm = TRUE),
    .groups = "drop"
  )

# funkce pro výpočet modusu, při shodě upřednostňuji nejnovější záznam
modus_cas <- function(data, sloupec_nazev) {
  data %>%
    filter(pond %in% drought_stats$pond & drought == "f") %>%
    # opět pro výpočet ignoruju políčka, kde je NA nebo ""
    filter(!is.na(.data[[sloupec_nazev]]) & .data[[sloupec_nazev]] != "") %>%
    
    # vytvořím skupiny podle pond a proměnné
    group_by(pond, .data[[sloupec_nazev]]) %>%
    summarise(
      pocet_vyskytu = n(), 
      posledni_rok = max(year), 
      .groups = "drop"
    ) %>%
    
    # seřadím podle pond, počtu výskytů a posledního roku (při shodě vyhraje poslední rok)
    arrange(pond, desc(pocet_vyskytu), desc(posledni_rok)) %>%
    
    # vybereme modus
    group_by(pond) %>%
    slice(1) %>%
    
    # finální tabulka, jen pond a název sloupce
    dplyr::select(pond, .data[[sloupec_nazev]]) %>%
    ungroup()
}

# Vypočtení modusů a spojení do jednoho datasetu
traits_stats <- traits_numeric %>%
  left_join(modus_cas(data_training_roky, "LOC"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "veg"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "slope"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "sun"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "qual"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "fish"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "threat"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "surr"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "rec.t"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "rec.b"), by = "pond") %>%
  left_join(modus_cas(data_training_roky, "area_category"), by = "pond") %>%
  rename(rec_t = `rec.t`, rec_b = `rec.b`)   # aplikace funkce a přejmenování rekultivací

# finální training dataset
data_training <- inner_join(drought_stats, traits_stats, by = "pond")

#################
data_training <- inner_join(drought_stats, traits_stats, by = "pond") %>%
  mutate(
    strat_area_LOC = paste(LOC, area_category, sep = "_") # Připraveno pro pozdější křížovou stratifikaci
  )

data_training <- data_training %>%
  mutate(
    drought_type = ifelse(drought_prob == 0, "stabilní", "nestabilní"),
    strat_area_drought = paste(area_category, drought_type, sep = "_")
  ) %>%
  # Pro jistotu odstraníme kategorii >5000 nestabilní, kdyby v datech dělala prázdné faktory
  filter(strat_area_drought != ">5000_nestabilní")
##################

print(head(data_training))

# testovací dataset 2021-2025
data_testing <- data_vysypky %>%
  filter(year > 2020) %>%
  mutate(drought = ifelse(pond == 100 & drought == "n", "f", drought)) %>%
  filter(drought != "n")  # u jezírka 100 šlo nutně o přepis, jelikož byly zaznamenány ostatní údaje

# základní statistiky pro trénovací data
prumer_train <- mean(data_training$n_mean, na.rm = TRUE)
rozptyl_train <- var(data_training$n_mean, na.rm = TRUE)
median_train <- median(data_training$n_mean, na.rm = TRUE)
index_disperze <- round(rozptyl_train / prumer_train, 2)  # měří se u negativně binomického rozdělení

# histogram průměrného počtu snůšek
ggplot(data_training %>% filter(n_mean > 0), aes(x = n_mean)) +                   
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white", alpha = 0.8) +  
  geom_vline(aes(xintercept = median_train), color = "darkgreen", linetype = "dashed", size = 0.5) +
  annotate("text", x = median_train + 1, y = 30, label = paste("Medián:", round(median_train, 1)), color = "darkgreen", fontface = "bold", angle = 90, vjust = -0.8) +
  geom_vline(aes(xintercept = prumer_train), color = "firebrick", linetype = "solid", size = 0.5) +
  annotate("text", x = prumer_train + 1, y = 65, label = paste("Průměr:", round(prumer_train, 1)), color = "firebrick", fontface = "bold", angle = 270, vjust = 0.1) +
  labs(title = "Distribuce průměrného počtu snůšek jezírek v tréninkovém datasetu (do roku 2020)", subtitle = "Zobrazeny pouze lokality s výskytem (n > 0).", x = "Průměrný počet snůšek na jezírko (n)", y = "Počet jezírek") +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, max(data_training$n_mean, na.rm = TRUE), by = 5))


# základní boxplot pro lokality

counts_data_loc <- data_training %>% 
  filter(!is.na(LOC)) %>% 
  group_by(LOC) %>% 
  summarise(pocet = n())

# 2. Vykreslení grafu
ggplot(data_training %>% filter(!is.na(LOC)), aes(x = LOC, y = n_mean, fill = LOC)) +
  # Boxplot
  geom_boxplot(alpha = 0.7, outlier.color = "firebrick", outlier.size = 2, outlier.shape = 16) +
  # Přidání textu s počty jezírek k hornímu okraji (y = 10)
  geom_text(data = counts_data_loc, aes(x = LOC, y = 10, label = paste0("N = ", pocet)), 
            inherit.aes = FALSE, color = "black", fontface = "bold", size = 4.5) +
  labs(
    title = "Rozložení průměrného počtu snůšek podle lokalit", 
    subtitle = "Dataset: data_training, N = počet jezírek",
    x = "Lokalita (LOC)", 
    y = "Průměrný počet snůšek (n_mean)"
  ) +
  theme_classic() +
  # Očištěné téma bez tučného písma v nadpisech os
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2") +
  coord_cartesian(ylim = c(0, 10))

m2_labels <- c(10, 100, 1000, 10000, 100000) # Reálné hodnoty v m² pro popisky
log_breaks <- c(1,2,3,4,5)

ggplot(data_training, aes(x = area_mean_log, y = n_mean)) +
  # Svislé čáry (přidáváme je pod body, aby nerušily - proto jsou v kódu dříve)
  geom_vline(xintercept = log10(c(20, 100, 500, 5000)), 
             linetype = "dashed", 
             color = "gray60", 
             alpha = 0.8) +
  geom_point(alpha = 0.5, color = "steelblue", size = 2) +
  geom_smooth(method = "loess", color = "firebrick", se = TRUE, fill = "gray80") +
  scale_x_continuous(
    breaks = log_breaks,
    labels = paste0(log_breaks, "\n(", m2_labels, " m²)")
  ) +
  labs(
    title = "Závislost počtu snůšek na rozloze tůně",
    subtitle = "Dataset: data_training | Svislé čáry značí hranice kategorií (20, 100, 500, 5000 m²)",
    x = "Log10(rozloha tůně) — v závorce skutečná rozloha v m²",
    y = "Průměrný počet snůšek (n_mean)"
  ) +
  theme_classic() +
  coord_cartesian(ylim = c(0, 15))

# boxplot podle rozlohových kategorií s počty
counts_data <- data_training %>% filter(!is.na(area_category)) %>% group_by(area_category) %>% summarise(pocet = n())
ggplot(data_training %>% filter(!is.na(area_category)), aes(x = area_category, y = n_mean, fill = area_category)) +
  geom_boxplot(alpha = 0.7, outlier.color = "firebrick", outlier.size = 2, outlier.shape = 16) +
  geom_text(data = counts_data, aes(x = area_category, y = 25, label = paste0("N = ", pocet)), inherit.aes = FALSE, color = "black", fontface = "bold", size = 4.5) +
  labs(title = "Distribuce průměrného počtu snůšek podle rozlohy tůně", subtitle = "Dataset: data_training | Přiblíženo do 25 snůšek", x = "Kategorie rozlohy (m²)", y = "Průměrný počet snůšek (n_mean)") +
  theme_classic() + theme(legend.position = "none") +
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(ylim = c(0, 25))


sum(data_training$n_mean == 0)
sum(round(data_training$n_mean) == 0)



#MODEL

# Korelační matice
data_num <- data_training %>%
  dplyr::select(LOC, area_mean_log, area_category, depth_prev_mean,depth_max_mean, veg, fish, drought_prob, sun, qual, surr, slope) %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))

par(mfrow = c(1, 1))

korelacni_matice <- cor(data_num, use = "complete.obs", method = "spearman")
corrplot(korelacni_matice, method = "number", type = "lower", tl.col = "black", tl.cex = 0.8, diag = FALSE)

# Výpočet modelů
model_nb_log <- glm.nb(round(n_mean) ~ area_mean_log + LOC + depth_prev_mean + veg + fish + drought_prob + sun + qual + slope + surr, 
                       data = data_training, control = glm.control(maxit = 1000))

model_nb_cat <- glm.nb(round(n_mean) ~ area_category + depth_prev_mean + LOC + veg + fish + drought_prob + sun + qual + slope + surr, 
                       data = data_training, control = glm.control(maxit = 1000))

finalni_model <- glm.nb(round(n_mean) ~ area_category + LOC + drought_prob + sun + qual + fish,
                        data = data_training, control = glm.control(maxit = 1000))

###############################################možná navíc


par(mfrow = c(2, 2))

data_training_gamma <- data_training %>%
  mutate(n_mean_gamma = ifelse(n_mean == 0, 0.1, n_mean)) # Zkus 0.1 místo 0.001

model_gamma <- glm(n_mean_gamma ~ area_category + LOC + drought_prob + 
                     sun + qual + fish + surr, 
                   family = Gamma(link = "log"), 
                   data = data_training_gamma)

#  Výpis výsledků
summary(model_gamma)


plot(model_gamma)


par(mfrow = c(1, 1))


######################################################################



# VIF kontrola a summary
vif(model_nb_log) # Odkomentuj pokud je potřeba
vif(model_nb_cat)
vif(finalni_model)

summary(model_nb_log)
summary(model_nb_cat)
summary(finalni_model)

# LRT
drop1_test <- drop1(finalni_model, test = "Chi")

# Výpočet hodnot pro grafy
# Použijeme standardizovaná devianční rezidua
res_dev <- rstandard(finalni_model, type = "deviance")

# Predikované hodnoty (na logaritmické škále)
fits <- predict(finalni_model, type = "link")

par(mfrow = c(2, 2))

# graf reziduí
plot(fits, res_dev, 
     xlab = "Predicted values (link scale)", 
     ylab = "Std. Deviance Residuals",
     main = "Std. Deviance Residuals",
     pch = 16, col = rgb(0,0,0,0.3)) # Průhledné body pro lepší přehlednost
abline(h = 0, lty = 2, col = "red")
lines(lowess(fits, res_dev), col = "blue", lwd = 2) # Vyhlazený trend

# GRAF 2: Normal Q-Q 
# kontrola normálního rozdělení reziduí
qqnorm(res_dev, main = "Normal Q-Q (Deviance)", pch = 16, col = rgb(0,0,0,0.3))
qqline(res_dev, col = "red", lwd = 2)

# GRAF 3: Scale-Location
# Kontrolujeme homoskedasticitu (rozptyl chyb by měl být všude podobný)
plot(fits, sqrt(abs(res_dev)), 
     xlab = "Predicted values (link scale)", 
     ylab = "sqrt(|Std. Dev. Res|)",
     main = "Scale-Location",
     pch = 16, col = rgb(0,0,0,0.3))
lines(lowess(fits, sqrt(abs(res_dev))), col = "blue", lwd = 2)

# GRAF 4: Cook's Distance
plot(finalni_model, which = 4, main = "Cook's distance")

par(mfrow = c(1, 1))
res <- residuals(finalni_model, type = "deviance")

hist(res, 
     breaks = 30, 
     col = "steelblue", 
     border = "white",
     main = "Histogram deviančních reziduí",
     xlab = "Devianční rezidua",
     ylab = "Frekvence")

# Přidání linky pro nulu
abline(v = 0, col = "red", lwd = 2, lty = 2)

library(ggplot2)

ggplot(data_training, aes(x = drought_prob, y = n_mean)) +
  # Body (tůně) s průhledností, aby byla vidět hustota u nuly
  geom_point(alpha = 0.3, color = "steelblue", size = 2) +
  # Vyhlazovací křivka ukazující trend
  geom_smooth(method = "loess", color = "firebrick", fill = "gray80", size = 1.2) +
  labs(
    title = "Vztah mezi pravděpodobností vysychání a počtem snůšek",
    subtitle = "Trend vyhlazen metodou LOESS | Osa Y omezena pro lepší čitelnost",
    x = "Pravděpodobnost vyschnutí tůně",
    y = "Průměrný počet snůšek (n_mean)"
  ) +
  theme_classic() +
  # Omezíme osu Y na 15, abychom viděli hlavní trend a ne jen pár extrémů
  coord_cartesian(ylim = c(0, 15))

pocet_nula <- sum(data_training$drought_prob == 0, na.rm = TRUE)


# Výpočet počtů pro jednotlivé kategorie sucha
counts_data_drought <- data_training %>% 
  filter(!is.na(drought_type)) %>% 
  group_by(drought_type) %>% 
  summarise(pocet = n())

# Boxplot
ggplot(data_training %>% filter(!is.na(drought_type)), aes(x = drought_type, y = n_mean, fill = drought_type)) +
  # Boxplot s jasně definovanými odlehlými hodnotami (outliers)
  geom_boxplot(alpha = 0.7, outlier.color = "firebrick", outlier.size = 2, outlier.shape = 16) +
  # Přidání textu s počty jezírek k hornímu okraji (y = 10)
  geom_text(data = counts_data_drought, aes(x = drought_type, y = 15, label = paste0("N = ", pocet)), 
            inherit.aes = FALSE, color = "black", fontface = "bold", size = 4.5) +
  labs(
    title = "Rozložení průměrného počtu snůšek v kategoriích na základě pravděpodobnosti vysychání jezírka", 
    subtitle = "Dataset: data_training, N = počet jezírek", 
    x = "Kategorie tůně dle pravděpodobnosti vysychání", 
    y = "Průměrný počet snůšek (n_mean)"
  ) +
  theme_classic() + 
  theme(legend.position = "none") +
  # Modrá paleta pro sjednocení stylu
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(ylim = c(0, 15))




# Hodnocení modelu (Deviance a Testy)
null_dev <- finalni_model$null.deviance
res_dev <- finalni_model$deviance
df_diff <- finalni_model$df.null - finalni_model$df.residual

G_test <- null_dev - res_dev
p_value_model <- pchisq(G_test, df = df_diff, lower.tail = FALSE)
pseudo_r2 <- (null_dev - res_dev) / null_dev

cat("Nulová deviance: ", round(null_dev, 2), "\nReziduální deviance: ", round(res_dev, 2), "\nZměna deviance (G): ", round(G_test, 2), "\nVysvětlená deviance (Pseudo-R2): ", round(pseudo_r2 * 100, 1), "%\nP-hodnota celého modelu: ", format.pval(p_value_model, eps = .001), "\n\n")

phi_deviance <- finalni_model$deviance / finalni_model$df.residual

# Cookova vzdálenost a identifikace vlivných pozorování
data_training$cooks_d <- cooks.distance(finalni_model)

hranice_vlivnych_p <- 4 / nrow(data_training)
vlivna_id <- data_training %>% filter(cooks_d >= hranice_vlivnych_p) %>% pull(pond)
vlivna_jezirka_tabulka <- data_training %>% filter(cooks_d >= hranice_vlivnych_p)
print(vlivna_jezirka_tabulka)


# Celkový skutečný trend
trend_abs <- data_testing %>%
  filter(year %in% 2021:2025) %>%
  group_by(year) %>%
  summarise(pocet_snusek = sum(n, na.rm = TRUE), .groups = "drop") %>%
  mutate(Metoda = "Skutečná populace")

trend_proc <- trend_abs %>%
  arrange(year) %>%
  mutate(trend_proc = (pocet_snusek - lag(pocet_snusek)) / lag(pocet_snusek) * 100) %>%
  filter(!is.na(trend_proc))

skutecnost_vektor <- trend_proc$trend_proc 

# Funkce pro výběr vzorku (Neymanova alokace s iterativním přerozdělením)
stratifikovany_vyber <- function(data_pool, data_fixni, cilova_velikost, strat_sloupec, naklady = NULL) {
  pocet_k_vyberu <- cilova_velikost - nrow(data_fixni)
  
  # 1. Výpočet vah a prvotní alokace
  alokace <- data_pool %>%
    group_by(!!sym(strat_sloupec)) %>%
    summarise(Nh = n(), Sh = sd(n_mean, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Sh = ifelse(is.na(Sh) | Sh == 0, 0.01, Sh),
      cost = if(is.null(naklady)) 1 else naklady[as.character(!!sym(strat_sloupec))],
      cost = ifelse(is.na(cost), 1, cost),
      weight = (Nh * Sh) / sqrt(cost),
      nh = round(pocet_k_vyberu * (weight / sum(weight, na.rm = TRUE)))
    )
  
  # 2. Iterativní přerozdělování (Řeší problém mizejících vzorků)
  while(any(alokace$nh > alokace$Nh)) {
    pretika <- alokace$nh > alokace$Nh
    propadlo <- sum(alokace$nh[pretika] - alokace$Nh[pretika]) # Kolik vzorků propadlo
    
    # Zastropujeme strata, která narazila na fyzický limit
    alokace$nh[pretika] <- alokace$Nh[pretika]
    
    # Najdeme strata, která ještě mají volnou kapacitu
    muze_prijmout <- alokace$nh < alokace$Nh
    if(!any(muze_prijmout) || propadlo == 0) break
    
    # Rozdělíme propadlý zbytek mezi nenaplněná strata podle jejich váhy
    vahy_zbytkove <- alokace$weight[muze_prijmout]
    alokace$nh[muze_prijmout] <- alokace$nh[muze_prijmout] + round(propadlo * (vahy_zbytkove / sum(vahy_zbytkove)))
  }
  
  # 3. Finální zarovnání zaokrouhlovacích chyb (aby jich bylo přesně např. 250)
  rozdil <- pocet_k_vyberu - sum(alokace$nh)
  if(rozdil != 0) {
    muze_prijmout <- alokace$nh < alokace$Nh
    if(any(muze_prijmout)) {
      # Přidáme/ubereme rozdíl největšímu dostupnému stratu
      idx_korekce <- which(muze_prijmout)[which.max(alokace$nh[muze_prijmout])]
      alokace$nh[idx_korekce] <- alokace$nh[idx_korekce] + rozdil
    }
  }
  
  # 4. Samotný fyzický výběr jezírek
  vzorek_pool <- data_pool %>%
    left_join(alokace %>% dplyr::select(!!sym(strat_sloupec), nh), by = strat_sloupec) %>%
    group_by(!!sym(strat_sloupec)) %>%
    group_split() %>%
    lapply(function(stratum) { dplyr::sample_n(stratum, min(nrow(stratum), stratum$nh[1])) }) %>%
    bind_rows() %>% dplyr::select(-nh)
  
  return(bind_rows(data_fixni, vzorek_pool))
}

# Funkce pro nastavení parametrů metody
# Přidali jsme parametr cenik_area_drought
parametry_metody <- function(metoda, id_obrich, id_vsech, cenik_area, cenik_kombi, cenik_area_drought) { 
  if (metoda == "nahodny_vyber") fixni_id <- c()
  else if (grepl("jen_obri", metoda)) fixni_id <- id_obrich
  else fixni_id <- id_vsech
  
  if (grepl("bez_ceny", metoda)) naklady <- NULL
  else if (grepl("loc", metoda)) naklady <- cenik_kombi
  else if (grepl("drought", metoda)) naklady <- cenik_area_drought # PŘIDÁNO
  else naklady <- cenik_area
  
  # PŘIDÁNA ROZBOČKA PRO SLOUPEC:
  sloupec <- if (grepl("loc", metoda)) "strat_area_LOC" 
  else if (grepl("drought", metoda)) "strat_area_drought" 
  else "area_category"
  
  nazev <- switch(metoda,
                  "nahodny_vyber" = "1. Náhodný výběr",
                  "strat_area_jen_obri" = "2. Area, Cena",
                  "strat_area_jen_obri_bez_ceny" = "3. Area, Bez ceny",
                  "strat_area_obri_i_cook" = "4. Area, Cook, Cena",
                  "strat_area_obri_i_cook_bez_ceny" = "5. Area, Cook, Bez ceny",
                  "strat_area_loc_jen_obri" = "6. Area+LOC, Cena",
                  "strat_area_loc_jen_obri_bez_ceny" = "7. Area+LOC, Bez ceny",
                  "strat_area_loc_obri_i_cook" = "8. Area+LOC, Cook, Cena",
                  "strat_area_loc_obri_i_cook_bez_ceny" = "9. Area+LOC, Cook, Bez ceny",
                  # PŘIDANÉ NOVÉ METODY:
                  "strat_area_drought_jen_obri" = "10. Area+Sucho, Cena",
                  "strat_area_drought_jen_obri_bez_ceny" = "11. Area+Sucho, Bez ceny",
                  "strat_area_drought_obri_i_cook" = "12. Area+Sucho, Cook, Cena",
                  "strat_area_drought_obri_i_cook_bez_ceny" = "13. Area+Sucho, Cook, Bez ceny"
  )
  return(list(fixni_id = fixni_id, naklady = naklady, sloupec = sloupec, nazev = nazev))
}

# Funkce pro jedno fyzické losování
konkretni_vyber <- function(metoda, parametry, velikost, data_train, data_pool, data_fixni, data_test) {
  if (metoda == "nahodny_vyber") vzorek_id <- sample_n(data_train, velikost) %>% dplyr::select(pond)
  else vzorek_id <- stratifikovany_vyber(data_pool, data_fixni, velikost, parametry$sloupec, parametry$naklady) %>% dplyr::select(pond)
  sapply(2021:2025, function(rok) {
    data_test %>% filter(year == rok) %>% dplyr::select(pond, n) %>% right_join(vzorek_id, by = "pond") %>% mutate(n = ifelse(is.na(n), 0, n)) %>% pull(n) %>% sum() 
  })
}

# Funkce pro vyhodnocení Pearsona a MAE
hodnoceni_vyberu <- function(sim_matice, skutecnost) {
  metriky <- apply(sim_matice, 2, function(sloupec_odhadu) {
    yoy_vzorku <- (sloupec_odhadu[-1] - sloupec_odhadu[-5]) / sloupec_odhadu[-5] * 100
    if(any(is.na(yoy_vzorku)) | any(is.infinite(yoy_vzorku))) return(c(Pearson = NA, MAE = NA))
    return(c(Pearson = cor(yoy_vzorku, skutecnost, method = "pearson"), MAE = mean(abs(yoy_vzorku - skutecnost))))
  })
  return(as.data.frame(t(metriky)) %>% na.omit())
}

# Hlavní simulační cyklus
velikosti_k_testu <- c(200, 250, 300)
metody_k_testu <- c(
  "nahodny_vyber", 
  "strat_area_jen_obri", 
  "strat_area_jen_obri_bez_ceny",     
  "strat_area_obri_i_cook", 
  "strat_area_obri_i_cook_bez_ceny", 
  "strat_area_loc_jen_obri", 
  "strat_area_loc_jen_obri_bez_ceny",  
  "strat_area_loc_obri_i_cook", 
  "strat_area_loc_obri_i_cook_bez_ceny",
  "strat_area_drought_jen_obri",
  "strat_area_drought_jen_obri_bez_ceny",
  "strat_area_drought_obri_i_cook",
  "strat_area_drought_obri_i_cook_bez_ceny"
)
vysledky_simulaci <- data.frame()
spolehlivost_simulaci <- data.frame()
data_realita_graf <- data.frame()
data_distribuce_250 <- data.frame()
tabulka_alokaci_wide <- data.frame()

# Ceníky/minuty strávené výzkumem
naklady <- c("<20" = 5.5, "<100" = 6, "<500" = 8, "<5000" = 20, ">5000" = 30)
naklady_kombi <- c("HJV_<20" = 5.5, "KV_<20" = 5.5, "HJV_<100" = 6, "KV_<100" = 6, 
                   "HJV_<500" = 8, "KV_<500" = 8, "HJV_<5000" = 20, "KV_<5000" = 20, "HJV_>5000" = 30, "KV_>5000" = 30)
naklady_area_drought <- c(
  "<20_stabilní" = 5.5, "<20_nestabilní" = 5.5, 
  "<100_stabilní" = 6, "<100_nestabilní" = 6, 
  "<500_stabilní" = 8, "<500_nestabilní" = 8, 
  "<5000_stabilní" = 20, "<5000_nestabilní" = 20, 
  ">5000_stabilní" = 30
)


id_obrich_jezirek <- data_training %>% filter(area_category == ">5000") %>% pull(pond)
vsechna_fixni_id <- unique(c(id_obrich_jezirek, vlivna_id)) 

set.seed(6767)

for (metoda in metody_k_testu) {
  parametry <- parametry_metody(metoda, id_obrich_jezirek, vsechna_fixni_id, naklady, naklady_kombi, naklady_area_drought)
  data_fixni <- data_training %>% filter(pond %in% parametry$fixni_id)
  data_pool <- data_training %>% filter(!(pond %in% parametry$fixni_id))
  
  for (velikost in velikosti_k_testu) {
    cat("Simuluji vzorek velikosti", velikost, "pro metodu", parametry$nazev, "...\n")
    simulace_matice <- replicate(500, konkretni_vyber(metoda, parametry, velikost, data_training, data_pool, data_fixni, data_testing))
    
    vysledky_simulaci <- bind_rows(vysledky_simulaci, data.frame(year = 2021:2025, Odhad = rowMeans(simulace_matice), Velikost = paste("Vzorek", velikost), Metoda = parametry$nazev))
    
    metriky_df <- hodnoceni_vyberu(simulace_matice, skutecnost_vektor)
    spolehlivost_simulaci <- bind_rows(spolehlivost_simulaci, data.frame(
      Metoda = parametry$nazev, Velikost = paste("Vzorek", velikost), Median_r = median(metriky_df$Pearson), Worst_5_percent_r = quantile(metriky_df$Pearson, 0.05),  
      Min_r = min(metriky_df$Pearson), Max_r = max(metriky_df$Pearson), Median_MAE = median(metriky_df$MAE), Worst_5_percent_MAE = quantile(metriky_df$MAE, 0.95) 
    ))
    
    # Zaznamenání dat pro vzorek 250
    # pro graf
    if (velikost == 250) {
      df_temp <- as.data.frame(simulace_matice)
      df_temp$year <- 2021:2025
      df_long <- df_temp %>%
        pivot_longer(cols = starts_with("V"), names_to = "simulace", values_to = "Odhad") %>%
        mutate(Metoda = parametry$nazev)
      data_distribuce_250 <- bind_rows(data_distribuce_250, df_long)
      
      # pro nakladovou tabulku
      if (metoda == "nahodny_vyber") {
        vybrano <- data_training %>%
          sample_n(250) %>%
          count(area_category) %>%
          pivot_wider(names_from = area_category, values_from = n) %>%
          mutate(Metoda = parametry$nazev)
      } else {
        vybrano <- stratifikovany_vyber(data_pool, data_fixni, 250, parametry$sloupec, parametry$naklady) %>%
          count(area_category) %>%
          pivot_wider(names_from = area_category, values_from = n) %>%
          mutate(Metoda = parametry$nazev)
      }
      tabulka_alokaci_wide <- bind_rows(tabulka_alokaci_wide, vybrano)
    }
    # -------------------------------------------------------------------------
    
    if (metoda != "nahodny_vyber" && velikost == 250) {
      yoy_matice <- (simulace_matice[-1, ] - simulace_matice[-5, ]) / simulace_matice[-5, ] * 100
      ukazkovy_vzorek <- stratifikovany_vyber(data_pool, data_fixni, 250, parametry$sloupec, parametry$naklady)
      pocet_navstivenych <- sapply(2022:2025, function(r) nrow(data_testing %>% filter(year == r & pond %in% ukazkovy_vzorek$pond)))
      data_realita_graf <- bind_rows(data_realita_graf, data.frame(Metoda = parametry$nazev, Obdobi = c("2021-2022", "2022-2023", "2023-2024", "2024-2025"), Trend_Vzorku = yoy_matice[, 1], Skutecnost = skutecnost_vektor, MAE_roku = rowMeans(abs(yoy_matice - skutecnost_vektor), na.rm = TRUE), Navstiveno = pocet_navstivenych, Chyba_95_roku = apply(abs(yoy_matice - skutecnost_vektor), 1, quantile, probs = 0.95, na.rm = TRUE)))
    }
  }
}

realny_trend <- data.frame(year = trend_abs$year, Odhad = trend_abs$pocet_snusek, Velikost = "Skutečnost", Metoda = "Skutečná populace")
vysledky_simulaci <- bind_rows(vysledky_simulaci, realny_trend)

# Časová náročnost

# Výpočet celkových minut pro celou výsypku (všechna trénovací data)
celkem_minut <- sum(naklady[as.character(data_training$area_category)], na.rm = TRUE)

# Čas nutný pro vyčerpávající šetření (Celá populace)
pocty_celkem <- data_training %>% 
  count(area_category) %>% 
  pivot_wider(names_from = area_category, values_from = n)

radek_populace <- pocty_celkem %>%
  mutate(
    Metoda = "Vyčerpávající šetření",
    Prumerny_soucet_snusek_ve_vzorku = round(sum(data_training$n_mean, na.rm = TRUE), 1),
    Cas_celkem_minuty = celkem_minut,
    Odhad_hodin = round(celkem_minut / 60, 1)
  )



prumerne_snusky_vzorku <- vysledky_simulaci %>%
  filter(Velikost == "Vzorek 250") %>%
  group_by(Metoda) %>%
  summarise(
    Prumerny_soucet_snusek_ve_vzorku = round(mean(Odhad, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Čas pro vzorky 250
finalni_srovnani <- tabulka_alokaci_wide %>%
  # Připojíme průměrné počty nalezených snůšek ze simulací
  left_join(prumerne_snusky_vzorku %>% ungroup() %>% dplyr::select(Metoda, Prumerny_soucet_snusek_ve_vzorku), by = "Metoda") %>%
  # Vypočítáme celkový čas pro každý řádek vzorku
  mutate(
    Cas_celkem_minuty = (`<20` * naklady["<20"]) + (`<100` * naklady["<100"]) + 
      (`<500` * naklady["<500"]) + (`<5000` * naklady["<5000"]) + 
      (`>5000` * naklady[">5000"]),
    Odhad_hodin = round(Cas_celkem_minuty / 60, 1)
  ) %>%
  # Přidáme na konec řádek s celou populací
  bind_rows(radek_populace) %>%
  # Seřadíme podle hodin (od nejrychlejší metody)
  arrange(Odhad_hodin) %>%
  # PŘESUN SLOUPEČKU METODA NA PRVNÍ MÍSTO
  dplyr::select(Metoda, everything())

print(finalni_srovnani)


# Distribuční funkce pro vzorky 250

if(exists("distribuce_vsechny_roky")) rm(distribuce_vsechny_roky)
distribuce_vsechny_roky <- data.frame()
set.seed(67)

# Odhaduji pomocí simulace velikost populace pro jednotlivé roky
for (metoda in metody_k_testu) {
  
  # Načtení parametrů metod ze sekce výše
  parametry <- parametry_metody(metoda, id_obrich_jezirek, vsechna_fixni_id, naklady, naklady_kombi, naklady_area_drought)
  
  data_fixni <- data_training %>% filter(pond %in% parametry$fixni_id)
  data_pool <- data_training %>% filter(!(pond %in% parametry$fixni_id))
  N_krajina <- nrow(data_training)
  
  cat("Počítám vážené odhady (extrapolaci) pro metodu:", parametry$nazev, "...\n")
  
  vysledky_500_matic <- replicate(500, {
    
    # VÝBĚR VZORKU (250)
    if (metoda == "nahodny_vyber") {
      vzorek <- data_training %>% sample_n(250)
    } else {
      vzorek <- stratifikovany_vyber(data_pool, data_fixni, 250, parametry$sloupec, parametry$naklady)
    }
    
    # VÁŽENÉ ODHADY PRO KAŽDÝ ROK
    odhady_5_let <- sapply(2021:2025, function(rok) {
      data_rok <- data_testing %>% filter(year == rok)
      
      vzorek_n <- vzorek %>%
        left_join(data_rok %>% dplyr::select(pond, n), by = "pond") %>%
        mutate(n = ifelse(is.na(n), 0, n))
      
      # Prostý náhodný výběr (Průměr vzorku * Počet všech jezírek)
      if (metoda == "nahodny_vyber") {
        odhad <- mean(vzorek_n$n) * N_krajina
        
        # Stratifikované metody (Suma fixních + Vážený průměr zbytku)
      } else {
        # Přesný součet fixních jezírek 
        suma_fixni <- sum(vzorek_n$n[vzorek_n$pond %in% data_fixni$pond])
        
        # Tabulka skutečných velikostí strat pro zbytek krajiny (data_pool)
        tabulka_Nh_pool <- data_pool %>% count(!!sym(parametry$sloupec), name = "Nh")
        
        # Vážený odhad pro vylosovaný zbytek
        odhad_zbytku <- vzorek_n %>%
          filter(!(pond %in% data_fixni$pond)) %>%
          group_by(!!sym(parametry$sloupec)) %>%
          summarise(prumer_h = mean(n), .groups = "drop") %>%
          left_join(tabulka_Nh_pool, by = parametry$sloupec) %>%
          # Pojistka: pokud by ve stratu nebyl záznam, nahradí se průměrem zbytku
          mutate(prumer_h = ifelse(is.na(prumer_h), mean(vzorek_n$n[!(vzorek_n$pond %in% data_fixni$pond)]), prumer_h)) %>%
          mutate(odhad_h = prumer_h * Nh) %>%
          pull(odhad_h) %>% sum()
        
        odhad <- suma_fixni + odhad_zbytku
      }
      return(odhad)
    })
    return(odhady_5_let)
  })
  
  # Složení matice do dlouhého formátu
  df_metoda <- data.frame(
    Metoda = parametry$nazev, 
    Rok = rep(2021:2025, times = 500), 
    Odhad = as.vector(vysledky_500_matic)
  )
  distribuce_vsechny_roky <- bind_rows(distribuce_vsechny_roky, df_metoda)
}

# Skutečnost 2021-2025
skutecnost_vsechny_roky <- data_testing %>%
  filter(year %in% 2021:2025 & pond %in% data_training$pond) %>% 
  group_by(year) %>% summarise(Skutecnost = sum(n, na.rm = TRUE), .groups = "drop") %>% rename(Rok = year)

tabulka_intervalu_vsechny <- distribuce_vsechny_roky %>%
  group_by(Rok, Metoda) %>% summarise(Spodni_hranice = quantile(Odhad, 0.025), Horni_hranice = quantile(Odhad, 0.975), .groups = "drop")


# area+LOC
metody_do_grafu <- c(
  "1. Náhodný výběr",
  "6. Area+LOC, Cena",
  "7. Area+LOC, Bez ceny",
  "8. Area+LOC, Cook, Cena",
  "9. Area+LOC, Cook, Bez ceny"
)


for (aktualni_rok in 2021:2025) {
  # Filtrujeme data a intervaly pouze na vybraných 5 metod
  data_rok <- distribuce_vsechny_roky %>% 
    filter(Rok == aktualni_rok & Metoda %in% metody_do_grafu)
  
  intervaly_rok <- tabulka_intervalu_vsechny %>% 
    filter(Rok == aktualni_rok & Metoda %in% metody_do_grafu)
  
  skutecnost_rok <- skutecnost_vsechny_roky %>% filter(Rok == aktualni_rok) %>% pull(Skutecnost)
  
  plot_rok <- ggplot() +
    geom_density(data = data_rok, aes(x = Odhad, fill = Metoda, color = Metoda), alpha = 0.4, linewidth = 0.8) +
    geom_vline(data = intervaly_rok, aes(xintercept = Spodni_hranice, color = Metoda), linetype = "dotted", linewidth = 1) +
    geom_vline(data = intervaly_rok, aes(xintercept = Horni_hranice, color = Metoda), linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = skutecnost_rok, color = "red", linetype = "dashed", linewidth = 1.5) +
    labs(
      title = paste("Srovnání modelů odhadu populace pro ROK", aktualni_rok),
      subtitle = "Stratifikované výběry (Area+LOC) a Náhodný výběr | N=250",
      x = "Odhadovaný absolutní počet snůšek", 
      y = "Hustota pravděpodobnosti", 
      fill = "Použitá metoda:", color = "Použitá metoda:"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      plot.title = element_text(face = "bold", size = 15), 
      legend.text = element_text(size = 10)
    ) +
    # Nastavíme mřížku legendy na 3 sloupce, aby se to hezky srovnalo
    guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
  
  print(plot_rok)
}




#znovu pro area
metody_do_grafu <- c(
  "1. Náhodný výběr",
  "2. Area, Cena",
  "3. Area, Bez ceny",
  "4. Area, Cook, Cena",
  "5. Area, Cook, Bez ceny"
)


for (aktualni_rok in 2021:2025) {
  # Filtrujeme data a intervaly pouze na vybraných 5 metod
  data_rok <- distribuce_vsechny_roky %>% 
    filter(Rok == aktualni_rok & Metoda %in% metody_do_grafu)
  
  intervaly_rok <- tabulka_intervalu_vsechny %>% 
    filter(Rok == aktualni_rok & Metoda %in% metody_do_grafu)
  
  skutecnost_rok <- skutecnost_vsechny_roky %>% filter(Rok == aktualni_rok) %>% pull(Skutecnost)
  
  plot_rok <- ggplot() +
    geom_density(data = data_rok, aes(x = Odhad, fill = Metoda, color = Metoda), alpha = 0.4, linewidth = 0.8) +
    geom_vline(data = intervaly_rok, aes(xintercept = Spodni_hranice, color = Metoda), linetype = "dotted", linewidth = 1) +
    geom_vline(data = intervaly_rok, aes(xintercept = Horni_hranice, color = Metoda), linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = skutecnost_rok, color = "red", linetype = "dashed", linewidth = 1.5) +
    labs(
      title = paste("Srovnání modelů odhadu populace pro ROK", aktualni_rok),
      subtitle = "Filtrováno na křížovou stratifikaci (Area) a Náhodný výběr | N=250",
      x = "Odhadovaný absolutní počet snůšek", 
      y = "Hustota pravděpodobnosti", 
      fill = "Použitá metoda:", color = "Použitá metoda:"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      plot.title = element_text(face = "bold", size = 15), 
      legend.text = element_text(size = 10)
    ) +
    # Nastavíme mřížku legendy na 3 sloupce, aby se to hezky srovnalo
    guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
  
  print(plot_rok)
}



# Znovu pro Area+Sucho
metody_do_grafu_drought <- c(
  "1. Náhodný výběr",
  "10. Area+Sucho, Cena",
  "11. Area+Sucho, Bez ceny",
  "12. Area+Sucho, Cook, Cena",
  "13. Area+Sucho, Cook, Bez ceny"
)

for (aktualni_rok in 2021:2025) {
  # Filtrujeme data a intervaly pouze na vybraných 5 metod
  data_rok <- distribuce_vsechny_roky %>% 
    filter(Rok == aktualni_rok & Metoda %in% metody_do_grafu_drought)
  
  intervaly_rok <- tabulka_intervalu_vsechny %>% 
    filter(Rok == aktualni_rok & Metoda %in% metody_do_grafu_drought)
  
  skutecnost_rok <- skutecnost_vsechny_roky %>% filter(Rok == aktualni_rok) %>% pull(Skutecnost)
  
  plot_rok <- ggplot() +
    geom_density(data = data_rok, aes(x = Odhad, fill = Metoda, color = Metoda), alpha = 0.4, linewidth = 0.8) +
    geom_vline(data = intervaly_rok, aes(xintercept = Spodni_hranice, color = Metoda), linetype = "dotted", linewidth = 1) +
    geom_vline(data = intervaly_rok, aes(xintercept = Horni_hranice, color = Metoda), linetype = "dotted", linewidth = 1) +
    geom_vline(xintercept = skutecnost_rok, color = "red", linetype = "dashed", linewidth = 1.5) +
    labs(
      title = paste("Srovnání modelů odhadu populace pro ROK", aktualni_rok),
      subtitle = "Filtrováno na stratifikaci (Area+Sucho) a Náhodný výběr | N=250",
      x = "Odhadovaný absolutní počet snůšek", 
      y = "Hustota pravděpodobnosti", 
      fill = "Použitá metoda:", color = "Použitá metoda:"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      plot.title = element_text(face = "bold", size = 15), 
      legend.text = element_text(size = 10)
    ) +
    # Nastavíme mřížku legendy na 2 sloupce, aby se to hezky srovnalo
    guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2))
  
  print(plot_rok)
}



# graf trendu se MAE a jejím 95% intervalem

# Nastaveno na Metodu  "12. Area+Sucho, Cook, Cena"
metoda_nazev <- "12. Area+Sucho, Cook, Cena"
metoda_klic <- "strat_area_drought_obri_i_cook"


# Převedeme dlouhou tabulku zpět na matici 5 (roků) x 500 (simulací)
simulace_data <- distribuce_vsechny_roky %>%
  filter(Metoda == metoda_nazev) %>%
  mutate(Simulace_ID = rep(1:500, each = 5)) %>%
  pivot_wider(names_from = Simulace_ID, values_from = Odhad) %>%
  arrange(Rok) %>%
  dplyr::select(-Metoda, -Rok) %>%
  as.matrix()

# Výpočet matice meziročních změn (YoY)
yoy_matice <- (simulace_data[-1, ] - simulace_data[-5, ]) / simulace_data[-5, ] * 100
skutecnost_yoy <- trend_proc$trend_proc

# Zjistíme průměrnou (MAE) a extrémní (95%) absolutní chybu pro každý rok
mae_roku <- rowMeans(abs(yoy_matice - skutecnost_yoy), na.rm = TRUE)
chyba_95_roku <- apply(abs(yoy_matice - skutecnost_yoy), 1, quantile, probs = 0.95, na.rm = TRUE)

# konkrétní výběr
set.seed(67) 

parametry_vyber <- parametry_metody(metoda_klic, id_obrich_jezirek, vsechna_fixni_id, naklady, naklady_kombi, naklady_area_drought)
data_fixni_vyber <- data_training %>% filter(pond %in% parametry_vyber$fixni_id)
data_pool_vyber <- data_training %>% filter(!(pond %in% parametry_vyber$fixni_id))

vzorek_250 <- stratifikovany_vyber(data_pool_vyber, data_fixni_vyber, 250, parametry_vyber$sloupec, parametry_vyber$naklady)

# Výpočet odhadů velikosti populace pro každý rok
odhady_vzorku <- sapply(2021:2025, function(rok) {
  data_rok <- data_testing %>% filter(year == rok)
  vzorek_n <- vzorek_250 %>% left_join(data_rok %>% dplyr::select(pond, n), by = "pond") %>% mutate(n = ifelse(is.na(n), 0, n))
  
  suma_fixni <- sum(vzorek_n$n[vzorek_n$pond %in% data_fixni_vyber$pond])
  tabulka_Nh_pool <- data_pool_vyber %>% count(!!sym(parametry_vyber$sloupec), name = "Nh")
  
  odhad_zbytku <- vzorek_n %>%
    filter(!(pond %in% data_fixni_vyber$pond)) %>%
    group_by(!!sym(parametry_vyber$sloupec)) %>%
    summarise(prumer_h = mean(n), .groups = "drop") %>%
    left_join(tabulka_Nh_pool, by = parametry_vyber$sloupec) %>%
    mutate(prumer_h = ifelse(is.na(prumer_h), mean(vzorek_n$n[!(vzorek_n$pond %in% data_fixni_vyber$pond)]), prumer_h)) %>%
    mutate(odhad_h = prumer_h * Nh) %>%
    pull(odhad_h) %>% sum()
  
  return(suma_fixni + odhad_zbytku)
})

# Meziroční trend konkrétního výběru
yoy_vzorku <- (odhady_vzorku[-1] - odhady_vzorku[-5]) / odhady_vzorku[-5] * 100

# Data pro graf
data_graf_mae <- data.frame(
  Obdobi = c("2021-2022", "2022-2023", "2023-2024", "2024-2025"),
  Skutecnost = skutecnost_yoy,
  Vzorek_YoY = yoy_vzorku,
  MAE = mae_roku,
  Chyba95 = chyba_95_roku
) %>%
  mutate(
    # pásy chyb
    Pás_MAE_dolní = Vzorek_YoY - MAE,
    Pás_MAE_horní = Vzorek_YoY + MAE,
    Pás_95_dolní = Vzorek_YoY - Chyba95,
    Pás_95_horní = Vzorek_YoY + Chyba95
  )

plot_realita_mae <- ggplot(data_graf_mae, aes(x = Obdobi, group = 1)) +
  geom_ribbon(aes(ymin = Pás_95_dolní, ymax = Pás_95_horní), fill = "steelblue", alpha = 0.2) +
  geom_ribbon(aes(ymin = Pás_MAE_dolní, ymax = Pás_MAE_horní), fill = "steelblue", alpha = 0.5) +
  
  # Skutečnost
  geom_line(aes(y = Skutecnost), color = "firebrick", linewidth = 1.5, linetype = "dashed") +
  geom_point(aes(y = Skutecnost), color = "firebrick", size = 3) +
  
  # Konkrétní výběr
  geom_line(aes(y = Vzorek_YoY), color = "black", linewidth = 1.2) +
  geom_point(aes(y = Vzorek_YoY), color = "black", size = 3) +
  
  # Legenda
  labs(
    title = "Meziroční trend: Skutečnost vs. Konkrétní vítězný výběr",
    subtitle = paste("Metoda:", metoda_nazev, "| Vzorek N = 250\nModré pásy představují průměrnou chybu (MAE) a 95% interval chyby odhadu."),
    x = "Období",
    y = "Meziroční změna počtu snůšek [%]",
    caption = "Červená = Skutečnost | Černá = Konkrétní výběr v terénu s intervalem spolehlivosti"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "darkgrey", alpha = 0.7) +
  scale_y_continuous(breaks = seq(-1000, 1000, by = 50))

print(plot_realita_mae)

