### This is the raw file producing the entire results used in the papers
library(tidyverse)
library(metaumbrella)
collapsunique <- function(x) paste(unique(sort(x)), collapse = ", ")

source("1d - cct_level_psychosocial_scoring_2.R")
source("1a - cct_level psychosocial_scoring_140424.R")
source("1b - cct_level_cam_scoring_140424.R")
source("1c - cct_level_pharma_scoring_140424.R")
source("2 - cct_level_homogeneization_140424.R")
source("3 - ma_level_scoring140424.R")

chemin = "D:/drive_gmail/Recherche/Article 2 - Base de Donnees/7 - Data analysis/data/"

dcct = readxl::read_excel(paste0(chemin,
                                 "cct_level_homogeneized_scored.xlsx")) %>%
  filter(!intervention_type %in% c("Psychosocial-2", "Pharmacological"))
dma = readxl::read_excel(paste0(chemin,
                                "ma_level_homogeneized_scored.xlsx")) %>%
  filter(!intervention_type %in% c("Psychosocial-2", "Pharmacological"))

dcct$factor = with(dcct, paste0(paper, "_",
                                intervention_general, "_",
                                outcome_general))

dma$Factor = dma$factor = with(dma, paste0(paper, "_", 
                              intervention_general, "_", 
                              outcome_general))
nrow1 = nrow(dcct)
dma$available_control_agg = dma$available_control
dcct = left_join(dcct, dma[, c("factor", "PICO_amstar_ID", 
                               "Age_precise", "available_control_agg")])
nrow2 = nrow(dcct)
if (nrow1 != nrow2) stop("problem left_join")


###################
### MOVE THIS #####
###################
dcct$indirectness = with(dcct,
                        ifelse(dcct$Age_precise == "Mixed", "serious",
                               ifelse(dcct$available_control_agg < 0.75 | is.na(dcct$available_control_agg), "serious", "no")))

dcct$rob1_report = gsub(" risk", "", dcct$RoB_Reporting)
dat_error = view.errors.umbrella(dcct)
ok_mult = c("Wolstencroft (2018)", "Salazar de Pablo (2023)", "Fraguas (2019)",
  "Iffland (2023)")
View(dcct[dat_error[grepl("Study with same factor, author and year",
                     dat_error$column_errors, fixed = TRUE) &
                 dat_error$paper %in% ok_mult, "row_index"], c("factor", "author")])
dcct[dat_error[grepl("Study with same factor, author and year",
                          dat_error$column_errors, fixed = TRUE) &
                      dat_error$paper %in% ok_mult, "row_index"], ]$multiple_es <- "outcomes"
dat_error = view.errors.umbrella(dcct)
dcct_rr = dcct %>%
  group_by(factor) %>%
  filter(all(measure %in% c("OR", "RR")))

dcct_smd = dcct %>%
  group_by(factor) %>%
  filter(!all(measure %in% c("OR", "RR")))

if (
  !(all(dcct$factor %in% append(dcct_rr$factor, dcct_smd$factor)) &
    !(any(dcct_rr$factor %in% dcct_smd$factor) | any(dcct_smd$factor %in% dcct_rr$factor)))
) { stop("problem merging SMD and RR files")}

res_list_rr = umbrella(dcct_rr,
                       max_asymmetry = 50,
                       verbose = FALSE,
                       mult.level = TRUE, method.var = "PM", 
                       r = 0.8, pre_post_cor = 0.5)

res_list_smd = umbrella(dcct_smd,
                        verbose = FALSE,
                        mult.level = TRUE, method.var = "REML", 
                        r = 0.8, pre_post_cor = 0.5)

union_smd_rr = union.umbrella(res_list_rr, res_list_smd) 

res_GRADE = add.evidence(union_smd_rr,
                         criteria = "GRADE")

dat_metaumb = data.frame(
  indirectness_metaumbrella = as.character(sapply(res_GRADE, function(x) x$indirectness)),
  rob_metaumbrella = as.numeric(as.character(sapply(res_GRADE, function(x) x$overall_rob))),
  report_metaumbrella = as.numeric(as.character(sapply(res_GRADE, function(x) x$report_rob))),
  Factor = as.character(sapply(res_GRADE, function(x) x$factor))
)

res_smd_rr = summary(res_GRADE) %>%
  left_join(dat_metaumb)

# rio::export(summary(res_list_smd), 
#             paste0("D:/drive_gmail/Recherche/metaumbrella/",
#                    "data-raw/test-raw/res-ebi-asd-SMD.xlsx"), overwrite=TRUE)
# rio::export(summary(res_list_rr), 
#             paste0("D:/drive_gmail/Recherche/metaumbrella/",
#                    "data-raw/test-raw/res-ebi-asd-OR.xlsx"), overwrite=TRUE)
# rio::export(dcct_smd, paste0("D:/drive_gmail/Recherche/metaumbrella/",
#                              "data-raw/test-raw/dat-ebi-asd-SMD.xlsx"), overwrite=TRUE)
# rio::export(dcct_rr, paste0("D:/drive_gmail/Recherche/metaumbrella/",
#                             "data-raw/test-raw/dat-ebi-asd-OR.xlsx"), overwrite=TRUE)


#res_rr = summary(res_list_smd)
res_m_cred = res_smd_rr #bind_rows(res_list_smd, res_list_rr) #
res_m_cred$Outcome = t(do.call(cbind, stringr::str_split(res_m_cred$Factor, "_")))[, 3]
res_m_cred$Intervention = t(do.call(cbind, stringr::str_split(res_m_cred$Factor, "_")))[, 2]
res_m_cred$"Meta-review" = t(do.call(cbind, stringr::str_split(res_m_cred$Factor, "_")))[, 1]

res_m = left_join(res_m_cred, dma, by="Factor") 
if (nrow(res_m_cred) != nrow(res_m)) stop("problem left_join2")

res_m$rob_num = res_m$low_RoB_num
res_m$report_rob_num = res_m$reporting_num
res_m$indirectness_num = with(res_m,
      ifelse(res_m$Age_precise == "Mixed", "serious",
             ifelse(res_m$available_control < 0.75 | is.na(res_m$available_control), 
                    "serious", "no")))
measure = res_m$measure
I2 = res_m$I2

indirectness = res_m$indirectness_num
report_rob = res_m$report_rob_num
rob = res_m$rob_num
  
indirectness = res_m$indirectness_metaumbrella
report_rob = res_m$report_metaumbrella
rob = res_m$rob_metaumbrella
  
View(with(res_m, cbind(
  res_m$Factor, res_m$indirectness_num, res_m$report_rob_num, res_m$rob_num,
  res_m$indirectness_metaumbrella, res_m$report_metaumbrella, res_m$rob_metaumbrella)))

# GRADE
conf.inter_g <- str_extract_all(res_m$eG_CI, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$ci_lo_g <- as.numeric(as.character(conf.inter_g[,1]))
res_m$ci_up_g <- as.numeric(as.character(conf.inter_g[,2]))

conf.inter_or <- str_extract_all(res_m$eOR_CI, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$ci_lo_or <- as.numeric(as.character(conf.inter_or[,1]))
res_m$ci_up_or <- as.numeric(as.character(conf.inter_or[,2]))

pred.inter_g <- str_extract_all(res_m$PI_eG, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$PI_lo_g <- as.numeric(as.character(pred.inter_g[,1]))
res_m$PI_up_g <- as.numeric(as.character(pred.inter_g[,2]))

pred.inter_or <- str_extract_all(res_m$PI_eOR, "-?\\d+\\.\\d+", simplify=TRUE)
res_m$PI_lo_or <- as.numeric(as.character(pred.inter_or[,1]))
res_m$PI_up_or <- as.numeric(as.character(pred.inter_or[,2]))

res_m$CI_lo = CI_lo = with(res_m, 
                           ifelse(measure == "G", ci_lo_g, ci_lo_or))
res_m$CI_up = CI_up = with(res_m, 
                           ifelse(measure == "G", ci_up_g, ci_up_or))

res_m$PI_lo = PI_lo = with(res_m, 
                           ifelse(measure == "G", PI_lo_g, PI_lo_or))
res_m$PI_up = PI_up = with(res_m, 
                           ifelse(measure == "G", PI_up_g, PI_up_or))

if(!all(res_m$total_n.y == res_m$total_n.y)) stop("problem res_m$total_n")
res_m$total_n = res_m$total_n.x
n_cases = res_m$n_cases
n_controls = res_m$total_n.x - res_m$n_cases
## HETEROGENEITY
meas_G = res_m$measure == "G"
meas_OR = res_m$measure %in% c("OR", "RR")

## INDIRECTNESS

# ----------------------------- #
# ------- 1. indirectness ----- #
# ----------------------------- #
down_ind = ifelse(
  is.na(indirectness) | 
    indirectness == "very serious", 2,
      ifelse(indirectness == "serious", 1, 
             ifelse(indirectness == "no", 0, 999)))

if (any(down_ind == 999 | indirectness== 2)) {
  stop("problem scoring ind")
}
# ------------------------- #
# ---------- 2. RoB ---------- #
# ------------------------- #
down_rob = ifelse(is.na(rob), 2,
                  ifelse(rob >= 75, 0,
                         ifelse(rob >= 50, 1, 2)))

# ----------------------------- #
# ---- 5. publication bias ---- #
# ----------------------------- #
egger_p_na = as.numeric(as.character(res_m$egger_p))

esb_p_na = as.numeric(as.character(res_m$ESB_p))

report_na = as.numeric(as.character(report_rob))

down_pubbias = ifelse(
  (is.na(egger_p_na) & is.na(esb_p_na) & is.na(report_na)), NA,
  ifelse(
    (!is.na(egger_p_na) & egger_p_na <= 0.10) |
      (!is.na(esb_p_na) & esb_p_na <= 0.10) |
      (!is.na(report_na) & report_na < 50), 
    1, 0)
)

# ----------------------------------- #
# -------- 3. Heterogeneity ---------- #
# ----------------------------------- #

down_het = NA

eq_range_or = c(0.8, 1.25)
eq_range_g = c(-0.1, 0.1)


low_ci_neg_range =
  ((meas_G  & CI_lo < 0)  & CI_lo >= eq_range_g[1]) |
  ((meas_OR & CI_lo < 1) & CI_lo >= eq_range_or[1])

low_pi_neg_range =
  (meas_G  & PI_lo < 0 & PI_lo >= eq_range_g[1]) |
  (meas_OR & PI_lo < 1 & PI_lo >= eq_range_or[1])

up_ci_pos_range =
  (meas_G  & CI_up >= 0 & CI_up <= eq_range_g[2]) |
  (meas_OR & CI_up >= 1 & CI_up <= eq_range_or[2])

up_pi_pos_range =
  (meas_G  & PI_up >= 0 & PI_up <= eq_range_g[2]) |
  (meas_OR & PI_up >= 1 & PI_up <= eq_range_or[2])

low_ci_pos = (meas_G  & CI_lo >= 0) |
  (meas_OR & CI_lo >= 1)

low_pi_pos = (meas_G  & PI_lo >= 0) |
  (meas_OR & PI_lo >= 1)

up_ci_neg = (meas_G  & CI_up < 0) |
  (meas_OR & CI_up < 1)

up_pi_neg = (meas_G  & PI_up < 0) |
  (meas_OR & PI_up < 1)


low_ci_out_range = (meas_G  & CI_lo < eq_range_g[1]) |
  (meas_OR & CI_lo < eq_range_or[1])

low_pi_out_range = (meas_G  & PI_lo < eq_range_g[1]) |
  (meas_OR & PI_lo < eq_range_or[1])

up_ci_out_range = (meas_G  & CI_up > eq_range_g[2]) |
  (meas_OR & CI_up > eq_range_or[2])

up_pi_out_range = (meas_G  & PI_up > eq_range_g[2]) |
  (meas_OR & PI_up > eq_range_or[2])

# Condition 1 - Row 8 CINeMA
down_het[((low_ci_neg_range & low_pi_out_range) &
            (up_ci_pos_range  & up_pi_out_range))] <- 2

# Condition 3 - Row 4 CINeMA (a)
down_het[is.na(down_het) &
           ((low_ci_pos & low_pi_out_range) |
              (up_ci_neg  & up_pi_out_range))] <- 2

# Condition 2 - Row 6 CINeMA
down_het[is.na(down_het) &
           ((low_ci_neg_range & low_pi_out_range) |
              (up_ci_pos_range  & up_pi_out_range))] <- 1


# Condition 4 - Row 4 CINeMA (b)
down_het[is.na(down_het) &
           ((low_ci_pos & low_pi_neg_range) |
              (up_ci_neg  & up_pi_pos_range))] <- 1

# Condition 5 - Row 1 CINeMA
down_het[is.na(down_het) &
           ((low_ci_pos & low_pi_pos) |
              (up_ci_neg  & up_pi_neg))] <- 0

# Condition 6 - Row 2 & 7 CINeMA
down_het[is.na(down_het) &
           ((low_ci_neg_range & low_pi_neg_range) |
              (up_ci_pos_range  & up_pi_pos_range))] <- 0

# Condition 7 - 5
down_het[is.na(down_het) &
           ((low_ci_out_range & low_pi_out_range) |
              (up_ci_out_range  & up_pi_out_range))] <- 0


I2 = as.numeric(as.character(I2))
dat_imp = data.frame(factor=rep(NA, nrow(res_smd_rr)),
                     perc_contradict=rep(NA, nrow(res_smd_rr)),
                     tau2=rep(NA, nrow(res_smd_rr)),
                     I2=rep(NA, nrow(res_smd_rr)))
i = 0
for (fact in unique(res_smd_rr$Factor)) {
  i = i+1
  # print(i)
  dat_imp$Factor[i] = fact
  dat_sub = NA
  dat_sub = union_smd_rr[[fact]]$x
  poolval = union_smd_rr[[fact]]$ma_results$value
  if (union_smd_rr[[fact]]$measure %in% c("SMD", "MD", "G")) {
    ci_lo = dat_sub$ci_lo
    ci_up = dat_sub$ci_up
  } else {
    ci_lo = log(dat_sub$ci_lo)
    ci_up = log(dat_sub$ci_up)
    ci_lo = ifelse(is.na(ci_lo), -Inf, ci_lo)
    ci_up = ifelse(is.na(ci_up), Inf, ci_up)
  }
  
  dat_imp$perc_contradict[i] = ifelse(poolval >= 0, 
                                      sum(ci_up < 0) / nrow(dat_sub),
                                      sum(ci_lo >= 0) / nrow(dat_sub))
  
  dat_imp$I2[i] = as.numeric(as.character(union_smd_rr[[fact]]$heterogeneity$i2))
  dat_imp$tau2[i] = union_smd_rr[[fact]]$heterogeneity$tau2
}

down_hetB = ifelse(
  is.na(dat_imp$I2), 
  ifelse(dat_imp$perc_contradict >= 0.10, 2, 0),
  ifelse(
    dat_imp$I2 >= 50 & dat_imp$perc_contradict >= 0.10, 2,
    ifelse(dat_imp$I2 >= 30 & dat_imp$perc_contradict >= 0.10, 1, 0)
  )
)
res_m$I2 = dat_imp$I2
res_m$perc_contradict = dat_imp$perc_contradict

down_hetA = down_het
down_het = ifelse(is.na(down_hetA), down_hetB, down_hetA)
# ------------------------- #
# ------ 4. Imprecision ------ #
# ------------------------- #
cross_low_high =
  (meas_G  & CI_lo <= 0    & CI_up >= 0.8) |
  (meas_G  & CI_lo <= -0.8 & CI_up >= 0) |
  (meas_OR & CI_lo <= 1    & CI_up >= 5) |
  (meas_OR & CI_lo <= 0.2  & CI_up >= 1)

n_fail_detect_small_effects =
  n_cases < 394 | n_controls < 394

n_fail_detect_large_effects =
  n_cases < 64 | n_controls < 64
n_studies = res_m$n_studies
down_imp =
  ifelse(cross_low_high & n_fail_detect_small_effects, 2,
         ifelse(n_fail_detect_large_effects, 2,
                ifelse(cross_low_high | n_fail_detect_small_effects, 1, 0)))

res_m$n_controls = n_controls
res_m$down_rob = down_rob
res_m$down_het = down_het
res_m$down_pubbias = down_pubbias
res_m$down_ind = down_ind
res_m$down_imp = down_imp

res_m$down_hetA = down_hetA
res_m$down_hetB = down_hetB

res_m$GRADE_numraw = rowSums(res_m[, 
                                c("down_rob", "down_het", "down_ind", 
                                  "down_imp", "down_pubbias")]) 

res_m$GRADE_num = ifelse(res_m$n_studies >= 5, 
                         res_m$GRADE_numraw, res_m$GRADE_numraw + 1)

res_m$GRADE_w = with(res_m, ifelse(
  GRADE_num == 0, "High", 
  ifelse(GRADE_num %in% 1, "Moderate", 
         ifelse(GRADE_num %in% 2, "Weak", 
                ifelse(GRADE_num >= 3, "Very weak", NA)))))
res_m$GRADE = as.character(res_m$Class)
res_m$GRADE[res_m$GRADE=="Weak"] <- "Low"
res_m$GRADE[res_m$GRADE=="Very weak"] <- "Very low"


if (!all(as.character(res_m$Class) == as.character(res_m$GRADE_w))) {
  stop("PROBLEM GRADE !!!")
}
View(cbind(res_m$Factor, res_m$IN_meta, 
           GRADE_meta = as.character(res_m$Class), 
           GRADE_manual = as.character(res_m$GRADE_w),
           as.character(res_m$Class) == as.character(res_m$GRADE_w), 
           res_m$indirectness_num, res_m$report_rob_num, res_m$rob_num,
           res_m$indirectness_metaumbrella, res_m$report_metaumbrella, res_m$rob_metaumbrella))

# res_union[["De Crescenzo (2020)_PUFA_Adverse events"]]
# View(res_m %>%
#        filter(factor == "De Crescenzo (2020)_PUFA_Adverse events") %>%
#        # filter(IN_meta == 1) %>%
#        arrange(GRADE_num) %>%
#        select(paper, n_studies, n_cases,n_controls, 
#               intervention_general, outcome_general, 
#               GRADE, GRADE_num, measure, value, value_CI, PI_eG, PI_eOR,
#               rob, report_rob, reporting_num,
#               down_rob, 
#               down_het, down_pubbias, down_ind, 
#               down_imp, rob, measure, perc_contradict))

rio::export(res_m, 
            paste0("D:/drive_gmail/Recherche/metaumbrella/",
                   "data-raw/test-raw/res-ebi-asd-GRADE.xlsx"), overwrite=TRUE)

# rio::export(cbind(res_m [, c(
#   "paper", "Intervention", "Outcome", "Outcome_raters", "Outcome_followup",
#   "GRADE", "GRADE_num", "n_studies.x", "down_rob", "rob", "measure", "perc_contradict")], 
#   
#   PI_lo, CI_lo, CI_up, PI_up, 
#   cross_low_high,
#   n_cases, n_controls,
#   n_fail_detect_small_effects,
#   n_fail_detect_large_effects,
#   down_imp,
  # heterogeneity -----------
  # low_ci_neg_range, low_pi_neg_range, up_ci_pos_range, up_pi_pos_range,
  # low_ci_pos, low_pi_pos, up_ci_neg, up_pi_neg,
  # low_ci_out_range, low_pi_out_range, up_ci_out_range, up_pi_out_range,
  # down_hetA, down_hetB, down_het,
  # I2),
  # "datasets/GRADE_double_check.xlsx",
  # overwrite=TRUE)

ASD_symptoms = c("Overall ASD symptoms" , "Social-communication", 
                 "Restricted/repetitive behaviors", "Sensory Profile")
functioning = c("Global cognition (IQ)", "Specific cognition (nvIQ)",
                "Adaptive behaviors", "Quality of life", 
                "Language (Expressive skills)", "Language (Overall skills)", 
                "Language (Receptive skills)") 
safety = c("Acceptability", "Tolerability", "Adverse events")
comorbidities = c("ADHD symptoms", "Anxiety", "Mood related symptoms")
ASD_related = c("Disruptive behaviors")
sleep = c("Sleep quality", "Sleep quantity")
# !!!!!!!!!!!!!!!!!!!
if (!all(unique(res_m$Outcome) %in% c(
  ASD_symptoms,sleep, functioning,safety,
  comorbidities, ASD_related))) {
  print(
    unique(res_m$Outcome)[which(!unique(res_m$Outcome) %in% c(ASD_symptoms,functioning,safety, comorbidities, ASD_related))]
  )
  stop("problem outcomes cat")
}
res_m$sig = ifelse(as.numeric(as.character(res_m$p_value)) < 0.05, TRUE, FALSE)

res_m = res_m %>%
  mutate(
    age_pre = case_when(
      Age == "< 6 yo" ~ "Pre-school (<6 years old)",
      Age == "6-12 yo" ~ "School-age (6-12 years old)",
      Age == "13-19 yo" ~ "Adolescents (13-19 years old)",
      Age == ">= 20 yo" ~ "Adults (>=20 years old)"),
    age_short = case_when(
      Age == "< 6 yo" ~ "Pre-school (<6yo)",
      Age == "6-12 yo" ~ "School-age (6-12yo)",
      Age == "13-19 yo" ~ "Adolescents (13-19yo)",
      Age == ">= 20 yo" ~ "Adults (>=20yo)"),
    age_vshort = case_when(
      Age == "< 6 yo" ~ "Pre-school",
      Age == "6-12 yo" ~ "School-age",
      Age == "13-19 yo" ~ "Adolescents",
      Age == ">= 20 yo" ~ "Adults"),
    
    Outcome_group = case_when(
      Outcome %in% ASD_symptoms ~ "Core ASD symptoms",
      Outcome %in% safety ~ "Safety",
      Outcome %in% functioning ~ "Functioning",
      Outcome %in% comorbidities ~ "Psych. comorbidity",
      Outcome %in% sleep ~ "Sleep",
      Outcome %in% ASD_related ~ "ASD-related symptoms"
    ),
    
    intervention_group = case_when(
      intervention_general %in% c("V1a-RA", 
                                  "MEMANT", "FOLI") ~ "Pharmacological",
      intervention_general %in% c("EIBI", "NDBI", 
                                  "DEV", "SSG", 
                                  "CBT", "TECH", 
                                  "PMI", "TEACCH") ~ "Psychosocial",
      intervention_general %in% c("MUSIC", "SENS", 
                                  "PHYS", "rTMS", 
                                  "tDCS", "ACUP", "DIET", "HERB", "L-CARNIT",
                                  "L-CARNO", "MELAT", "NAC", "OXYT", "PROB", 
                                  "PUFA", "SECRET", "SULFO", 
                                  "VIT-D", "AAI") ~ "Complementary"
    ),
    intervention_spell = case_when(
      
      ## PHARMA 
      intervention_general == "V1a-RA" ~ "Vasopressin antagonist",
      intervention_general == "MEMANT" ~ "Memantine",
      intervention_general == "FOLI" ~ "Folinic acid",
      
      ## PSYCHO
      intervention_general == "EIBI" ~ "Early Intensive Behavioral",
      intervention_general == "NDBI" ~ "Naturalistic int.",
      intervention_general == "DEV" ~ "Developmental int.",
      intervention_general == "SSG" ~ "Social skills groups",
      intervention_general == "CBT" ~ "Cognitive-Behav. Therapy",
      intervention_general == "TECH" ~ "Technology-based",
      intervention_general == "PMI" ~ "Parent-mediated",
      intervention_general == "TEACCH" ~ "TEACCH",
      
      ## COMPLEMENTARY
      intervention_general == "MUSIC" ~ "Music therapy",
      intervention_general == "SENS" ~ "Sensory Integration",
      intervention_general == "PHYS" ~ "Physical activity",
      intervention_general == "rTMS" ~ "rTMS",
      intervention_general == "tDCS" ~ "tDCS",
      intervention_general == "ACUP" ~ "Acupuncture",
      intervention_general == "DIET" ~ "Specific diet",
      intervention_general == "HERB" ~ "Herbal medicine",
      intervention_general == "L-CARNIT" ~ "L-Carnitine",
      intervention_general == "L-CARNO" ~ "L-Carnosine",
      intervention_general == "MELAT" ~ "Melatonin",
      intervention_general == "NAC" ~ "N-acetylcysteine",
      intervention_general == "OXYT" ~ "Oxytocin",
      intervention_general == "PROB" ~ "Probiotics",
      intervention_general == "PUFA" ~ "Fatty acids",
      intervention_general == "SECRET" ~ "Secretin",
      intervention_general == "SULFO" ~ "Sulforaphane",
      intervention_general == "VIT-D" ~ "Vitamin-D",
      intervention_general == "AAI" ~ "Animal-assisted"
    ),
    col_sig = case_when(
      eG <= -0.8 ~ "#9e6b4c",
      eG <= -0.5 &  eG > -0.8 ~ "#ec240a",
      eG <= -0.2 &  eG > -0.5 ~ "#df958c",
      eG < 0.2 &  eG > -0.2 ~ "#D4D4D4",
      eG >= 0.2 &  eG < 0.5 ~ "#a6d2a7",
      eG >= 0.5 &  eG < 0.8 ~ "#02d70e",
      eG >= 0.8 ~ "#19d2ff"),
    effect_text = case_when(
      eG <= -0.8 & sig ~ paste0("a very large, statistically significant, and harmful effect"),
      eG <= -0.8 & !sig ~ paste0("no effect"),
      (eG <= -0.5 &  eG > -0.8) & sig ~ paste0("a moderate-to-large, statistically significant, harmful effect"),
      (eG <= -0.5 &  eG > -0.8) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG <= -0.2 &  eG > -0.5) & sig ~ paste0("a small-to-moderate, statistically significant, harmful effect"),
      (eG <= -0.2 &  eG > -0.5) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG < 0.2 &  eG > -0.2) & sig ~ paste0("a negligeable but statistically significant, harmful effect"),
      (eG < 0.2 &  eG > -0.2) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG >= 0.2 &  eG < 0.5) & sig ~ paste0("a small-to-moderate, statistically significant, clinically beneficial effect"),
      (eG >= 0.2 &  eG < 0.5) & !sig ~ paste0("no effect (not statistically significant)"),
      (eG >= 0.5 &  eG < 0.8) & sig  ~ paste0("a moderate-to-large, statistically significant, clinically beneficial effect"),
      (eG >= 0.5 &  eG < 0.8) & !sig ~ paste0("no effect (not statistically significant)"),
      eG >= 0.8 & sig  ~ paste0("a very large, statistically significant, clinically beneficial effect"),
      eG >= 0.8 & !sig  ~ paste0("no effect (not statistically significant)")),
    col_grade = case_when(
      GRADE == "Very low" ~ "#000000",
      GRADE == "Low" ~ "#ffa064",
      GRADE == "Moderate" ~ "#cf2061",
      GRADE == "High" ~ "#3d0173"),           
    GRADE_rank = case_when(
      GRADE == "Very low" ~ 1,
      GRADE == "Low" ~ 1.5,
      GRADE == "Moderate" ~ 3,
      GRADE == "High" ~ 4
    ),
    col_contour = case_when(
      GRADE == "Very low" ~ "transparent",
      GRADE != "Very low" ~ "#000"
    )
    
  )

# n_participants = dcct %>%
#   ungroup() %>%
#   group_by(intervention_type, trial_id) %>%
#   filter(n_tot == max(n_tot)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   group_by(intervention_type) %>%
#   summarise(n_intervention_type = unique(paste0("n-", intervention_type, 
#                                                 " = ", sum(n_tot))),
#             n_studies_manual = length(unique(trial_id)))



res_m$outcome_short = res_m$outcome_general
res_m$outcome_short[res_m$outcome_short == "Restricted/repetitive behaviors"] <- "Rest./repet. behaviors"
res_m$outcome_short[res_m$outcome_short == "Language (Expressive skills)"] <- "Language (Expressive)"
res_m$outcome_short[res_m$outcome_short == "Language (Receptive skills)"] <- "Language (Receptive)"
res_m$outcome_short[res_m$outcome_short == "Language (Overall skills)"] <- "Language (Overall)"
res_m$outcome_short[res_m$outcome_short == "Specific cognition (nvIQ)"] <- "Specific cognition"


# !!!!!!!!!!!!!!!!!!!
# if (length(unique(res_m$intervention_general)) != 
#     length(unique(na.omit(res_m$intervention_spell)))) {
#   print(unique(res_m$intervention_general[is.na(res_m$intervention_spell)]))
#   stop("problem identifying interventions")
# }

rio::export(res_m, paste0(chemin, "UR_TOTAL_analysis.xlsx"), overwrite = TRUE)
rio::export(res_m %>% filter(intervention_type == "Complementary"), 
            paste0("D:/drive_gmail/Recherche/Article 2 - Base de Donnees/7 - Data analysis/supplementary - CAM/", "UR_CAM_analysis.txt"))
rio::export(res_m, 
            "D:/drive_gmail/Recherche/app_ebi/ebi_apps/automated_gen/ebiact_database/UR_TOTAL_analysis.xlsx", 
            overwrite = TRUE)

zioup_dataset = function(y) {
  DAT = data.frame(y["x"])
  names(DAT) = gsub("x.", "", names(DAT))
  DAT[, "Factor"] <- y["factor"]$factor
  DAT[, "factor"] <- y["factor"]$factor
  DAT[, "measure"] <- y["measure"]
  return(DAT)
}
dat_rct = dplyr::bind_rows(do.call(dplyr::bind_rows, 
                                   lapply(res_list_smd, zioup_dataset)),
                           do.call(dplyr::bind_rows, 
                                   lapply(res_list_rr, zioup_dataset)))
View(dcct %>% 
       select(intervention_type, intervention_comment, intervention_label, 
              duration_month, dose, dose_unit, setting, factor) %>%
       distinct())
dat_export_rct = left_join(
  dcct %>% 
    select(link_cct, intervention_type, author, year, intervention_comment, intervention_label, 
           duration_month, dose, dose_unit, setting, factor) %>%
    distinct(),
  dat_rct %>% 
    select(author, year, measure, value, ci_lo, ci_up, n_cases, n_controls, factor, Factor)
); nrow(dat_export_rct)


all(dat_export_rct$Factor %in% res_m$Factor)
dat_export_rct$Factor[which(!dat_export_rct$Factor %in% res_m$Factor)]

rio::export(dat_export_rct, 
            "D:/drive_gmail/Recherche/app_ebi/ebi_apps/automated_gen/ebiact_database/RCT_TOTAL_analysis.xlsx", 
            overwrite = TRUE)




