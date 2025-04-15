datumb = read.delim("full_replication_mateirals/cct_level_CAM.txt")


dcct = readxl::read_excel(paste0(chemin,
                                 "cct_level_homogeneized_scored.xlsx")) %>%
  filter(!intervention_type %in% c("Psychosocial-2", "Pharmacological"))
dma = readxl::read_excel(paste0(chemin,
                                "ma_level_homogeneized_scored.xlsx")) %>%
  filter(!intervention_type %in% c("Psychosocial-2", "Pharmacological"))

if (restrict_low_bias) {
  dcct = dcct %>%
    
} else {
  
}
