
# -------------------------------------------------------------------------

# check for Q10 temp effect (positive, negative, etc.)
loadd(srdb_q10)
srdb_v5_equations = read.csv("data/SRDB_V5_1827/data/srdb-equations-V5.csv")


temp_effect = 
  srdb_v5_equations %>% 
  dplyr::select(Temp_effect, Q10_5_15, Q10_0_20, Q10_0_10) %>% 
  pivot_longer(-Temp_effect)

temp_effect %>% 
  ggplot(aes(x = name, y = value, color = Temp_effect, group = Temp_effect))+
  geom_point(position = position_dodge(width = 0.5))
