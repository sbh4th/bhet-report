library(here)
library(tidyverse)
library(osfr)
library(patchwork)

supp <- osf_retrieve_node("h25tz")
supp %>%
  osf_ls_files(path = "Heating-device-use/code-data",
               pattern = "csv") %>%
  osf_download(path = here("data-clean"),
               conflicts = "overwrite")

Expenditure_House <- read_csv(here("data-clean", 
  "Expenditure_House.csv"))

Heat_raw <- read_csv(here("data-clean", 
  "Room_Heat.csv"))

Expenditure <- Expenditure_House %>%
  mutate(time_to_treat = ifelse(T_Timing != 0, year - T_Timing, 0),
         Treat_i = ifelse(T_Timing != 0, 1, 0)) %>%
  mutate(year_treated = ifelse(Treat_i == 0, 10000, T_Timing)) %>%
  mutate(rel_year = ifelse(Treat_i == 0, Inf, year - T_Timing))

#~~ Heating -------------------
Heat <- Heat_raw %>%
  dplyr::select(hh_id: total_room) 

Heat_master <- Expenditure %>%
  merge(., Heat, by = c("hh_id", "wave"), all.x = T) %>%
  mutate(heated_area_share = house_area_heated/house_area,
         ave_dur_total = heat_dur_master/total_room,
         ave_dur_heatable = heat_dur_master/heatable_room,
         ave_dur_heatuse = heat_dur_master/heat_regular)

Heat_merge <- Heat_master %>%
  dplyr::select(hh_id, wave, heatable_room:ave_dur_heatuse)

HEI_Master <- Expenditure %>%
  merge(., Heat_merge, by = c("hh_id", "wave"), all.x = T) 

#~~ Plot of typical heating pattern from room-heat data -------------
Room_Number <- HEI_Master %>%
  dplyr::select(hh_id, wave,matches("room_number*"), total_room) %>%
  dplyr::select(-"room_number_heatuse") %>%
  pivot_longer(
    cols = starts_with("room_number"), # Select columns that start with 'room_number'
    names_to = "room",        # Name of the new variable column
    values_to = "room_number",         # Name of the new value column
    names_prefix = "room_number"       # Remove the 'room_number' prefix from the variable names
  ) %>%
  filter(is.na(room_number) == F)

Heat_Use <- Heat %>%
  dplyr::select(hh_id, wave, heat_regular)

Heating_Pattern <- HEI_Master %>%
  dplyr::select(hh_id, wave, year, ID_COUNTY, Treat_V, T_Timing, Treat_i,
                cent_hotwater_fuel, Quantity_Briquettes, Quantity_coal,
                matches("room_heat*_*")) %>%
  pivot_longer(
    cols = starts_with("room_heat"),
    names_to = c(".value", "room", "number"), 
    names_pattern = "(room_heat|room_heatdur)(\\d+)_(\\d+)") %>%
  mutate(id = paste(room, number, sep = "_")) %>%
  dplyr::select(-c(number)) %>%
  filter(is.na(room_heat) == F) %>%
  filter(room_heat != 1) %>%
  mutate(room_heat_dur = ifelse(room_heatdur == 1, 0, NA),
         room_heat_dur = ifelse(room_heatdur == 2, 1, room_heat_dur),
         room_heat_dur = ifelse(room_heatdur == 3, 3, room_heat_dur),
         room_heat_dur = ifelse(room_heatdur == 4, 5, room_heat_dur),
         room_heat_dur = ifelse(room_heatdur == 5, 7, room_heat_dur),
         room_heat_dur = ifelse(room_heatdur == 6, 9, room_heat_dur),
         room_heat_dur = ifelse(room_heatdur == 7, 11, room_heat_dur),
         room_heat_dur = ifelse(room_heatdur == 8, 14, room_heat_dur),
         room_heat_dur = ifelse(room_heatdur == 9, 20, room_heat_dur)) %>%
  merge(., Room_Number, by = c("hh_id", "wave", "room")) %>%
  merge(., Heat_Use, by = c("hh_id", "wave")) %>%
  mutate(heardur = room_heat_dur * room_number) %>%
  group_by(hh_id, wave, year, room_heat, Treat_V, T_Timing, Treat_i,cent_hotwater_fuel, Quantity_Briquettes, Quantity_coal) %>%
  summarise(total_heatdur = sum(room_heat_dur, na.rm = T),
            total_room = mean(total_room, na.rm = T),
            heat_regular = mean(heat_regular, na.rm = T),
            ave_heatdur = total_heatdur/heat_regular) %>%
  ungroup() %>%
  mutate(room_heat = case_when(hh_id == "20180824" & room_heat == 3~ 13,
                               hh_id == "20180505" & room_heat == 3~ 7,
                               hh_id == "20180973" & room_heat == 3~ 13,
                               hh_id == "20190223" & wave == 2 & room_heat == 3~ 13,
                               hh_id == "20190247" & wave == 2 & room_heat == 3~ 4,
                               
                               TRUE ~ room_heat)) %>% #elec-wire floor and floor radiator
  mutate(heat_labal= case_when(room_heat %in% c("2", "3") & str_detect(cent_hotwater_fuel,"1") ~ "Radiator-coal",
                               room_heat %in% c("2", "3") & is.na(cent_hotwater_fuel) &
                                 (is.na(Quantity_Briquettes) == F |is.na(Quantity_coal) == F  )~ "Radiator-coal",
                               room_heat %in% c("2", "3") & str_detect(cent_hotwater_fuel,"2") & !str_detect(cent_hotwater_fuel,"1") ~ "Radiator-HP",
                               room_heat %in% c("2", "3") & cent_hotwater_fuel == "6"~ "Radiator-EBoiler",
                               room_heat == "4" ~ "Kang-Wood",
                               room_heat == "5" ~ "Kang-Coal",
                               room_heat == "6" ~ "Stove(direct)-Coal",
                               room_heat == "7" ~ "Stove(direct)-Wood",
                               room_heat == "8" ~ "ATA",
                               room_heat == "9" ~ "Storage",
                               room_heat == "10" ~ "ELE-Mobile",
                               room_heat == "11" ~ "ELE-Blanket",
                               room_heat == "12" ~ "ELE-AC",
                               room_heat == "13" ~ "ELE-Floor",
                               room_heat == "90" ~ "Other", 
                               TRUE ~ NA)) %>% 
  filter(total_heatdur != 0) %>%
  filter(is.na(heat_labal) == F) %>% 
  group_by(year, Treat_V, Treat_i, T_Timing, heat_labal) %>%
  summarise(total_heatdur = sum(total_heatdur)) 

sample_size <- HEI_Master %>%
  group_by(year,Treat_i, T_Timing) %>%
  summarise(n = n())

HEI <- Heating_Pattern %>%
  merge(., sample_size, by = c("Treat_i", "year", "T_Timing")) %>%        
  mutate(ave_dur = total_heatdur/n)  %>%
  mutate(heat_fuel = case_when(heat_labal %in% 
   c("Radiator-coal", "Kang-Coal", "Stove(direct)-Coal" ) ~ "Coal",
   heat_labal %in% c("Kang-Wood","Stove(direct)-Wood") ~ "Biomass",
   heat_labal %in% c("Radiator-HP","ATA", "Storage", "ELE-Mobile",
     "ELE-Blanket", "ELE-AC", "ELE-Floor") ~ "ELEC",
                               TRUE ~ "Other"))

HEI$Treat_i <- factor(HEI$Treat_i, 
                      levels = c(0,1),
                      labels = c("Untreat", "Treat"))

HEI$T_Timing <- factor(HEI$T_Timing, 
                       levels = c(0,2019, 2020, 2021),
                       labels = c("Never", "2019", "2020", "2021"))

sample_size_treat <- HEI_Master %>%
  group_by(Treat_i, Treat_V) %>%
  summarise(n = n())

HEI_treat <- Heating_Pattern %>%
  merge(., sample_size_treat, by = c("Treat_i", "Treat_V")) %>%        
  mutate(ave_dur = total_heatdur/n)  %>%
  mutate(heat_fuel = case_when(heat_labal %in% c("Radiator-coal", "Kang-Coal", "Stove(direct)-Coal" ) ~ "Coal",
                               heat_labal %in% c("Kang-Wood","Stove(direct)-Wood") ~ "Biomass",
                               heat_labal %in% c("Radiator-HP","ATA", "Storage", "ELE-Mobile",
                                                 "ELE-Blanket", "ELE-AC", "ELE-Floor") ~ "ELEC",
                               TRUE ~ "Other"))


HEI_treat$Treat_i <- factor(HEI_treat$Treat_i, 
                            levels = c(0,1),
                            labels = c("Never", "Treat"))

HEI_treat$Treat_V <- factor(HEI_treat$Treat_V, 
                            levels = c(0,1),
                            labels = c("Before", "After"))


HEI_treat_Fuel <- HEI_treat %>%
  group_by(Treat_i, Treat_V, heat_fuel) %>%
  summarise(ave_dur = sum(ave_dur)) 

HEI_treat_Fuel$heat_fuel <- factor(HEI_treat_Fuel$heat_fuel,
                                   levels = c("Other","ELEC","Biomass","Coal"))

fuel_quantity <- Expenditure %>%
  dplyr::select(hh_id, wave, year, Treat_V, Treat_i, T_Timing, 
                Quantity_Briquettes, Quantity_coal, 
                Quantity_Honeycomb_coal, Quantity_Wood,
                Quantity_Straw, Quantity_LPG) %>%
  rowwise()%>%
  mutate(Coal = sum(Quantity_Briquettes, Quantity_coal, na.rm = T) * 1000,
         Biomass = sum(Quantity_Wood,Quantity_Straw, na.rm = T)) %>%
  group_by(year, T_Timing) %>%
  summarise(n = n(),
            Coal = sum(Coal)/n,
            Biomass = sum(Biomass)/n) %>%
  dplyr::select(-n) %>%
  pivot_longer(cols = c("Coal", "Biomass"))

ggplot(fuel_quantity, aes(x = year, y = value, 
                          fill = factor(name)))+
  geom_bar(stat = "identity", position = "stack", width = 0.5)+
  theme_bw()+ facet_grid(~ T_Timing, switch = "x") +
  #theme(strip.placement = "outside",
  #      strip.background = element_rect(fill = NA, color = "white"),
  #      panel.spacing = unit(-.01,"cm")) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 12,  color="black"),
        axis.text.y = element_text(size = 14,  color="black")) +
  labs(fill = "Fuel", y = "Solid Fuel Consumption(kg)")+
  theme(legend.background = element_rect(fill = 'white', colour = 'black'))+
  guides(color = guide_legend(ncol = 3), shape = guide_legend(ncol = 3)) +
  scale_y_continuous(expand = expansion(mult = c(0, .05))) +
  theme(strip.text = element_text(size = 18, face = "bold")) +
  scale_fill_manual(values = c("#72b043","#7d6b57"))

# set theme
stheme <- theme_classic() + theme(plot.title = element_text(size = 18, face = "bold"), plot.subtitle = element_text(size=16)) + theme(axis.text.x = element_text(size = 16, colour = "grey60"), axis.title.y=element_text(size=16, angle=90, colour="grey60"), axis.text.y = element_text(size = 16, colour="grey60"), legend.position="none", panel.grid.major.y = element_line(linetype="dotted", colour="grey60"), panel.grid.major.x = element_line(colour="white"), panel.grid.minor = element_line(colour="white")) + theme(axis.line.x=element_line(colour="white"), axis.line.y=element_line(colour="white"), axis.ticks = element_blank(), strip.text = element_text(size = 16), strip.background = element_rect(colour="white"))

# Untreated coal
ucc <- "#33a02c"
ucp <- ggplot(fuel_quantity, 
  aes(x=year, y=value, group=T_Timing))  + 
  # geom_line(show.legend=F, colour="grey") +
  geom_line(data=subset(fuel_quantity, name=="Biomass" & 
    T_Timing==0), colour="black", linewidth=1) +
  geom_line(data=subset(fuel_quantity, name=="Coal" & 
    T_Timing==0), colour=ucc, linewidth=1.5) +
  geom_point(data=subset(fuel_quantity, name=="Coal" & 
    T_Timing==0), shape=21, colour="white", 
    fill=ucc, size=3, stroke=2) +
  scale_y_continuous(limits=c(0,3100),
    breaks=c(0,1000,2000,3000)) +
  annotate("text", x = 2018, y = 2500, color=ucc, 
    label = "Coal", hjust=0, size=5) +
  annotate("text", x = 2018, y = 800, color="black", 
           label = "Biomass", hjust=0, size=5) +
  annotate('curve',
           x = 2018.5, y = 2500, yend = 2700, xend = 2019,
           linewidth = 0.5,
           curvature = 0.5,
           color = ucc,
           arrow = arrow(length = unit(0.25, 'cm'))) +
  annotate('curve',
           x = 2018.9, y = 800, yend = 950, xend = 2019.4,
           linewidth = 0.5,
           curvature = 0.5,
           color = "black",
           arrow = arrow(length = unit(0.25, 'cm'))) +
  # scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("Solid fuel consumption (kg)") + stheme +
  ggtitle("Never treated") +
  theme(plot.title = element_text(hjust=0.5))

# Treated in 2019 coal
# fcc <- "#377eb8"
fcp <- ggplot(fuel_quantity, 
         aes(x=year, y=value, group=T_Timing))  + 
  geom_vline(xintercept = 2019, linetype="dashed") +
  geom_line(data=subset(fuel_quantity, name=="Biomass" & 
                          T_Timing==2019), colour="black", linewidth=1) +
  geom_line(data=subset(fuel_quantity, name=="Coal" & 
                          T_Timing==2019), colour=ucc, linewidth=1.5) +
  geom_point(data=subset(fuel_quantity, name=="Coal" & 
                           T_Timing==2019), shape=21, colour="white", 
             fill=ucc, size=3, stroke=2) +
 # annotate("text", x = 2019, y = 3100, color=fcc, 
  #  label = "Treated in 2019", hjust=0) +
  scale_y_continuous(limits=c(0,3100),
                     breaks=c(0,1000,2000,3000)) +
  annotate("segment", x = 2019.3, xend = 2019.1, size=0.5, 
    y = 2800, yend = 2800, 
    arrow = arrow(length = unit(0.15, 'cm'))) +
  annotate("text", x = 2019.3, y = 2800, color="black", 
           label = "Intervention", hjust=0, size=5) +
  # scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Treated in 2019") +
  theme(plot.title = element_text(hjust=0.5),
    axis.line.y = element_blank(), axis.text.y = element_blank())


# Treated in 2020 coal
# scc <- "#4daf4a"
scp <- ggplot(fuel_quantity, 
              aes(x=year, y=value, group=T_Timing))  + 
  geom_vline(xintercept = 2020, linetype="dashed") +
  geom_line(data=subset(fuel_quantity, name=="Biomass" & 
                          T_Timing==2020), colour="black", linewidth=1) +
  geom_line(data=subset(fuel_quantity, name=="Coal" & 
                          T_Timing==2020), colour=ucc, linewidth=1.5) +
  geom_point(data=subset(fuel_quantity, name=="Coal" & 
                           T_Timing==2020), shape=21, colour="white", 
             fill=ucc, size=3, stroke=2) +
  # annotate("text", x = 2019, y = 3100, color=fcc, 
  #  label = "Treated in 2019", hjust=0) +
  scale_y_continuous(limits=c(0,3100),
                     breaks=c(0,1000,2000,3000)) +
  # scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Treated in 2020") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line.y = element_blank(), axis.text.y = element_blank())


# Treated in 2021 coal
# tcc <- "#984ea3"
tcp <- ggplot(fuel_quantity, 
              aes(x=year, y=value, group=T_Timing))  + 
  geom_vline(xintercept = 2021, linetype="dashed") +
  geom_line(data=subset(fuel_quantity, name=="Biomass" & 
                          T_Timing==2021), colour="black", linewidth=1) +
  geom_line(data=subset(fuel_quantity, name=="Coal" & 
                          T_Timing==2021), colour=ucc, linewidth=1.5) +
  geom_point(data=subset(fuel_quantity, name=="Coal" & 
                           T_Timing==2021), shape=21, colour="white", 
             fill=ucc, size=3, stroke=2) +
  # annotate("text", x = 2019, y = 3100, color=fcc, 
  #  label = "Treated in 2019", hjust=0) +
  scale_y_continuous(limits=c(0,3100),
                     breaks=c(0,1000,2000,3000)) +
  # scale_x_continuous(breaks=c(2005,2015)) +
  xlab("") + ylab("") + stheme +
  ggtitle("Treated in 2021") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line.y = element_blank(), axis.text.y = element_blank())

# put plots for men together
cp <- (ucp | fcp | scp | tcp) # + plot_annotation(subtitle = 'Coal')
cp

ggsave(here("images", "coal-plot.png"), 
       plot=cp, width=14, height=5)


