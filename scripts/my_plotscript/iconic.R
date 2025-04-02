require(dplyr)
require(tidyr)
require(ggplot2)
require(quitte)
require(mip)
require(purrr)

require(plotly) #install.packages("plotly")
library(magick) #install.packages("magick")
library(magrittr) #install.packages("magrittr")
require(stringr)

for (kfile in list.files("~/Documents/INTEGRATE/data_comparison_AR6/functions/", pattern="*.R")) source(paste0("~/Documents/INTEGRATE/data_comparison_AR6/functions/", kfile))

library(cowplot)  # Useful for themes and for arranging plots
library(metR)  # Useful for contour_fill 
library(scales)
library(dichromat)

# Plotting style ----------------------------------------------------------
theme_set(theme_cowplot(font_size = 8))  # Use simple theme and set font size

options(warn=-1)

setwd('~/source/REMIND_integrate/remind/output')
run_number = "standalone_v23"

data.dir = paste0("./",run_number)

plot.dir = paste0("./plots/", run_number,"/")
dir.create(plot.dir, showWarnings = FALSE)

regs = c("World", "CHA", "EUR")
reg = "CHA"
plot.period <- seq(2015, 2060, 5)

tt = plotstyle.add("1.5C", "1.5C", "#009E73", linestyle = "solid", marker = 19, replace = T)
tt = plotstyle.add("2C", "2C", "#882288", linestyle = "solid", marker = 19,replace = T)

scenarios_mapping_2C = c(
  "Base_INT" = "Baseline",
  # "elec_INT_1150_po_plateau30" = "2C plateau 30",
  # "elec_INT_1150_po_plateau25" = "2C plateau 25",
  # "elec_INT_1150_po_slope_fast" = "2C fast",
  # "elec_INT_1150_po_slope_med" = "2C med",
  # "elec_INT_1150_po_slope_slo" = "2C slow",
  
  "elec_INT_1150_po_plateau30_adj" = "2C plateau 30 adj",
  "elec_INT_1150_po_plateau25_adj" = "2C plateau 25 adj",
  "elec_INT_1150_po_slope_fast_adj" = "2C fast adj",
  "elec_INT_1150_po_slope_med_adj" = "2C med adj",
  "elec_INT_1150_po_slope_slo_adj" = "2C slow adj",
  NULL)

c("#0072B2", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

pe.color.mapping <- c("PE|Gas" = "#999959", "PE|Coal" = "#0c0c0c",
                   "PE|Solar" = "#ffcc00", "PE|Wind" = "#337fff", 
                   "PE|Geothermal" = "#334cff", "PE|Biomass" = "#005900",
                   "PE|Oil" = "#e51900", "PE|Hydro" = "#191999", "PE|Nuclear" = "#ff33ff",
                   NULL)

scenario_color_mapping_2C = c(
  "Baseline" = "#CC79A7",
  "2C plateau 30" = "#0072B2",
  "2C plateau 25" = "#882288",
  "2C slow" = "#56B4E9",
  "2C med" = "#009E73",
  "2C fast" = "#D55E00",
  "2C med adj" = "#E69F00",
  "2C slow adj" = "#56B4E9",
  "2C fast adj" = "#e51900",
  "2C plateau 30 adj" = "#0072B2",
  "2C plateau 25 adj" = "#882288",
  NULL)

scenarios_mapping_1.5C = c(
  "elec_INT_500_po_plateau30" = "1.5C plateau 30",
  "elec_INT_500_po_plateau25" = "1.5C plateau 25",
  "elec_INT_500_po_slope_slo" = "1.5C slow",
  "elec_INT_500_po_slope_med" = "1.5C medium",
  "elec_INT_500_po_slope_fast" = "1.5C fast",
  NULL)

scenario_color_mapping_1.5C = c(
  "1.5C plateau 30" = "#0072B2",
  "1.5C plateau 25" = "#E69F00",
  "1.5C slow" = "#56B4E9",
  "1.5C medium" = "#009E73",
  "1.5C fast" = "#D55E00",
  NULL)

scenario_group_toplot <- c(list(scenarios_mapping_2C))
scenario_group_names <- c("2C")
scenario_group_color <- c(list(scenario_color_mapping_2C))

# scenario_group_toplot <- c(list(scenarios_mapping_1.5C))
# scenario_group_names <- c("1.5C")
# scenario_group_color <- c(list(scenario_color_mapping_1.5C))

for (i in c(1:length(scenario_group_toplot))){
  
  scenarios_mapping <- scenario_group_toplot[[i]]
  scenario_color_mapping <- scenario_group_color[[i]]
  scenario_color_mapping.whist <- c(scenario_color_mapping, "historical" = "#000000")
  scenario_group_name <- scenario_group_names[[i]]
  
  data.files = lapply(names(scenarios_mapping), function(x) paste0( data.dir, "/REMIND_generic_", x, "_withoutPlus.mif")) %>% unlist
  
  run_indices = seq(from = 1, to = length(data.files), by = 1)
  df.raw = NULL

# trim df to chosen scenarios and regions
  for (i in run_indices){
    print(i)
    REMIND.data = read.quitte(data.files[[i]])
    df.raw <- rbind(df.raw, REMIND.data)
  }

  scens0 = stringr::str_replace(names(scenarios_mapping), "REMIND_generic_", "")
  scens = stringr::str_replace(scens0, "-rem-5", "")
  
  df0 = filter(df.raw, scenario %in% scens, region %in% reg ) %>%
    order.levels(scenario = scens) %>%
    revalue.levels(scenario = scenarios_mapping) %>%
    factor.data.frame()
  
  df.world = filter(df.raw, scenario %in% scens, region %in% c("World") ) %>%
    order.levels(scenario = scens) %>%
    revalue.levels(scenario = scenarios_mapping) %>%
    factor.data.frame()
  
  df_test = filter(df.world, variable == "Emi|CO2|Gross|Energy|Supply|Electricity")

###################### Rescale price ######################
# rescale price and cost data to 2020 using 2005->2020 deflator of 1.34 (https://stats.oecd.org/Index.aspx?DataSetCode=PRICES_CPI)

indices = grepl("US\\$2005",df0$unit)
df0[indices, "value"] = df0[indices, "value"] * 1.52
levels(df0$unit) = gsub("US\\$2005", "US\\$2020", levels(df0$unit))

# MJ/US$2020

rm(df.raw)

######## Tsinghua mif #########################
data.TH = read.quitte(paste0("C-GEM_CHA_NZ.mif"))

###################### Historical mif ######################################
tmax = 2060

# also read in historical data
df.hist = read.quitte(paste0("./",run_number, "/historical.mif"))
df.hist = 
  calc_addVariable(df.hist, 
                   "`Share|FE|Electricity`" = "`FE|Electricity` / `FE` * 100",
                   units  = "%") 

df.hist_FE = filter(df.hist, model =="IEA", region %in% regs ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("FE", "FE|Industry", "FE|Building", "FE|Transport", "FE|Electricity"))

df.hist.emi = df.hist %>%
  filter(model =="PRIMAPhist", region %in% c("CHA") ) %>% 
  filter(variable == "Emi|CO2") %>%
  mutate(value = value/1e3) %>% 
  select(-scenario)

df.hist_cap = filter(df.hist, model =="IEA", region %in% regs ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("FE", "FE|Industry", "FE|Building", "FE|Transport", "FE|Electricity"))

df.hist_eshare = filter(df.hist, model =="IEA", region %in% regs ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("Share|FE|Electricity"))

df.hist_eshare2 = filter(df.hist, model =="IEA_WEO", region %in% regs, period >= 1971, period <= 2020 ) %>%
  factor.data.frame() %>% 
  filter(variable %in% c("Share|FE|Electricity"))

var.pe <- c("PE|Coal",
            "PE|Oil",
            "PE|Gas",
            "PE|Biomass",
            "PE|Nuclear",
            "PE|Solar",
            "PE|Wind",
            "PE|Hydro",
            "PE|Geothermal")

df.hist.pe = filter(df.hist, model =="IEA_WEO", region %in% regs, period >= 1971, period <= 2020 ) %>%
  factor.data.frame() %>% 
  filter(variable %in% var.pe) %>% 
  dplyr::group_by(period) %>%
  dplyr::summarise( value = sum(value) ) %>% 
  dplyr::ungroup()

rm(df.hist)

df.hist.ember = read.quitte("./Ember/Ember_CHA.csv") %>% 
  factor.data.frame() %>% 
  mutate(period = as.numeric(period)) 

df.hist.ember <-  df.hist.ember %>% 
  calc_addVariable( "`Carbon Intensity|Electricity`" = "(`Emi|CO2|Energy|Supply|+|Electricity w/ couple prod`) / (`SE|Electricity`)*3.6", units  = "tCO2/MWh")  %>% 
  calc_addVariable( "`Capacity Factor|Coal`" = "(`SE|Electricity|+|Coal`)*2.7e5  / ((`Cap|Electricity|Coal`) * 8760) * 100", units  = "%")

df.hist.ember.cap <-  df.hist.ember %>% 
  filter(variable == "Cap|Electricity|Coal") %>% 
  filter(region == "CHN" )

df.hist.ember.gen <-  df.hist.ember %>% 
  filter(variable == "SE|Electricity|+|Coal") %>% 
  filter(region == "CHN" )

df.hist_FE = calc_addVariable(df.hist_FE,
                   "`FE|Electricity|Share`" = "`FE|Electricity` / `FE` * 100",
                   units  = "%") 

###################### CI vs. elec rate #####################################
vars = c("FE|Electricity", "FE")

df.plot.elecshare = filter(df0, variable == "FE|Electricity|Share", period <= 2060, region == reg ) 

df.hist.plot.elecshare  = filter(df.hist_FE, variable == "FE|Electricity|Share", period <= 2060 , region == reg) 

p <- ggplot() +
  geom_line(data = df.hist.plot.elecshare, aes(x = period, y = value), size = 1,  color = "black") +
  geom_line(data = df.plot.elecshare, aes(x = period, y = value, color = scenario,  size = tech), size = 0.4, alpha = 0.5) +
  # geom_point(data = df.plot, aes(x = period, y = value, color = scenario), size = 1.2) +
  scale_size_manual( values = c(1,1.7)) +
  ylab("Electricity Share in Final Energy [%]") +
  xlab("") +
  scale_x_continuous(limits = c(1995, 2060), breaks = c(2000, 2020, 2040, 2060),
                     minor_breaks = unique(seq(2000, 2060,10)), ) +
  theme_bw(base_size = 7) +
  theme(legend.key.width = unit(1.5,"cm"))

ggsave(filename = paste0(plot.dir, "/", scenario_group_name, "_Elecshares.png"), width=11, height=6, units = "cm")

df.ci <- df0 %>% 
  calc_addVariable("`Carbon Intensity|Electricity`" = "(`Emi|CO2|Gross|Energy|Supply|Electricity`) / (`SE|Electricity` -`SE|Electricity|Hydrogen`)*3.6", units = "tCO2/MWh")  

df.plot.ci <- df.ci %>% 
  filter(variable == "Carbon Intensity|Electricity") %>% 
  filter(period <= 2060)

p <- ggplot() +
  geom_line(data = df.plot.ci, aes(x = period, y = value, color = scenario), size = 1, alpha = 0.85) +
  geom_point(data = df.plot.ci, aes(x = period, y = value, color = scenario), size = 1.5) +
  # scale_size_manual( values = c(1,1.7)) +
  scale_x_continuous(limits = c(2015, 2060), breaks = c( 2020, 2040,  2060, 2080, 2100),
                     minor_breaks = unique(seq(2000, 2100,10)), ) +
  ylab("Fossil CO2 Intenstity [kgCO2/MWh]") + 
  scale_color_manual(name = "scenario", values = scenario_color_mapping) + 
  xlab("") + 
  theme_bw(base_size = 8) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5)) 

vari = "FE|Electricity|Share"

df.plot.eshare = filter(df0, variable == vari, period <= 2060 )

plot_scale = 10

p <- p +
  geom_line(data = df.plot.eshare, aes(x = period, y = value*plot_scale, color = scenario), size = 1, alpha = 0.3) +
  geom_point(data = df.plot.eshare , aes(x = period, y = value*plot_scale, color = scenario), size = 1) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  coord_cartesian(ylim = c(0,800)) +
  scale_y_continuous(sec.axis = sec_axis(~./plot_scale, name = "Electricity Share in Final Energy [%]"))

ggsave(filename = paste0(plot.dir, "/CI-elecR_", scenario_group_name, ".png"),width=10, height=8, units = "cm", bg = "white")

df.plot.ci.small <- df.plot.ci %>% 
  select(scenario,period, ci = value)

df.plot.eshare.small <- df.plot.eshare %>% 
  select(scenario,period, eshare = value)

remind.ci.elshare <- list(df.plot.ci.small, df.plot.eshare.small) %>% 
  reduce(full_join) 

p <- ggplot() +
  geom_text(data = remind.ci.elshare %>% 
              filter(scenario !="2C default") %>% 
              filter(scenario %in% c(scenarios_mapping[[4]])), aes(x = eshare, y = ci, color = scenario, label=period), size = 2, alpha = 0.8, hjust=1.5, vjust=0) +
  geom_point(data = remind.ci.elshare %>% filter(scenario !="2C default"), aes(x = eshare, y = ci, color = scenario, label=period), size = 2, alpha = 0.8) +
  geom_line(data = remind.ci.elshare %>% filter(scenario !="2C default"), aes(x = eshare, y = ci, color = scenario, label=period), size = 1, alpha = 0.3) +
  ylab("Fossil CO2 Intenstity [kgCO2/MWh]") + 
  scale_color_manual(name = "scenario", values = scenario_color_mapping) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8, face="bold"), strip.text = element_text(size=8)) +
  xlab("Electricity Share in Final Energy [%]") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=8)) +
guides(color=guide_legend(nrow=2, byrow=TRUE))

ggsave(filename = paste0(plot.dir, "/CI-elecR-cross_", scenario_group_name, ".png"),p, width=10, height=10, units = "cm", bg = "white")


###################### power sector ##################################
# coal capacity
df.plot.coal.tot.cap = filter(df0, variable == "Cap|Electricity|Coal", period <= 2060, region == reg )

df.plot.coal_wcc = filter(df0, variable == "Cap|Electricity|Coal|w/ CC", period <= 2060, region == reg ) 

p <- ggplot() +
  geom_line(data = df.hist.ember.cap, aes(x = period, y = value, color = scenario), size = 2, alpha = 1)+
  geom_line(data = df.plot.coal.tot.cap %>% 
              filter(scenario !="2C default"), aes(x = period, y = value, color = scenario), size = 2, alpha = 0.8) +
  ylab("Coal Power Capacity [GW]") + 
  scale_color_manual(name = "scenario", values = scenario_color_mapping.whist) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8, face="bold"), strip.text = element_text(size=8)) +
  xlab("") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=8)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

ggsave(filename = paste0(plot.dir, "/coalCap_", scenario_group_name, ".png"),p, width=10, height=10, units = "cm", bg = "white")

# coal generation
df.plot.coal.tot.gen = filter(df0, variable == "SE|Electricity|Coal", period <= 2060, region == reg )

p <- ggplot() +
  geom_line(data = df.hist.ember.gen, aes(x = period, y = value, color = scenario), size = 2, alpha = 1)+
  geom_line(data = df.plot.coal.tot.gen %>% 
              filter(scenario !="2C default"), aes(x = period, y = value, color = scenario), size = 2, alpha = 0.8) + 
  ylab("Coal Power Generation [EJ/yr]") + 
  scale_color_manual(name = "scenario", values = scenario_color_mapping.whist) + 
  theme(axis.text=element_text(size=8), axis.title=element_text(size= 8, face="bold"), strip.text = element_text(size=8)) +
  xlab("") + 
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(), legend.text = element_text(size=8)) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

ggsave(filename = paste0(plot.dir, "/coalGen_", scenario_group_name, ".png"),p, width=11, height=7, units = "cm", bg = "white")


##################### SE Electricity Supply and Demand Mix ####################
### plot with positive supply and negative demand bars

el.supp.vars.mapping <- c("SE|Electricity|Wind|Offshore" = "Wind Offshore",
                          "SE|Electricity|Wind|Onshore" = "Wind Onshore",
                          "SE|Electricity|Solar" = "PV",
                          "SE|Electricity|Hydrogen" = "Hydrogen",
                          "SE|Electricity|Gas" = "Gas",
                          "SE|Electricity|Nuclear" = "Nuclear",
                          "SE|Electricity|Biomass" = "Biomass",
                          "SE|Electricity|Solar|CSP" = "CSP",
                          "SE|Electricity|Hydro" = "Hydro",
                          "SE|Electricity|Geothermal" = "Geothermal",
                          "SE|Electricity|Coal|w/ CC" = "Coal CCS",
                          "SE|Electricity|Coal|w/o CC" = "Coal without CCS", 
                          "SE|Electricity|Oil" = "Oil",
                          NULL)

el.dem.vars.mapping <- c(
                 "SE|Input|Electricity|Self Consumption Energy System" = "for own consumption",
                 "SE|Input|Electricity|Hydrogen|direct FE H2" = "for direct H2",
                 "SE|Input|Electricity|Hydrogen|Electricity Storage" = "for power storage",
                 "SE|Input|Electricity|Hydrogen|Synthetic Fuels"  = "for synthetic fuel", 
                 # "SE|Input|Electricity|CDR" = "for CDR",
                 "SE|Input|Electricity|Buildings" = "for Buildings",
                 "SE|Input|Electricity|Industry" = "for Industry",
                 "SE|Input|Electricity|Transport" = "for Transport",
                 # "SE|Input|Electricity|Hydrogen|Synthetic Fuels|Liquids" = "for synthetic liquids", 
                 # "SE|Input|Electricity|Hydrogen|Synthetic Fuels|Gases" = "for synthetic gases",
                 NULL)

el.vars.mapping <- c(el.supp.vars.mapping, el.dem.vars.mapping)

el.color.mapping <- c( "PV" = "yellow",
                       "Wind Offshore" = "#0085B2",
                       "Wind Onshore" = "deepskyblue",
                      "Hydrogen" = "cyan",
                      "Nuclear" = "darkorchid",
                      "Gas" = "grey80",
                      "Biomass" = "darkgreen",
                      "CSP" = "#8E4585",
                      "Hydro" = "#186FEF",
                      "Geothermal" = "#ff8242",
                      "Coal CCS" = "#e9e3c8",
                      "Coal without CCS" = "#222444", 
                      "Oil" = "#666633",
                      "for own consumption" = "darkgoldenrod",
                      "for direct H2" = "darkcyan",
                      "for power storage" = "mediumpurple1",
                      # "for CDR" = "lightgreen",
                      "for Buildings" = "red",
                      "for Industry" = "grey50",
                      "for Transport" = "blue",
                      "for synthetic fuel" = "lightyellow3"
                      # "for synthetic liquids" = "lightyellow3", 
                      # "for synthetic gases" = "navajowhite1"
                      )

vars.order <- c("PV",
                "Wind",
                "Hydrogen",
                "Nuclear",
                "Biomass",
                "CSP",
                "Hydro",
                "Geothermal",
                "Coal|w/ CC",
                "Coal|w/o CC",
                "Oil",
                "Gas",
                "Other Fossil",
                # "for synthetic gases",
                # "for synthetic liquids",
                "for synthetic fuel",
                "for power storage",
                "for direct H2",
                # "for CDR",
                "for Buildings",
                "for Industry",
                "for Transport",
                "for own consumption")

for(scen in scenarios_mapping){
  
df.el <- df0 %>% 
  filter(scenario == scen) %>% 
  filter(variable %in% names(el.supp.vars.mapping)) %>% 
  # bind demand variables with negative sign
  rbind(df0 %>% 
          filter(scenario == scen) %>% 
           filter(variable %in% names(el.dem.vars.mapping)) %>%
           mutate( value = - value)) %>% 
  revalue.levels(variable = el.vars.mapping) %>% 
  order.levels(variable = names(el.color.mapping)) %>% 
  filter(period %in% plot.period)

  p.el <- ggplot() +
    geom_col(data=df.el, 
             aes(period, value, fill=variable), 
             alpha=.6, width = 3) +
    facet_wrap(~scenario) +
    scale_y_continuous("Electricity (EJ/yr)") +
    scale_x_continuous("") +
    scale_fill_manual(values = el.color.mapping) +
    theme_bw() +
    theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
           legend.position = "bottom",
           strip.background = element_blank(),
           legend.title = element_blank(),
           strip.text.y = element_text(angle = 0, hjust = 0)) +
    guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 4, byrow = TRUE))
  
  ggsave(plot = p.el,
         filename =  paste0(plot.dir, "/",reg, "_", scen, "_El_SinkSource.png"),
         width=15, height=16, units="cm", bg = "white")
}

df.el.scens <- df0 %>% 
  filter(variable %in% names(el.supp.vars.mapping)) %>% 
  # bind demand variables with negative sign
  rbind(df0 %>% 
          filter(variable %in% names(el.dem.vars.mapping)) %>%
          mutate( value = - value)) %>% 
  revalue.levels(variable = el.vars.mapping) %>% 
  order.levels(variable = names(el.color.mapping)) %>% 
  filter(period %in% c(2020,2030,2040,2060))

p.compare <- ggplot() +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[1]]) %>% 
             mutate(period = period -1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[2]]) %>% 
             mutate(period = period), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[3]]) %>% 
             mutate(period = period +1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.el.scens %>% 
             filter(scenario== scenarios_mapping[[4]]) %>% 
             mutate(period = period + 2), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(name = "Technology", values = el.color.mapping) + 
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=10)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))+
  scale_linetype_discrete(breaks=scenarios_mapping) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
  xlab("period") + ylab(paste0("Generation (TWh)")) 

ggsave(plot = p.compare,
       filename =  paste0(plot.dir, "/",reg,"_",scenario_group_name,"_El_SinkSource_compare.png"),
       width=10, height=12, units="cm", bg = "white")

#################### cumulative emission ####################
df.cum <- df0 %>% 
  filter(variable == "Emi|CO2|Cumulated") %>% 
  filter(period %in% plot.period) %>% 
  select(scenario,period,cum_cha = value)

df.cum.world <- df.world %>% 
  filter(variable == "Emi|CO2|Cumulated") %>% 
  filter(period %in% plot.period)%>% 
  select(scenario,period,cum_world = value)

df.cum.fraction <- list(df.cum.world, df.cum) %>% 
  reduce(full_join) %>% 
  mutate(value = cum_cha/cum_world*1e2)

df.cum.elec <- df0 %>% 
  filter(variable == "Emi|CO2|Cumulated|Gross|Energy|Supply|Electricity") %>% 
  filter(period %in% plot.period) %>% 
  select(scenario,period,cum_cha_elec = value)

df.cum.fraction.elec <- list(df.cum.world, df.cum.elec) %>% 
  reduce(full_join) %>% 
  mutate(value = cum_cha_elec/cum_world*1e2)

p.cum <- ggplot() +
  geom_line(data=df.cum %>% mutate(cum_cha = cum_cha /1e3), 
            aes(period, cum_cha, color=scenario), 
            alpha=1, width = 5) +
  xlab("") + ylab(paste0("China's cumulative emission (Gt)")) +
  scale_color_manual(name = "scenario", values = scenario_color_mapping) + 
  # theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), strip.text = element_text(size = 8)) +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 2, byrow = TRUE)) + 
theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5)) +
  theme(legend.key.width = unit(0.5,"cm"))

ggsave(plot = p.cum,
       filename =  paste0(plot.dir, "/",reg, "_Cumulative.png"),
       width=8, height=12, units="cm", bg = "white")

p.cum.elec <- ggplot() +
  geom_line(data=df.cum.elec %>% mutate(cum_cha_elec = cum_cha_elec /1e3), 
            aes(period, cum_cha_elec, color=scenario), 
            alpha=1, width = 5) +
  xlab("") + ylab(paste0("China's cumulative power sector emission (Gt)")) +
  scale_color_manual(name = "scenario", values = scenario_color_mapping) + 
  # theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), strip.text = element_text(size = 8)) +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 2, byrow = TRUE))+
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=7)) +
  theme(legend.key.width = unit(0.5,"cm"))

ggsave(plot = p.cum.elec,
       filename =  paste0(plot.dir, "/",reg, "_ElecCumulative.png"),
       width=9, height=9, units="cm", bg = "white")

p.cum_frac <- ggplot() +
  geom_line(data=df.cum.fraction, 
           aes(period, value, color=scenario), 
           alpha=.8, width = 6) +
  xlab("") + ylab(paste0("China's cumulative emission share of world total (%)")) +
  scale_color_manual(name = "scenario", values = scenario_color_mapping) + 
  theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=6), strip.text = element_text(size = 8)) +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 2, byrow = TRUE))

ggsave(plot = p.cum_frac,
       filename =  paste0(plot.dir, "/",reg, "_Cumulative_Frac.png"),
       width=10, height=12, units="cm", bg = "white")

p.cum_frac.elec <- ggplot() +
  geom_line(data=df.cum.fraction.elec, 
            aes(period, value, color=scenario), 
            alpha=.8, width = 6) +
  xlab("") + ylab(paste0("China's cumulative power sector \n emission share of world total (%)")) +
  scale_color_manual(name = "scenario", values = scenario_color_mapping) + 
  theme_bw() +
  theme(axis.text=element_text(size=6), axis.title=element_text(size=6), strip.text = element_text(size = 6)) +
  theme( axis.text.x = element_text(angle=90, vjust = 0.5), 
         legend.position = "bottom",
         # legend.text = element_text(size=7),
         strip.background = element_blank(),
         legend.title = element_blank(),
         strip.text.y = element_text(angle = 0, hjust = 0)) +
  guides(fill=guide_legend(ncol=3, direction = "horizontal"), color = guide_legend(nrow = 3, byrow = TRUE))+
  theme( legend.text = element_text(size=5))

ggsave(plot = p.cum_frac.elec,
       filename =  paste0(plot.dir, "/",reg, "_Cumulative_Elec_Frac.png"),
       width=7, height=8, units="cm", bg = "white")


df.temp <- df.world %>% 
  filter(variable == "Temperature|Global Mean") %>% 
  filter(period < 2060) %>% 
  filter(period > 2035) 

p.temp <- ggplot() +
  geom_line(data=df.temp, 
            aes(period, value, color=scenario), 
            alpha=.8, width = 6) +
  xlab("") + ylab(paste0("World Mean Temperature (C)")) +
  scale_color_manual(name = "scenario", values = scenario_color_mapping) + 
  theme_bw() +
  theme(axis.text=element_text(size=8), axis.title=element_text(size=8), strip.text = element_text(size = 8)) +
  theme(legend.position="bottom")+
  guides(fill=guide_legend(ncol=4, direction = "horizontal"), color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(plot = p.temp, 
       filename =  paste0(plot.dir, "/",reg, "_world_temp.png"),
       width=10, height=12, units="cm", bg = "white")

p.legend <- get_legend(p.cum +
                         theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=6)) +
                         guides(color = guide_legend(nrow = 1, byrow = TRUE))+
                         theme(legend.box.margin = margin(0, 0, 0, 2)))

p.plots <- plot_grid(p.cum +
                 theme(legend.position = "none"),
               p.cum_frac +
                 theme(legend.position = "none"),
               p.temp +
                 theme(legend.position = "none"),
               # labels = c("(a)", "(b)", "(c)"),
               labels = c("", "", ""),
               label_size = 9,
               nrow = 1)

p <- plot_grid(p.plots,
               # p.legend,
               label_size = 11,
               nrow = 2,
               rel_heights = c(1, 0.2))

ggsave(plot = p, 
       filename =  paste0(plot.dir, "/",reg,"_",scenario_group_name, "_emi_temp.png"),
       width=18, height=7, units="cm", bg = "white")


################# PE #################

df.pe <- df0 %>% 
  filter(variable %in% var.pe) %>% 
  filter(period <2065) 

for (i in c(1:length(scenarios_mapping))){
  
  scen <- scenarios_mapping[[i]]
  
p.pe <- 
  mip::mipArea(df.pe %>% filter(scenario == scen,period >2010, period < 2065), total = F) +
  geom_line(data = df.hist.pe %>% filter(period >2015), aes(x=period, y = value), color = "black", size =2 ) +
  ylab("Primary Energy [EJ/yr]") +
  xlab("") +
  theme_bw(base_size = 7) + 
  theme(strip.background = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank(),legend.text = element_text(size=5), legend.key.size = unit(0.5, 'cm')) 

ggsave(plot = p.pe,
       filename =  paste0(plot.dir, "/",reg,"_",scen,"_PE.png"),
       width=8, height=10, units="cm", bg = "white")
  }

df.pe <- df0 %>% 
  filter(variable %in% var.pe) %>% 
  filter(period %in% c(2020,2030,2040, 2060)) 

p.pe.compare <- ggplot() +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[1]]) %>% 
             mutate(period = period -1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[2]]) %>% 
             mutate(period = period), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[3]]) %>% 
             mutate(period = period +1), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) +
  geom_bar(data = df.pe %>% 
             filter(scenario== scenarios_mapping[[4]]) %>% 
             mutate(period = period + 2), 
           aes(x=period, y=value, fill=variable, linetype=scenario), colour = "black", stat="identity", position="stack", width=1) + 
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(name = "", values = pe.color.mapping) +
  theme(legend.title = element_blank()) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.text = element_text(size=10)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE), linetype=guide_legend(nrow=4,byrow=TRUE))+
  scale_linetype_discrete(breaks=scenarios_mapping) +
  theme(axis.text = element_text(size=10), axis.title = element_text(size= 10, face="bold")) +
  xlab("period") + ylab(paste0("PE")) 

ggsave(plot = p.pe.compare,
       filename =  paste0(plot.dir, "/",reg,"_",scenario_group_name,"_PE_compare.png"),
       width=16, height=12, units="cm", bg = "white")
}

############################## AR6 data ###############################
load('/home/chengong/source/ar6data/data/ar6data_CHA.Rds')
load('/home/chengong/source/ar6data/data/ar6scenarios.rda')

sw.reload.ar6 = T

if(sw.reload.ar6){
  # require(ar6data)
  # convert metadata into a more useful format
  ar6meta <- ar6scenarios %>% spread(key = indicator, value = value) 
  
  ar6meta.short <- ar6meta %>% select(model, scenario, Category) %>% 
    factor.data.frame()
  
  varis = c("Price|Carbon", 
            "SE|Electricity|Wind",
            "SE|Electricity|Solar", 
            "SE|Hydrogen",
            "SE|Hydrogen|Electricity", 
            "SE|Electricity|Hydro",
            "SE|Electricity|Non-Biomass Renewables",
            "SE|Hydrogen|Electricity",          
            "SE|Heat|Geothermal",
            "SE|Heat",
            "SE|Electricity",
            "FE|Electricity",
            "FE|Heat",
            "FE|Solar",
            "FE|Geothermal",
            "FE|Hydrogen",
            "FE|Gases",
            "FE|Solids",
            "FE|Liquids",
            "FE|Residential and Commercial|Electricity",
            "FE|Residential and Commercial|Hydrogen",
            "FE|Residential and Commercial",
            "FE|Transportation|Electricity",
            "FE|Transportation|Hydrogen",
            "FE|Transportation",
            "FE|Industry|Electricity",
            "FE|Industry",
            "FE", 
            "PE|Non-Biomass Renewables",
            "PE|Biomass",
            "PE|Oil", 
            "PE|Coal", 
            "Emi|CO2",
            "Emi|CO2|Energy",
            "PE|Gas", 
            "PE",
            "Carbon Sequestration|CCS",
            "Carbon Sequestration|CCS|Biomass",
            "Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity",
            "Carbon Sequestration|Land Use", 
            "Emi|CO2|Energy|Supply|Electricity",
            NULL)
  
  indicators = c( "Population",
                  "GDP|MER", 
                  "Emi|CO2|Energy and Industrial Processes",
                  "FE",
                  "Share|FE|Electricity",
                  "Share|FE|Non-Bio RE", 
                  # "Share|FE|Electricity|Transport", 
                  # "Share|FE|Electricity|Industry", 
                  # "Share|FE|Electricity|Buildings", 
                  "Share|PE|Fossil",
                  "PE|Biomass",
                  "CDR",
                  "Price|Carbon",
                  NULL)
  
  varis <-c(varis, indicators)
  
  temp.ar6data = ar6data.cha 
  
  # temp.ar6data$variable =  gsub( "Primary Energy","PE",  temp.ar6data$variable)
  levels(temp.ar6data$variable) <-  gsub(  "Primary Energy","PE",  levels(temp.ar6data$variable))
  levels(temp.ar6data$variable) <-  gsub(  "Secondary Energy","SE",  levels(temp.ar6data$variable))
  levels(temp.ar6data$variable) <-  gsub(  "Final Energy", "FE", levels(temp.ar6data$variable))
  levels(temp.ar6data$variable) <-  gsub(  "Emissions", "Emi", levels(temp.ar6data$variable))
  
  temp.ar6data = factor.data.frame(temp.ar6data)
  
  ar6data.wmeta = temp.ar6data  %>%
    filter(region == "R10CHINA+",
           variable %in% varis ) %>% 
    select(-region)
  
  indices = grepl("US\\$2010",ar6data.wmeta$unit)
  ar6data.wmeta[indices, "value"] = ar6data.wmeta[indices, "value"] * 1.1
  levels(ar6data.wmeta$unit) = gsub("US\\$2010", "US\\$2020", levels(ar6data.wmeta$unit))
  
  ar6data.wmeta <- ar6data.wmeta %>% factor.data.frame()
  
  # rm(temp.ar6data)
  
  # fix reporting errors: FE|Solar reports PV in WITCH, IMAGE
  ar6data.wmeta[(grepl("IMAGE", ar6data.wmeta$model) | grepl("IMAGE", ar6data.wmeta$model))  & (ar6data.wmeta$variable == "FE|Solar"), "value"] = 0
  
  ar6data.wmeta = ar6data.wmeta %>%
    calc_addVariable( "`GDP|per capita|MER`" = "`GDP|MER` / `Population`",
                      units  = "Thousand$/capita") %>%
    calc_addVariable( "`Intensity|Final Energy|CO2`" = "`Emi|CO2|Energy and Industrial Processes` / `FE`",
                      units  = "MtCO2/EJ") %>%
    calc_addVariable( "`Intensity|GDP|Final Energy`" = "`FE` / `GDP|MER`",
                      units  = "EJ/Billion$") %>%
    calc_addVariable( "`SE|Electricity|WindSolar`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`)",
                      "`SE|Electricity|Non-Bio RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`)",
                      "`FE|Heat|Non-Bio RE`" ="ifelse(is.na(`SE|Heat|Geothermal`), 0, `FE|Heat` * `SE|Heat|Geothermal` / `SE|Heat`) + ifelse(is.na(`FE|Geothermal`), 0,`FE|Geothermal`) + ifelse(is.na(`FE|Solar`), 0,`FE|Solar`)",
                      units  = "EJ/yr") %>%
    calc_addVariable( "`Share|H2|Electricity`" = "ifelse(is.na(`SE|Hydrogen|Electricity`), 0, `SE|Hydrogen|Electricity` / `SE|Hydrogen` * 100)"  ,
                      "`Share|Electricity|New RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`) / `SE|Electricity` * 100",
                      
                      "`FE|Electricity|Share`" = "`FE|Electricity` / `FE` * 100",
                      "`Share|FE|Hydrogen`" =  "ifelse(is.na(`FE|Hydrogen`), 0, `FE|Hydrogen`) / `FE` * 100",
                      "`Share|FE|ElecH2`" = "(`FE|Electricity` + `FE|Hydrogen` * `Share|H2|Electricity` /100) / `FE` * 100",
                      "`FE|Transport|Electricity|Share`" = "`FE|Transportation|Electricity` / `FE|Transportation` * 100",
                      "`FE|Industry|Electricity|Share`" = "`FE|Industry|Electricity` / `FE|Industry` * 100",
                      "`Share|FE|ElecH2|Industry`" = "( `FE|Industry|Electricity` + `FE|Hydrogen` * `Share|H2|Electricity`/100) / `FE|Industry` * 100",
                      "`FE|Buildings|Electricity|Share`" = "`FE|Residential and Commercial|Electricity` / `FE|Residential and Commercial` * 100",
                      
                      "`Share|Electricity|New RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`) / `SE|Electricity` * 100",
                      "`Share|PE|Fossil`" = "(`PE|Coal` + `PE|Gas` + `PE|Oil`) / `PE` * 100",
                      "`Share|PE|Non-Biomass Renewables`" = "`PE|Non-Biomass Renewables` / `PE` * 100",
                      
                      units  = "%") %>%
    
    calc_addVariable( "`Share|Electricity|Non-Bio RE`" = "`SE|Electricity|Non-Biomass Renewables` / `SE|Electricity` * 100",
                      "`Share|Electricity|New RE`" = "(`SE|Electricity|Wind` + `SE|Electricity|Solar`) / `SE|Electricity` * 100",
                      units  = "%") %>%
    
    calc_addVariable(  "`Share|FE|Non-Bio RE`" = "((`FE|Electricity`  +  ifelse(is.na(`FE|Hydrogen`),0,`FE|Hydrogen`)  * `Share|H2|Electricity`/100)  * `Share|Electricity|Non-Bio RE`/100 +  `FE|Heat|Non-Bio RE`  ) / `FE` * 100", units  = "%") %>%
    calc_addVariable("`Carbon Intensity|Electricity`" = "(`Emi|CO2|Energy|Supply|Electricity` + `Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity`) / (`SE|Electricity`)*3.6",
                     "`Carbon Intensity|Non-electric`" = "(`Emi|CO2|Energy` - `Emi|CO2|Energy|Supply|Electricity` + `Carbon Sequestration|CCS|Biomass` - `Carbon Sequestration|CCS|Biomass|Energy|Supply|Electricity`) / (`FE|Solids`   + `FE|Liquids` + `FE|Gases` + `FE|Hydrogen`)*3.6", units  = "tCO2/MWh")     %>% 
    calc_addVariable(    "`CDR3`" = "abs(`Carbon Sequestration|CCS|Biomass` + `Carbon Sequestration|Land Use`)" ,
                         units  = "EJ/yr") %>%
    calc_addVariable(    "`CDR`" = "abs(ifelse(is.na(`Carbon Sequestration|CCS|Biomass`), 0, `Carbon Sequestration|CCS|Biomass`) + ifelse(is.na(`Carbon Sequestration|Land Use`), 0, `Carbon Sequestration|Land Use`))" , units  = "EJ/yr") %>%
    calc_addVariable(    "`CDR2`" = "abs(sum(`Carbon Sequestration|CCS|Biomass`, `Carbon Sequestration|Land Use`, na.rm = TRUE))" , units  = "MtCO2/yr") %>%
    left_join(
      ar6meta.short, by = c("model", "scenario")) %>%
    factor.data.frame()
  
  ar6data.wmeta$variable = plyr::mapvalues(ar6data.wmeta$variable, from = c("Primary Energy|Biomass"), to  = c("PE|Biomass"))
  ar6data.wmeta = factor.data.frame(ar6data.wmeta)
  
  save( ar6data.wmeta, file = "df.ar6.rdata" )
} else {
  load( "df.ar6.rdata" )
}

ar6scencats = c("ar6-2C","ar6-1.5C")

elecshare_var <- c("FE|Electricity|Share" = "Total_Elec_Share", 
                   "FE|Transport|Electricity|Share" = "Tran_Elec_Share",
                   "FE|Industry|Electricity|Share" = "Indu_Elec_Share",
                   "FE|Buildings|Electricity|Share" = "Buil_Elec_Share")

ylabels <- c("Electricity Share in Final Energy [%]",
             "Transport Sector Electricity Share in Final Energy [%]",
             "Industry Sector Electricity Share in Final Energy [%]",
             "Buildings Sector Electricity Share in Final Energy [%]",
             NULL               )

df.pl.ar6 <- 
  ar6data.wmeta %>%
  filter(
    Category %in% c('C1','C2','C3'),
    variable %in% names(elecshare_var), 
    period %in% c(2005, seq(2010, 2060, 10))) %>% 
  mutate(scenar6 = case_when(Category %in% c('C1','C2') ~ "ar6-1.5C",
                             Category %in% c('C3') ~ "ar6-2C" )) %>%  
  factor.data.frame() 

df.pl.ar6.stat <- df.pl.ar6 %>%
  filter(variable %in% names(elecshare_var), period %in% c(2005, seq(2010, 2060, 10))) %>%
  group_by(period, variable, unit, scenar6) %>%
  calc_quantiles(probs = c(q0 = 0, q10= 0.10, q25 = 0.25, q50 = 0.5, 
                           q75 = 0.75, q90 = 0.9, q100 = 1)) %>%
  ungroup() %>%
  factor.data.frame() %>% 
  spread(key = quantile, value = value) %>% 
  order.levels(variable = names(elecshare_var))

df.plot.elecshares = filter(df0, variable %in% names(elecshare_var), period <= 2060 ) 

for (i in c(1:length(elecshare_var))){
  vari = names(elecshare_var)[[i]]
  file_name = elecshare_var[[i]]
  ylabel = ylabels[[i]]
  
p <- ggplot() +
  geom_ribbon(data = filter(df.pl.ar6.stat, variable == vari) , aes(x = period, ymin = q10, ymax = q90, fill = scenar6),  alpha = 0.25) +
  geom_ribbon(data = filter(df.pl.ar6.stat, variable == vari) , aes(x = period, ymin = q0, ymax = q100,fill = scenar6),  alpha = 0.1) +
  geom_line(data = filter(df.pl.ar6, variable == vari,scenar6 == "ar6-2C"  ), aes(x = period, y = value, group= interaction(model, scenario)), size = 0.3,  color = plotstyle("ar6-2C"), alpha = 0.2 )+
  geom_line(data = filter(df.pl.ar6, variable == vari,scenar6 == "ar6-1.5C"  ), aes(x = period, y = value, group= interaction(model, scenario)), size = 0.3,  color = plotstyle("ar6-1.5C"), alpha = 0.2 )+
  geom_line(data = df.hist.plot.elecshare, aes(x = period, y = value), size = 1,  color = "black") + 
  geom_line(data = filter(df.plot.elecshares, variable == vari), aes(x = period, y = value, color = scenario), size = 1, alpha = 1) + 
  geom_point(data = filter(df.plot.elecshares, variable == vari), aes(x = period, y = value, color = scenario), size = 1.2) +
  scale_fill_manual( values = plotstyle(levels(df.pl.ar6$scenar6)),
                     labels = plotstyle(levels(df.pl.ar6$scenar6), out = "legend"), name = "") +
  labs(y = ylabel, x = "") +
    scale_x_continuous(limits = c(1995, 2060), breaks = c(2000, 2020, 2040, 2060),
                     minor_breaks = unique(seq(2000, 2060,10))) +
  theme_bw(base_size = 7) +
  theme(legend.key.width = unit(1.5,"cm"))

ggsave(filename = paste0(plot.dir, "/", file_name, "Elecshares.png"),width=11, height=6, units = "cm")
}

p <- ggplot() +
  geom_ribbon(data = filter(df.pl.ar6.stat) , aes(x = period, ymin = q10, ymax = q90, fill = scenar6),  alpha = 0.25) +
  geom_ribbon(data = filter(df.pl.ar6.stat) , aes(x = period, ymin = q0, ymax = q100,fill = scenar6),  alpha = 0.1) +
  geom_line(data = filter(df.pl.ar6,scenar6 == "ar6-2C"  ), aes(x = period, y = value, group= interaction(model, scenario)), size = 0.3,  color = plotstyle("ar6-2C"), alpha = 0.2 )+
  geom_line(data = filter(df.pl.ar6,scenar6 == "ar6-1.5C"  ), aes(x = period, y = value, group= interaction(model, scenario)), size = 0.3,  color = plotstyle("ar6-1.5C"), alpha = 0.2 )+
  geom_line(data = df.hist.plot.elecshare, aes(x = period, y = value), size = 1,  color = "black") +
  geom_line(data = filter(df.plot.elecshares), aes(x = period, y = value, color = scenario), size = 1, alpha = 1) +
  geom_point(data = filter(df.plot.elecshares), aes(x = period, y = value, color = scenario), size = 1.2) +
  scale_fill_manual( values = plotstyle(levels(df.pl.ar6$scenar6)),
                     labels = plotstyle(levels(df.pl.ar6$scenar6), out = "legend"), name = "") +
  labs(y = ylabels[[1]], x = "") +
  scale_x_continuous(limits = c(1995, 2060), breaks = c(2000, 2020, 2040, 2060),
                     minor_breaks = unique(seq(2000, 2060,10))) +
  theme_bw(base_size = 7) +
  theme(legend.key.width = unit(1.5,"cm"))+
  facet_wrap(~variable)

ggsave(filename = paste0(plot.dir, "/Elecshares_panels.png"),width=11, height=6, units = "cm")

##################### discrete AR 6 indicators ###########################
tt <- plotstyle.add("ar6-1.5C", "ar6-1.5C", "#009E73", replace=T)
tt <- plotstyle.add("ar6-2C", "ar6-2C", "#882288", replace=T)

climscens = 
  c(  "1.5C",
      "2C",
      NULL)

indicators_a = c(  "GDP|per capita|MER", 
                "Intensity|Final Energy|CO2", 
                "Intensity|GDP|Final Energy",
                "Share|FE|Non-Bio RE", 
                "Share|Electricity|New RE", 
                "FE",
                "Share|PE|Fossil",
                "PE|Biomass",
                "CDR",
                # "Share|PE|Fossil",
                "Price|Carbon",
                "Carbon Sequestration|CCS|Biomass",
                "Emi|CO2|CDR|BECCS",
                NULL)

df.pl.ar6.test <- ar6data.wmeta %>%
  filter(variable == "Intensity|GDP|Final Energy")

plot_ind_t = 2050

df.pl.ar6 <- ar6data.wmeta %>%
  filter(Category %in% c('C1', 'C2', 'C3', 'C4'),
         variable %in% indicators_a, 
         period == plot_ind_t) %>% 
  # TODO: merge to model.scen variable
  unite(model.scen, model, scenario, remove = F) %>% 
  mutate(scencat=case_when(
    Category %in% c("C1", "C2") ~ climscens[1],
    Category %in% c("C3", "C4") ~ climscens[2]),
         value = case_when(variable %in% c("Carbon Sequestration|CCS|Biomass", "CDR") ~ value/1000,
                           TRUE ~ value),
    value = case_when(variable %in% c("Intensity|GDP|Final Energy") ~ value*1000,
                      TRUE ~ value)) %>%
  order.levels(variable = indicators_a) %>%       
  factor.data.frame() 

df.pl.ar6.test2 <- df.pl.ar6 %>%
  filter(variable == "Intensity|GDP|Final Energy")

df.plot = filter(df0, variable %in% indicators_a, period == plot_ind_t) %>%   
  mutate(value = case_when(variable %in% c("Carbon Management|Carbon Capture", "CDR") ~ value/1000,
           TRUE ~ value),
         scencat = strtrim(scenario, 4)) %>%
  factor.data.frame()

df.pl.ar6_a <- df.pl.ar6 %>% 
  filter(variable %in% c("Share|Electricity|New RE", "Share|FE|Non-Bio RE")) %>% 
  filter(value < 100)

df.pl.ar6_b <- df.pl.ar6 %>% 
  filter(variable %in% c("Price|Carbon")) %>% 
  filter(value < 2000)

df.pl.ar6_non <- df.pl.ar6 %>% 
  filter(!variable %in% c("Share|Electricity|New RE", "Share|FE|Non-Bio RE","Price|Carbon"))

df.pl.ar6 <- list(df.pl.ar6_a, df.pl.ar6_b, df.pl.ar6_non) %>% 
  reduce(full_join) 

 p <- ggplot()+
  geom_violin(data=df.pl.ar6, aes(x=scencat, y=value, fill=scencat), alpha=0.6)+
  geom_point(data = df.plot, aes( x = scencat, y = value, fill = scencat), stat = "identity", size = 2, shape = 23, linewidth = 2,  alpha = 0.7, color = "black")  +
  facet_wrap(
    vars(forcats::fct_recode(variable,  
                             "GDP MER per capita \n[Thousand$2020]" = "GDP|per capita|MER",
                             "Energy Intensity\n[EJ/Billion$2020]" = "Intensity|GDP|Final Energy",
                             "Carbon Intensity\n[tCO2/EJ]" = "Intensity|Final Energy|CO2",
                             "Electricity\nin FE [%]"= "Share|FE|Electricity",
                             "Wind/Solar \nin Elec. [%]" = "Share|Electricity|New RE", 
                             "VRE\n[EJ]" = "SE|Electricity|WindSolar"  ,  
                             "BECCS\n[GtCO2]" = "Carbon Sequestration|CCS|Biomass",
                             "CDR\n[GtCO2]" = "CDR",
                             "CO2 Price\n[$2020/tCO2]" = "Price|Carbon",
                             "Non-Bio RE\nin FE [%]" = "Share|FE|Non-Bio RE",
                             "Final\nEnergy [EJ]" = "FE",
                             "Biomass\n[EJ]" = "PE|Biomass",
                             "Fossils\nin PE [%]"= "Share|PE|Fossil") ), nrow = 2, scales = "free") +
  ylim(0,NA) +
   scale_fill_manual( values = plotstyle(levels(df.pl.ar6$scenar6)),
                      labels = plotstyle(levels(df.pl.ar6$scenar6), out = "legend"), name = "") +
  scale_color_manual( values = plotstyle(climscens),
  labels = plotstyle(climscens, out = "legend"), name = "") +
   # scale_fill_manual(name = "scencat", values = scenario_color_mapping) + 
  theme_bw(base_size = 8) +
  theme(
    # axis.text.x = element_text(size = 6.5),
    axis.text.x = element_blank(),
    legend.position="none",
    strip.background = element_blank())

# print(p)

ggsave(filename = paste0(plot.dir, "/ar6comp_Panel2.png"),width=25, height=14, units = "cm")


##############################################################################
# 
#   i = 1 
#   scenarios_mapping <- scenario_group_toplot[[i]]
#   scenario_color_mapping <- scenario_group_color[[i]]
#   scenario_color_mapping.whist <- c(scenario_color_mapping, "historical" = "#000000")
#   scenario_group_name <- scenario_group_names[[i]]
#   
#   data.files = lapply(names(scenarios_mapping), function(x) paste0( data.dir, "/", x, "Transport.mif")) %>% unlist 
#   
#   run_indices = seq(from = 1, to = length(data.files), by = 1)
#   df.edge = NULL 
#   
#   # trim df to chosen scenarios and regions
#   for (i in run_indices){
#     REMIND.data = read.quitte(data.files[[i]])
#     df.edge <- rbind(df.edge, REMIND.data)
#   }
# 
# df.edge0 = filter( df.edge, scenario %in% scens, region %in% reg ) %>%
#   order.levels(scenario = scens) %>%
#   revalue.levels(scenario = scenarios_mapping) %>%
#   factor.data.frame()
# 
# # try for one scenario
# df.china = filter( df.edge, region %in% c("CHA") ) %>%
#   order.levels(scenario = scens) %>%
#   revalue.levels(scenario = scenarios_mapping) %>%
#   factor.data.frame()
# 
# df_test = filter(df.china, variable == "EInt|Transport|VKM|Pass|Road|LDV")
# 
# FE_LDV.bd <- c( "FE|Transport|Pass|Road|LDV|Electricity",
#                 "FE|Transport|Pass|Road|LDV|Gases",
#                 "FE|Transport|Pass|Road|LDV|Hydrogen",
#                 "FE|Transport|Pass|Road|LDV|Liquids|Fossil",
#                 "FE|Transport|Pass|Road|LDV|Liquids|Biomass",
#                 "FE|Transport|Pass|Road|LDV|Liquids|Hydrogen")
# 
# FE_LDV.tot <- c("FE|Transport|Pass|Road|LDV")
# 
# ES_LDV <-c("ES|Transport|VKM|Pass|Road|LDV|BEV", "ES|Transport|VKM|Pass|Road|LDV|Liquids")
# 
# FE_LDV <- c(FE_LDV.bd, FE_LDV.tot)
# 
# vars <- c(FE_LDV, ES_LDV)
# 
# df.china.LDV <- df.china %>% 
#   filter(variable %in% vars) 
# 
# df.china.EInt_LDV <- df.china.LDV %>% 
#   #FE(EJ/yr) / ES(bn vkm/yr) = 1e12/1e9 = 1e3
#   calc_addVariable( "`EInt|Transport|VKM|Pass|Road|LDV|BEV`" = "`FE|Transport|Pass|Road|LDV|Electricity`/ `ES|Transport|VKM|Pass|Road|LDV|BEV` * 1000", units  = "MJ/vkm") %>% 
#   calc_addVariable( "`EInt|Transport|VKM|Pass|Road|LDV|ICE`" = "`FE|Transport|Pass|Road|LDV|Liquids|Fossil`/ `ES|Transport|VKM|Pass|Road|LDV|Liquids` * 1000", units  = "MJ/vkm")
# 
# df.china.EmiInt_LDV <- df.china.EInt_LDV %>% 
#   # MJ/vkm * MWh/MJ * diesel emission factor = 0.266 tCO2/MWh
#   calc_addVariable( "`EmiInt|Transport|VKM|Pass|Road|LDV|ICE`" = "`EInt|Transport|VKM|Pass|Road|LDV|ICE` 
#                     * 0.278 / 1e3 
#                     * 0.266 * 1e3
#                     ", units  = "kgCO2/vkm") 
# 
# df.ci.elec <- df.ci %>% 
#   filter(variable == "Carbon Intensity|Electricity")
# 
# df.china.EmiInt_LDV2  <- list(df.china.EmiInt_LDV, df.ci.elec) %>% 
#       reduce(full_join) %>% 
#       # MJ/vkm -> MWh/vkm * carbon intensity of electricity tCO2/MWh
#       calc_addVariable( "`EmiInt|Transport|VKM|Pass|Road|LDV|BEV`" = "`EInt|Transport|VKM|Pass|Road|LDV|BEV`                     * 0.278 / 1e3 
#                     * `Carbon Intensity|Electricity` 
#                     ", units  = "kgCO2/vkm") 
# 
# emiInt_var <- c("EmiInt|Transport|VKM|Pass|Road|LDV|ICE" = "Emission Intensity of ICE LDV", 
#                 "EmiInt|Transport|VKM|Pass|Road|LDV|BEV" = "Emission Intensity of BEV LDV"
#                 )
# 
# df.china.EmiInt_LDV3 <- df.china.EmiInt_LDV2 %>% 
#   filter(variable %in% c("EmiInt|Transport|VKM|Pass|Road|LDV|ICE", "EmiInt|Transport|VKM|Pass|Road|LDV|BEV") ) %>% 
#   revalue.levels(variable = emiInt_var) %>% 
#   select(-unit)
#   
# df.china.ci <- df.china.EmiInt_LDV2 %>% 
#   filter(variable %in% c("Carbon Intensity|Electricity"))
#   
# df.china.ci.plot <-  spread(df.china.ci, variable,value) %>% 
#   dplyr::rename( ci = `Carbon Intensity|Electricity` )%>% 
#   select(-unit)
# 
# df.china.EmiInt_LDV.plot <- list(df.china.EmiInt_LDV3, df.china.ci.plot) %>% 
#   reduce(full_join) %>% 
#   filter(period < 2050 & period > 2020)
# 
# linetype.map <- c("Emission Intensity of ICE LDV" = 'solid', 
#                   "Emission Intensity of BEV LDV" = 'dashed')
# 
# p <- ggplot() +
#   geom_line(data = df.china.EmiInt_LDV.plot, aes(x = ci, y = value, color = scenario, linetype=variable)) +
#   scale_size_manual( values = c(1,1.7)) +
#   geom_text(data = df.china.EmiInt_LDV.plot %>% 
#               # filter(scenario !="2C default") %>% 
#               filter(scenario %in% c(scenarios_mapping[[4]])), aes(x = ci, y = value, color = "black", label=period), size =1.5, alpha = 1, hjust=1, vjust=1) +
#   geom_point(data = df.china.EmiInt_LDV.plot, aes(x = ci, y = value, color = scenario, label=period), size = 0.7, alpha = 0.5) +
#   ylab("CO2 Emission Intensity [kgCO2/vkm]") +
#   xlab("Carbon intensity of electricity mix [gCO2/kWh]") +
#   theme_bw(base_size = 7) +
#   scale_linetype_manual(name = "", values = linetype.map) +
#   scale_color_manual(name = "Scenarios", values = scenario_color_mapping) + 
#   theme(legend.key.width = unit(1,"cm")) +
#   scale_x_reverse()
# 
# ggsave(filename = paste0(plot.dir, "/", scenario_group_name, "_LDV_EmiInt.png"), width=11, height=6, units = "cm")

####---------------------------------------------------------------
####---------------------------------------------------------------
####---------------------------------------------------------------
# df.china.FE_LDV.tot2 <- df.china.FE_LDV.tot %>%  
#   calc_addVariable( "`FE|Electricity|Share|LDV`" = "`FE|Transport|Pass|Road|LDV|Electricity`/ `FE|Transport|Pass|Road|LDV` * 100", units  = "%") %>% 
#   calc_addVariable( "`FE|Diesel|Share|LDV`" = "`FE|Transport|Pass|Road|LDV|Liquids|Fossil`/ `FE|Transport|Pass|Road|LDV` * 100", units  = "%")

# df.china.FE_LDV.shares <- df.china.FE_LDV.tot2 %>% 
#   filter(variable %in% c("FE|Electricity|Share|LDV", "FE|Diesel|Share|LDV")) 

# df.LDV.EInt = filter(df.china, variable == "EInt|Transport|VKM|Pass|Road|LDV")

# df.china.EInt_LDV.shares <- list(df.china.FE_LDV.shares, df.LDV.EInt) %>% 
#     reduce(full_join) %>%  
#   calc_addVariable( "`EInt|Transport|VKM|Pass|Road|LDV|EV`" = "`EInt|Transport|VKM|Pass|Road|LDV` * `FE|Electricity|Share|LDV`", units  = "MJ/vkm") %>% 
#   calc_addVariable( "`EInt|Transport|VKM|Pass|Road|LDV|ICE`" = "`EInt|Transport|VKM|Pass|Road|LDV` * `FE|Diesel|Share|LDV`", units  = "MJ/vkm")

# df.china.EmiInt_LDV <- df.china.LDV %>% 
#   calc_addVariable( "`EmiInt|Transport|VKM|Pass|Road|LDV|ICE`" = "`EInt|Transport|VKM|Pass|Road|LDV|ICE` 
#                     * 3.66 / 1e3 
#                     * 0.266 
#                     ", units  = "tCO2/vkm") 


# plot_ind_t = 2050
# 
# df.plot.ind = filter(df0, variable %in% indicators,
#                      period == plot_ind_t ) %>% 
# mutate(value = case_when(variable %in% c("Carbon Management|Carbon Capture", "CDR") ~ value/1000,
#                          TRUE ~ value),
#        scenario = strtrim(scenario, 4)) %>%
#   factor.data.frame()
# 
# climscens = 
#   c(  "1.5C",
#       "2C",
#       NULL)
# 
# df.pl.ar6 <- ar6data.wmeta %>%
#   filter(variable %in% indicators,
#          period == plot_ind_t,
#          Category %in% c('C1','C2','C3')) %>% 
#   unite(model.scen, model, scenario, remove = F) %>% 
#   mutate(scenar6 = case_when(Category %in% c('C1','C2') ~ "ar6-1.5C",
#                              Category %in% c('C3') ~ "ar6-2C" )) %>% 
#   mutate(value = case_when(variable %in% c("Carbon Sequestration|CCS", "CDR") ~ value/1000,
#                            TRUE ~ value)) %>% 
#   factor.data.frame() 
# 
# df.pl.ar6_a <- df.pl.ar6 %>% 
#   filter(variable %in% c("Share|Electricity|New RE", "Share|FE|Non-Bio RE")) %>% 
#   filter(value < 100)
# 
# df.pl.ar6_b <- df.pl.ar6 %>% 
#   filter(variable %in% c("Price|Carbon")) %>% 
#   filter(value < 2000)
# 
# df.pl.ar6_non <- df.pl.ar6 %>% 
#   filter(!variable %in% c("Share|Electricity|New RE", "Share|FE|Non-Bio RE","Price|Carbon"))
# 
# df.pl.ar6 <- list(df.pl.ar6_a, df.pl.ar6_b, df.pl.ar6_non) %>% 
#   reduce(full_join) 
# 
# p <- plot.discreteX.crossbar.points.ref(df.pl.ar6 %>% 
#       select(model, scenario,  variable, unit, value, period) ,
#       x.dim = "scenario", shape.dim = "model", color.dim = "scenario" ,
#       vari.ylab = "", facet.dim = "variable", marker.size = 0.5, marker.alpha = 0.6,  confidence = 0.8, reference = F) +
#   geom_point(data = df.plot.ind , aes( x = scenario, y = value, fill = scenario), stat = "identity", size = 2, shape = 23, linewidth = 2, alpha = 0.7, color = "black")  +
#   facet_wrap(vars(forcats::fct_recode(variable,  
#                              "Electricity\nin FE [%]"= "Share|FE|Electricity",
#                              "Wind/Solar \nin Elec. [%]" = "Share|Electricity|New RE", 
#                              "VRE\n[EJ]" = "SE|Electricity|WindSolar"  ,  
#                              "CCS\n[GtCO2]" = "Carbon Sequestration|CCS",
#                              "CDR\n[GtCO2]" = "CDR",
#                              "CO2 Price\n[$2020/tCO2]" = "Price|Carbon",
#                              
#                              "Non-Bio RE\nin FE [%]" = "Share|FE|Non-Bio RE",
#                              
#                              "Final\nEnergy [EJ]" = "FE",
#                              "Biomass\n[EJ]" = "PE|Biomass",
#                              "Fossils\nin PE [%]"= "Share|PE|Fossil") ), nrow = 4, scales = "free") +
#   ylim(0,NA) +
#   scale_fill_manual( values = plotstyle(climscens),
#                      labels = plotstyle(climscens, out = "legend"), name = "") +
#   scale_color_manual( values = plotstyle(climscens),
#                       labels = plotstyle(climscens, out = "legend"), name = "") +
#   theme_bw(base_size = 8) +
#   theme(
#     axis.text.x = element_blank(),
#     legend.position="none",
#     strip.background = element_blank())
# 
# ggsave(filename = paste0(plot.dir, "/ar6comp_Panel2.png"),width=20, height=20, units = "cm")


# Panel d shows population, GDP per person, emission indicators by region in 2019 for percentage GHG
# contributions, total GHG per person, and total GHG emissions intensity,