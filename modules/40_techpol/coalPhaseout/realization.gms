*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/40_techpol/coalPhaseout.gms

*####################### R SECTION START (PHASES) ##############################
$Ifi "%phase%" == "declarations" $include "./modules/40_techpol/coalPhaseout/declarations.gms"
$Ifi "%phase%" == "equations" $include "./modules/40_techpol/coalPhaseout/equations.gms"
$Ifi "%phase%" == "bounds" $include "./modules/40_techpol/coalPhaseout/bounds.gms"
*######################## R SECTION END (PHASES) ###############################
*** EOF ./modules/40_techpol/coalPhaseout.gms
