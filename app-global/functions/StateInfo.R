
StateInfo <- function(region_dat,
                      input_selected_eez,
                      is_hs = F){
  
  if(is_hs){
    
    eez_dat <- region_dat$eezs %>%
      dplyr::filter(zone == input_selected_eez)
    
    info_out <- paste0(
      "<h4>", unique(eez_dat$title), "</h3>",
      "Description: ", unique(eez_dat$description), ", high seas area only"
    ) %>%
      lapply(htmltools::HTML)
    
  }else{
    
  # Filter eez data
  eez_dat <- region_dat$eezs %>% # load this in up above
    dplyr::filter(eez_ter_iso3 == input_selected_eez)
  
  # Filter link data
  link_dat <- region_dat$links %>%
    dplyr::filter(eez_ter_iso3 == input_selected_eez)
  
  if(nrow(link_dat) > 0){
    
    # FAO country profile
    if(!is.na(link_dat$fao_country_profile)){
      
      fao_links_split <- unlist(str_split(link_dat$fao_country_profile, "; "))
        
      fao_country_profile <- paste0(
        "<br>",
        "<a href='", fao_links_split, "' target='_blank'>", 
        "FAO country profile", "</a>")
        

    }else{
      
      fao_country_profile <- ""
    }
    
    # Fisheries subsidies
    if(!is.na(link_dat$fisheries_subsidies)){
      
      fisheries_subsidies_split <- unlist(str_split(link_dat$fisheries_subsidies, "; "))
      
      if(length(fisheries_subsidies_split) > 1){
        
        fisheries_subsidies_names_split <- unlist(str_split(link_dat$fisheries_subsidies_names, "; "))
        
        fisheries_subsidies <- paste0("<br>", "Fisheries subsidies: ", 
                                    paste0("<a href='", 
                                           fisheries_subsidies_split, 
                                           "' target='_blank'>", 
                                           fisheries_subsidies_names_split, "</a>", collapse = " | "))
        
      }else{
        
        fisheries_subsidies <- paste0("<br>",
                                    "<a href='", fisheries_subsidies_split, "' target='_blank'>", "Fisheries subsidies", "</a>")
        
      }

    }else{
      
      fisheries_subsidies <- ""
    }
    
    # Access agreements
    if(!is.na(link_dat$fishing_access_agreements)){
      
      access_agreements_split <- unlist(str_split(link_dat$fishing_access_agreements, "; "))
      
      if(length(access_agreements_split) > 1){

        access_agreements_names_split <- unlist(str_split(link_dat$fishing_access_agreement_names, "; "))
        
        access_agreements <- paste0("<br>", "Internal fishing access agreements: ", 
                                    paste0("<a href='", 
                                           access_agreements_split, 
                                           "' target='_blank'>", 
                                           access_agreements_names_split, "</a>", collapse = " | "))
        
      }else{
      
      access_agreements <- paste0("<br>",
        "<a href='", access_agreements_split, "' target='_blank'>", "Internal fishing access agreements", "</a>")
      
      }

    }else{
      
      access_agreements <- ""
    }
    
  }else{
    
    fao_country_profile <- ""
    fisheries_subsidies <- ""
    access_agreements <- ""
    
  }
  
  ### Combine into country profile/summary of DW fishing
  info_out <- paste0(
    "<h4>", unique(eez_dat$eez_ter_name), "</h3>",
    "Sovereign state: ", unique(eez_dat$eez_sov_name),
    fao_country_profile,
    fisheries_subsidies,
    access_agreements
  ) %>%
    lapply(htmltools::HTML)
    
    
  } # close !is_hs
  
  info_out

}


# # Filter and format Country profile data
# ACP_codes_links <- ACP_codes %>%
#   dplyr::filter(territory_iso3 == input$africa_eez_select)
# 
# ACP_fao_membership <- ACP_codes_links %>%
#   separate_rows(fao_memberships, sep = ",")
# 
# RFMO_links_eez <- RFMO_links %>%
#   dplyr::filter(rfmo_abbr %in% ACP_fao_membership$fao_memberships)
# 
# ### Make HTML sections with links for each type of information
# # Fisheries management agency
# fisheries_mgmt_agency <- ifelse(
#   length(unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)])) > 0,
#   # Create link if we have one
#   paste0("<a href='", unique(ACP_codes_links$fishery_org_link[!is.na(ACP_codes_links$fishery_org_link)]), "' target='_blank'>", ACP_codes_links$fishery_org_eng, "</a>"),
#   # Otherwise just paste name of the agency
#   paste0(ACP_codes_links$fishery_org_eng)
# )
# 
# # FAO country profile
# country_profiles <- paste0(
#   # FAO
#   "<a href='", unique(ACP_codes_links$fao_country_profile[!is.na(ACP_codes_links$fao_country_profile)]), "' target='_blank'>", "FAO", "</a>", " | ",
#   # World Bank
#   ifelse(
#     length(unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)])) > 0,
#     paste0("<a href='", unique(ACP_codes_links$world_bank_profile[!is.na(ACP_codes_links$world_bank_profile)]), "' target='_blank'>", "World Bank", "</a>"),
#     "World Bank"
#   ), " | ",
#   # UN
#   ifelse(
#     length(unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)])) > 0,
#     paste0("<a href='", unique(ACP_codes_links$UN_profile[!is.na(ACP_codes_links$UN_profile)]), "' target='_blank'>", "United Nations", "</a>"),
#     "United Nations"
#   )
# ) # close paste
# 
# 
# # Treaties and Conventions
# treaties_conventions <- paste0(
#   "<a href= '",
#   unique(ACP_codes_links$treaties_conventions[!is.na(ACP_codes_links$treaties_conventions)]),
#   "' target='_blank'>",
#   unique(ACP_codes_links$territory[!is.na(ACP_codes_links$treaties_conventions)]),
#   "</a>",
#   collapse = " | "
# )
# 
# # Foreign access agreements by EEZ sector
# foreign_access_agreements <- paste0(
#   "<a href= '", 
#   unique(ACP_codes_links$internal_fishing_access_agreements[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
#   "' target='_blank'>", 
#   unique(ACP_codes_links$territory[!is.na(ACP_codes_links$internal_fishing_access_agreements)]), 
#   "</a>", 
#   collapse = " | ")
# 
# # FAO Regional Fisheries Body Memberships
# regional_body_memberships <- paste0(
#   "<a href= '", 
#   unique(RFMO_links_eez$link[!is.na(RFMO_links_eez$link)]),
#   "' target='_blank'>",
#   unique(RFMO_links_eez$rfmo_name[!is.na(RFMO_links_eez$link)]),
#   "</a>",
#   collapse = " | ")
  
  