# African Regional Groupings

# All African Countries

african_countries <- data.frame(
  Country = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", 
              "Burundi", "Cape Verde", "Cameroon", "Central African Republic", 
              "Chad", "Comoros", "Congo", "Democratic Republic of Congo", 
              "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", 
              "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", 
              "Guinea", "Guinea-Bissau", "Ivory Coast", "Kenya", 
              "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", 
              "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", 
              "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", 
              "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", 
              "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", 
              "Uganda", "Zambia", "Zimbabwe"),
  
  ISO2 = c("DZ", "AO", "BJ", "BW", "BF", 
           "BI", "CV", "CM", "CF", 
           "TD", "KM", "CG", "CD", 
           "DJ", "EG", "GQ", "ER", 
           "SZ", "ET", "GA", "GM", "GH", 
           "GN", "GW", "CI", "KE", 
           "LS", "LR", "LY", "MG", "MW", 
           "ML", "MR", "MU", "MA", "MZ", 
           "NA", "NE", "NG", "RW", "ST", 
           "SN", "SC", "SL", "SO", "ZA", 
           "SS", "SD", "TZ", "TG", "TN", 
           "UG", "ZM", "ZW"),
  
  ISO3 = c("DZA", "AGO", "BEN", "BWA", "BFA", 
           "BDI", "CPV", "CMR", "CAF", 
           "TCD", "COM", "COG", "COD", 
           "DJI", "EGY", "GNQ", "ERI", 
           "SWZ", "ETH", "GAB", "GMB", "GHA", 
           "GIN", "GNB", "CIV", "KEN", 
           "LSO", "LBR", "LBY", "MDG", "MWI", 
           "MLI", "MRT", "MUS", "MAR", "MOZ", 
           "NAM", "NER", "NGA", "RWA", "STP", 
           "SEN", "SYC", "SLE", "SOM", "ZAF", 
           "SSD", "SDN", "TZA", "TGO", "TUN", 
           "UGA", "ZMB", "ZWE")
)

# Display the dataframe
print(african_countries)

# Arab Maghreb Union (AMU)
amu <- c("Algeria", "Libya", "Mauritania", "Morocco", "Tunisia")

# Common Market for Eastern and Southern Africa (COMESA)
comesa <- c("Burundi", "Comoros", "Democratic Republic of the Congo", 
            "Djibouti", "Egypt", "Eritrea", "Eswatini", "Ethiopia", 
            "Kenya", "Libya", "Madagascar", "Malawi", "Mauritius", 
            "Rwanda", "Seychelles", "Somalia", "Sudan", "Tunisia", 
            "Uganda", "Zambia", "Zimbabwe")

# Community of Sahel-Saharan States (CEN-SAD)
cen_sad <- c("Benin", "Burkina Faso", "Central African Republic", "Chad", 
             "Comoros", "Djibouti", "Egypt", "Eritrea", "Gambia", "Ghana", 
             "Guinea", "Guinea-Bissau", "Ivory Coast", "Libya", "Mali", 
             "Mauritania", "Morocco", "Niger", "Nigeria", "Senegal", 
             "Sierra Leone", "Somalia", "Sudan", "Togo", "Tunisia")

# East African Community (EAC)
eac <- c("Burundi", "Democratic Republic of the Congo", "Kenya", 
         "Rwanda", "Somalia", "South Sudan", "Tanzania", "Uganda")

# Economic Community of Central African States (ECCAS/CEEAC)
eccas <- c("Angola", "Burundi", "Cameroon", "Central African Republic", "Chad", 
           "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", 
           "Republic of Congo", "Sao Tome and Principe")

# Economic Community of West African States (ECOWAS) - All (2025)
ecowas <- c("Benin", "Cape Verde", "Ivory Coast", "The Gambia", 
            "Ghana", "Guinea", "Guinea-Bissau", "Liberia", 
            "Nigeria", "Senegal", "Sierra Leone", "Togo")

# Economic Community of West African States (ECOWAS) - Francophone (2025)
ecowas_francophone <- c("Benin", "Ivory Coast", "Guinea", 
                        "Senegal", "Togo")

# Economic Community of West African States (ECOWAS) - Anglophone (2025)
ecowas_anglophone <- c("Gambia", "Ghana", "Liberia", 
                       "Nigeria", "Sierra Leone")

# Economic Community of West African States (ECOWAS) - Lusophone (2025)
ecowas_lusophone <- c("Cape Verde", "Guinea-Bissau")

# The Alliance of Sahel States (AES)
aes <- c("Burkina Faso", "Mali", "Niger")
aes_df <- africa |> filter(admin %in% aes)

# Intergovernmental Authority on Development (IGAD)
igad <- c("Djibouti", "Ethiopia", "Kenya", 
          "Somalia", "South Sudan", "Sudan", "Uganda")

# Southern African Development Community (SADC)
sadc <- c("Angola", "Botswana", "Comoros", "Democratic Republic of the Congo", 
          "Eswatini", "Lesotho", "Madagascar", "Malawi", "Mauritius", 
          "Mozambique", "Namibia", "Seychelles", "South Africa", "Tanzania", 
          "Zambia", "Zimbabwe")
