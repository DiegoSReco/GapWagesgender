## Packages ----
library(pacman)
p_load('tidyverse', 
        "survey",
        "glue")
#For data download use the function enoe_load() from script_package_load in my repository LaborAnalysis_ENOE
#The function takes two arguments the year and the quarter
df_enoe <-  enoe_load(2023, 4)
#Cleaning and filtering data
df_enoe_f <-  df_enoe |>  
                filter(r_def==0 & (c_res ==1 | c_res ==3) & (eda >= 15 &  eda<=29))
  #Rename Variables 
  rename_dict <- c(
    "clase1" = "clas_PEA",
    "clase2" = "clas_PO",
    "emp_ppal" = "clas_INF",
    "cs_p13_2" = "años_fin",
    "cs_p13_1" = "grado_educa",
    "sex" = "sexo",
    "eda" = "edad",
    "n_hij" = "hijos",
    "hrsocup" = "n_hrs",
    "ingocup" = "ing_men",
    "ing_x_hrs" = "ing_hrs",
    "ambito2" = "dim_emp",
    "rama" = "clas_sec",
    "scian" = "clas_ind",
    "t_loc" = "t_loc",
    "rama_est2" = "eco_sec", 
    "ent" = "entidad")
  colnames(df_enoe_f) <- rename_dict[match(colnames(df_enoe_f), 
                                                   names(rename_dict))]
  #Feauturing 
  df_enoe_f <- df_enoe_f |>  
    mutate(clas_informal = case_when(clas_INF == 1 ~ "Informal", 
                                     clas_INF == 2 ~ "Formal",
                                     TRUE   ~ NA), 
           clas_PEA_PNEA = case_when(clas_PEA ==1 ~ 'PEA', 
                                     clas_PEA == 2 ~ 'PNEA'),
           clas_ocupado = case_when(
            clas_PO == 1 ~ 'ocupados' , 
            clas_PO == 2 ~ 'desocupados',
            clas_PO == 3 ~ 'disponibles', 
            clas_PO == 4 ~ 'no disponibles',
            TRUE ~ NA),
           clas_subocup = sub_o,
            sex = case_when(sexo == 1 ~ "Hombre", 
                           sexo == 2 ~ "Mujer", 
                           TRUE ~ NA ),
           clas_pea_potencial = if_else(clas_PEA == 1 | (clas_PEA ==2 & clas_PO == 3), 1,0),
           clas_desocup_disfrazado = if_else(clas_PEA == 2 & clas_PO == 3,1, 0),
           entidad_ab = case_when(
             entidad == 1 ~ "AGS",
             entidad == 2 ~ "BC",
             entidad == 3 ~ "BCS",
             entidad == 4 ~ "CAMP",
             entidad == 5 ~ "COAH",
             entidad == 6 ~ "COL",
             entidad == 7 ~ "CHIS",
             entidad == 8 ~ "CHIH",
             entidad == 9 ~ "CDMX",
             entidad == 10 ~ "DGO",
             entidad == 11 ~ "GTO",
             entidad == 12 ~ "GRO",
             entidad == 13 ~ "HGO",
             entidad == 14 ~ "JAL",
             entidad == 15 ~ "MEX",
             entidad == 16 ~ "MICH",
             entidad == 17 ~ "MOR",
             entidad == 18 ~ "NAY",
             entidad == 19 ~ "NL",
             entidad == 20 ~ "OAX",
             entidad == 21 ~ "PUE",
             entidad == 22 ~ "QRO",
             entidad == 23 ~ "QR",
             entidad == 24 ~ "SLP",
             entidad == 25 ~ "SIN",
             entidad == 26 ~ "SON",
             entidad == 27 ~ "TAB",
             entidad == 28 ~ "TAMS",
             entidad == 29 ~ "TLAX",
             entidad == 30 ~ "VER",
             entidad == 31 ~ "YUC",
             entidad == 32 ~ "ZAC",
             TRUE ~ "Desconocido"  
           )) |>  
  filter(clas_ocupado ==1 &  n_hrs!=0 & ing_men!=0 )

#Probabilistic survey design in R
list_colnames  <- colnames(df_enoe_2023_t2)
factor <- list_colnames[grepl("(?i)fac",  list_colnames)][1] 
df_s_design <- svydesign(id = ~upm,
                         strata = ~est_d_tri,
                         weights = as.formula(paste0("~",factor)),
                         data = df_enoe_2023_t2_f , 
                         nest = TRUE)
#Estimations 
means_table_entidad_sex <- svyby(~ing_men + n_hrs,
                             by = ~ entidad_ab + sex, 
                             design = df_s_design, 
                             FUN = svymean,
                             na.rm = TRUE,
                             ci = TRUE)


  
