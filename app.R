library(shiny)
library(bs4Dash)
library(ciTools)
library(lubridate)
library(vitaltable)
library(leaflet)
library(dplyr)
library(shinythemes)
library(janitor)
library(plotly)
library(reshape2)
library(forcats)
library(sf)
library(DT)
library(tidyr)
library(haven)

dados::pinguins

load('dados/base_linkada_anon_female_2019_2021_14sep2023_final.Rdata')

df_2_ <- df_linkada_fem_2019_2021_2 |>
  filter(sg_sexo == "F") |> 
  dplyr::mutate(
    rede_enc_sau = case_when((rede_sau=="1" | enc_saude=="1")~1,
                             T~0),
    assit_soc_creas = case_when((assist_soc=="1" |enc_creas=="1")~1,
                                T~0), # abrigo
    atend_enc_mulh = case_when((atend_mulh=="1" | enc_mulher=="1")~1,
                               T~0),
    cons_enc_tutela = case_when((cons_tutel=="1" | enc_tutela=="1")~1,
                                T~0),
    mpu_enc_mpu = case_when((mpu=="1" | enc_mpu=="1")~1,
                            T~0),
    deleg_enc_cria = case_when((deleg_cria=="1"|enc_dpca=="1")~1,
                               T~0),
    deleg_enc_mulh = case_when((deleg_mulh=="1"| enc_deam=="1")~1,
                               T~0),
    deleg_enc_deleg = case_when((deleg=="1"|enc_deleg=="1")~1,
                                T~0),
    infan_enc_juv = case_when((infan_juv=="1"|enc_vara=="1")~1),
    
    ds_tp_ocor = case_when(
      ds_tp_ocor == "AMEAÃ‡A" ~ "Ameaça",
      ds_tp_ocor == "LESÃƒO CORPORAL" ~ "Lesão corporal",
      ds_tp_ocor == "DIFAMAÃ‡ÃƒO" ~ "Difamação",
      ds_tp_ocor == "CALÃšNIA" ~ "Calúnia",
      ds_tp_ocor == "ESTUPRO VULNERÃ<81>VEL" ~ "Estupro de vulnerável",
      ds_tp_ocor == "DESCUMPRIMENTO MEDIDAS PROTETIVA URGÃŠNCIA" ~ "Descumprimento de medidas protetivas de urgência",
      ds_tp_ocor == "VIAS FATO" ~ "Vias Fato",
      ds_tp_ocor == "ESTUPRO" ~ "Estupro",
      ds_tp_ocor == "EXTORSÃƒO" ~ "Extorsão",
      TRUE ~ ds_tp_ocor  # Mantém os valores originais que não correspondem a nenhum dos anteriores
    ),
    
    dt_comum = coalesce(dt_obito, dt_notific, dt_internacao)
  )

rel <- vitaltable::rel

aux <- df_2_ |> 
  filter( !is.na(par_1) & FL_SINAN == 1)|> 
  distinct(par_1) |>
  mutate(par_reduzido_1 = 1:n()*20,
         par_reduzido_2 = 1:n())

df_2_ <- df_2_ |> left_join(aux, by="par_1")

# Shape do município
muni_rn <- st_read("mapas/RN_Municipios_2022/RN_Municipios_2022.shp", crs = 4326)



tema <- fresh::create_theme(
  fresh::bs4dash_status(
    info = "#121e87",
    secondary = "#ff5054",
    danger = "#ffcb37",
    primary = '#131e3c',
    warning = "#2b364a"
  )
  # fresh::adminlte_sidebar(
  #   #width = "400px",
  #   dark_bg = "#D8DEE9",
  #   dark_hover_bg = "#81A1C1",
  #   dark_color = "#2E3440"
  # ),
  # fresh::adminlte_global(
  #   content_bg = "#FFF",
  #   box_bg = "#D8DEE9", 
  #   info_box_bg = "#D8DEE9"
  # )
)

ui <- dashboardPage(
  # controlbar = dashboardControlbar(),
  header = dashboardHeader(
    title = bs4DashBrand(
      title = "Rio Grande do Norte",
      color = "info",
      image = "https://www.vitalstrategies.org/wp-content/uploads/2019/05/vs_author_icon02-300x300.png"
    )
  ),
  sidebar = dashboardSidebar(
    
      status = "secondary",
      

    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "Introdução",
        tabName = "introducao",
        icon = icon("house")
      ),
      
      bs4SidebarMenuItem(
        "Descrição geral do linkage",
        tabName = "home",
        icon = icon("chart-column")
      ),
      
      bs4SidebarMenuItem(
        "Mulheres vivas",
        tabName = "vivas",
        icon = icon("heart-pulse")
      ),
      bs4SidebarMenuItem(
        "Desfecho em óbito",
        tabName = "obitos",
        icon = icon("x")
      )
    )
  ),
  
  # Corpo visual do dashboard
  body = dashboardBody(
    fresh::use_theme(tema),
    
    bs4TabItems(
      
      bs4TabItem(
        tabName = "introducao",
        capa_ui("capa")
      ),
      
      bs4TabItem(
        tabName = "home",
        home_ui("geral")
      ),
      
      bs4TabItem(
        tabName = "vivas",
        vivas_ui("vivas1")
      ),
      
      bs4TabItem(
        tabName = "obitos",
        obitos_ui("obitos1")
      )
    )
  ),
  
  
  footer = dashboardFooter(
    left = "©Vital Strategies, Inc., 2024, 501(c)(3) not-for-profit organization"
    # right = ""
  )
)


server <- function(input, output, session) {
  capa_server("capa")
  home_server("geral")
  vivas_server("vivas1")
  obitos_server("obitos1")
}


shinyApp(ui, server)
