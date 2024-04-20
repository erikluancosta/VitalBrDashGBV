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
library(tidyr)
library(DT)
library(haven)
library(writexl)
load('dados/base_linkada_anon_female_2019_2021_14sep2023_final.Rdata')

df_2_ <- df_linkada_fem_2019_2021_2 |>
  dplyr::filter(sg_sexo == "F") |>
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
    
    dt_comum = coalesce(dt_obito, dt_notific, dt_internacao),
    
    faixa_etaria_padrao = ifelse(is.na(faixa_etaria_padrao), "IGNORADA", faixa_etaria_padrao),
    banco = ifelse(
      banco == "SESAP", "SESED", ifelse(
        banco == "SESAP_OB", "SESED óbitos", banco
      )
    )
  )
rm(df_linkada_fem_2019_2021_2)



linhavida_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 12,
          title = "Filtros",
          collapsible = FALSE,
          fluidRow(
            column(4,
                   wellPanel(
                     selectInput(inputId = ns("filtro_raca"),
                                 label = "Raça/cor",
                                 multiple = TRUE,
                                 choices = unique(df_2_$ds_raca),
                                 selected = unique(df_2_$ds_raca))  # Inicialmente sem escolhas
                   )),
            column(4,
                   wellPanel(
                     selectInput(inputId = ns("filtro_banco"),
                                 label = "Banco de dados",
                                 multiple = TRUE,
                                 choices = unique(df_2_$banco),
                                 selected = unique(df_2_$banco))  # Inicialmente sem escolhas
                   )),
            column(4,
                   wellPanel(
                     selectInput(
                       inputId = ns("filtro_idade"),
                       label = "Faixa Etária",
                       multiple = TRUE,
                       choices = c("0 a 9 anos", "10 a 19 anos",
                                   "20 a 29 anos", "30 a 59 anos",
                                   "60+", "IGNORADA"),
                       selected = c("0 a 9 anos", "10 a 19 anos",
                                    "20 a 29 anos", "30 a 59 anos",
                                    "60+", "IGNORADA")
                     )  # Inicialmente sem escolhas
                   ))
          )
      )
    ),
    fluidRow(
      box(
        h4("Texto sobre o gráfico- Título"),
        p("textos, textos, textos
          A linha de vida é um equipamento fundamental para equipes que executam serviços em altura,
          que podem resultar em queda com fraturas graves ou óbito `r df_2_ |> nrow()`. A linha de vida assegura que o funcionário 
          esteja seguro enquanto executa sua atividade laboral."),
        p("A linha de vida é um equipamento fundamental para equipes que
          executam serviços em altura, que podem resultar em queda com fraturas graves ou óbito.
          A linha de vida assegura que o funcionário esteja seguro enquanto executa sua atividade laboral."),
        title = "Linha da vida",
        width = 12,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("linha_vida_geral_")),
        tags$script(HTML("
          document.addEventListener('shiny:connected', function(event) {
              setTimeout(function() {
                  var myPlot = document.getElementById('linha_vida_geral');
                  var legendItems = myPlot.querySelector('.legend').querySelectorAll('.traces');
                  legendItems.forEach(function(item) {
                      var label = item.querySelector('.legendtext').textContent;
                      if (label === 'SINAN') {
                          item.style.pointerEvents = 'none';
                          item.style.opacity = '0.5';
                      }
                  });
              }, 1000);
          });
        "))
      )
    )
    
    
  )
}

linhavida_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Linha da vida gráfico ----
      output$linha_vida_geral_ <- renderPlotly({
        
        req(input$filtro_banco) # Certifica que o filtro_banco está disponível
        
        aux <- df_2_ |>
          filter(!is.na(par_1) & FL_SINAN == 1, banco %in% input$filtro_banco,
                 ds_raca %in% input$filtro_raca,
                 faixa_etaria_padrao %in% input$filtro_idade) |>
          distinct(par_1) |>
          mutate(par_reduzido_1 = 1:n()*20, par_reduzido_2 = 1:n())
        
        # Esta etapa assume que df_2_ já contém as colunas necessárias ou é ajustado adequadamente
        df_aux <- left_join(df_2_, aux, by = "par_1")
        
        ca <- df_aux |>
          filter(!is.na(par_1), FL_SINAN == 1) |>
          group_by(par_1) |>
          mutate(repete = n()) |>
          ungroup() |>
          filter(repete > 1) |>
          ggplot(aes(x = dt_comum, y = par_reduzido_1)) +
          geom_line(aes(group = par_reduzido_1), color = 'lightgray', size = 0.15) +
          geom_point(aes(color = banco), size = 0.5) +
          labs(x = "Data do evento", y = "") +
          theme_minimal() +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())
        # adicionar o N de vezes que a pessoa aparece
        ggplotly(ca)
      })
      
      
      
    })}