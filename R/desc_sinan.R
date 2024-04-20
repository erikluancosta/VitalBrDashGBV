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

df_sinan <- df_linkada_fem_2019_2021_2 |>
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
    
    faixa_etaria_padrao = ifelse(is.na(faixa_etaria_padrao), "IGNORADA", faixa_etaria_padrao),
    
    ds_autor_sexo = case_when(
      autor_sexo == "1" ~ "Masculino",
      autor_sexo == "2" ~ "Feminino",
      autor_sexo == "3" ~ "Ambos os sexos",
      TRUE ~ "IGNORADO"
    ),
    autor_alco = case_when(
      autor_alco == "1" ~ "Sim",
      autor_alco == "2" ~ "Não",
      TRUE ~ "IGNORADO"
    )
  ) |> 
  filter(
    banco == "SINAN"
  )
rm(df_linkada_fem_2019_2021_2)




sinan_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(box
             (width = 12,
               title = "Filtros",
               collapsible = FALSE,
               fluidRow(
                 # column(3,
                 #        wellPanel(
                 #          sliderInput(ns("filtro_ano"), label = NULL,
                 #                      min = 2019,
                 #                      max = 2021,
                 #                      value = c(2019,2021)))
                 # ),
                 column(6,
                        wellPanel(
                          selectInput(inputId = ns("filtro_idade"),
                                      label = "Faixa Etária",
                                      multiple = TRUE,
                                      choices = c("0 a 9 anos", "10 a 19 anos",
                                                  "20 a 29 anos", "30 a 59 anos",
                                                  "60+", "IGNORADA"),
                                      selected = c("0 a 9 anos", "10 a 19 anos",
                                                   "20 a 29 anos", "30 a 59 anos",
                                                   "60+", "IGNORADA"))
                        )),
                 column(6,
                        wellPanel(
                          selectInput(inputId = ns("filtro_raca"),
                                      label = "Raça/cor",
                                      multiple = TRUE,
                                      choices = unique(df_sinan$ds_raca),
                                      selected = unique(df_sinan$ds_raca))
                        ))
                 
               )
             )),
    fluidRow(
      
      box(title = 'Faixa etária',
          width = 6,
          status = "secondary",
          maximizable = TRUE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("faixa_etaria_graf")),
          downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")),
      
      box(title = 'Raça/cor',
          width = 6,
          status = "secondary",
          maximizable = TRUE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("raca_cor_graf")),
          downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela"))
    ),
    
    fluidRow(
      box(
        title = "Informações do SINAN",
        width = 12,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        # Primeira linha contendo os filtros
        fluidRow(
          column(4,
                 wellPanel(
                   selectInput(
                     ns("evolution_filter"),
                     "Tipo de variável do SINAN:",
                     selected = "enc",
                     multiple = FALSE,
                     choices = c("Encaminhamentos" = "enc", 
                                 "Procedimentos" = "proc", 
                                 "Relação com o agressor" = "rel", 
                                 "Tipo de violência" = "viol")
                   )
                 )
          ),
          column(4,
                 wellPanel(
                   # Aqui você adicionaria o segundo filtro
                   # Por exemplo, um segundo selectInput ou qualquer outro widget que desejar
                   selectInput(ns("valor_sinan_filter"), "Valor:", 
                               choices = c("Frequência" = FALSE,
                                           "Porcentagem" = TRUE)
                   )
                 )
          ),
          column(4,
                 wellPanel(
                   # Aqui você adicionaria o segundo filtro
                   # Por exemplo, um segundo selectInput ou qualquer outro widget que desejar
                   selectInput(ns("extrato_sinan_filter"), "Extratificado por:", 
                               choices = c("Raça/cor" = 'ds_raca', 
                                           "Faixa etária" = 'faixa_etaria_padrao')
                   )
                 )
          )
        ),
        # Segunda linha contendo o gráfico
        fluidRow(
          column(12,
                 # Tabela para output
                 dataTableOutput(ns("sinan")),
                 downloadButton(outputId = ns("download_tab_sinan"), label = "Download da Tabela")
          )
        )
      )
    ),
    
    fluidRow(
      
      box(title = 'Sexo do agressor',
          width = 6,
          status = "secondary",
          maximizable = TRUE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("sexo_agressor_graf")),
          downloadButton(outputId = ns("download_tb_sexo_agressor_graf"), label = "Download da Tabela")),
      
      box(title = 'Suspeita do uso de álcool',
          width = 6,
          status = "secondary",
          maximizable = TRUE,
          closable = FALSE,
          solidHeader = TRUE,
          plotlyOutput(ns("uso_alc_graf")),
          downloadButton(outputId = ns("download_tab_uso_alc_graf"), label = "Download da Tabela"))
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Distribuição da taxa de notificação por município",
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        leafletOutput(ns("mapa_sinan")),
        sliderInput(ns("ano_filtro"),
                    "Escolha um ano:",
                    min = 2010,
                    max = 2022,
                    value = 2022,  # Valor inicial do slider
                    step = 1,  # Incremento por etapa
                    ticks = TRUE,  # Mostra os valores abaixo do slider
                    sep = "",  # Não usar separador de milhares
                    animate = TRUE # Animação ao mover o slider
        )
      )
    )
    
  )
}

sinan_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Faixa etaria grafico ----
      output$faixa_etaria_graf <- renderPlotly({
        
        a <-  df_sinan |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca) |> 
          #distinct(par_f, ds_raca, sg_sexo, faixa_etaria_padrao) |>
          tab_1(faixa_etaria_padrao) |>
          filter(faixa_etaria_padrao != "Total") |> 
          mutate(cor = ifelse(faixa_etaria_padrao == "IGNORADA", "#9ba2cb", "#121e87")) |>
          ggplot(aes(
            x = faixa_etaria_padrao, y = `%`, fill = cor, 
            text = paste("Faixa etária:", faixa_etaria_padrao, "\nFrequência: ", `%`,"%", "\nRegistros: ", n)
          )
          ) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Faixa etária", y = "Frequência") + 
          theme_minimal() +
          theme(legend.position = "none") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        
        ggplotly(a, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "white", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
      })
      # Faixa etária download
      output$download_tab_faixa_etaria <- downloadHandler(
        # nome do arquivo
        filename = function() {
          paste("dados-faixa-etaria-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Recriar a lógica de filtragem para obter os dados exatos mostrados em output$faixa_etaria_graf
          
          tabela_fxetaria <-  df_sinan |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca
            ) |>
            #distinct(par_f, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_fxetaria, file, row.names = FALSE)
          
        }
      )
      
      
      
      # Raça/cor ----
      output$raca_cor_graf <- renderPlotly({
        # Prepare os dados
        dados_preparados <- df_sinan |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
          ) |>
          tab_1(ds_raca) |>
          filter(ds_raca != "Total")
        
        # Assegura que "IGNORADO" seja um dos níveis e esteja no final
        racas_ordenadas <- unique(dados_preparados$ds_raca)
        racas_ordenadas <- racas_ordenadas[racas_ordenadas != "IGNORADA"]
        racas_ordenadas <- c(racas_ordenadas, "IGNORADA") # Garante que IGNORADO venha por último
        
        # Inverte a ordem das raças para obter a ordem espelhada
        racas_ordenadas <- rev(racas_ordenadas)
        
        dados_preparados$ds_raca <- factor(dados_preparados$ds_raca, levels = racas_ordenadas)
        
        # Cria um vetor de cores
        cores <- setNames(rep("#121e87", length(racas_ordenadas)), racas_ordenadas)
        cores["IGNORADA"] <- "#9ba2cb" # Define explicitamente a cor para IGNORADO
        
        # Cria o gráfico
        b <- ggplot(dados_preparados, aes(
          x = ds_raca, y = `%`, fill = ds_raca, 
          text = paste("Raça/Cor:", ds_raca, "\nFrequência: ", `%`,"%", "\nRegistros: ", n)
        )
        )  +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = cores) +
          labs(
            x = "Raça/cor",
            y = "Frequência"
          ) +
          theme_minimal() +
          coord_flip() + # Faz com que as barras fiquem deitadas
          theme(legend.position = "none") # Remove a barra de legenda
        # Converter o gráfico ggplot para plotly
        ggplotly(b, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "white", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
        
      })
      
      # Raça/cor download
      output$download_tab_raca_cor <- downloadHandler(
        # nome do arquivo
        filename = function() {
          paste("dados-raca-cor-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Recriar a lógica de filtragem para obter os dados exatos mostrados em output$download_tab_raca_cor
          
          tabela_raca <-  df_sinan |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
            ) |>
            tab_1(ds_raca) 
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_raca, file, row.names = FALSE)
          
        })
      
      # SINAN ----
      output$sinan <- renderDataTable({
        
        rel <- vitaltable::rel
        
        
        filtered_df <- get(input$evolution_filter)
        tabela_sinan <- df_sinan |> 
          vitaltable::tab_cat_sinan(filtered_df, input$extrato_sinan_filter, input$valor_sinan_filter) |> as.data.frame() 
        
        # Checagem do filtro e aplicação das mudanças correspondentes
        if (input$evolution_filter == "enc") {
          tabela_sinan <- tabela_sinan |>
            rename(Encaminhamentos = tipo_filtered_df) |>
            mutate(Encaminhamentos = ifelse(Encaminhamentos == "tipo_filtered_df", "Nenhum encaminhamento", Encaminhamentos))
        } else if (input$evolution_filter == "proc") {
          tabela_sinan <- tabela_sinan |>
            rename(Procedimentos = tipo_filtered_df) |>
            mutate(Procedimentos = ifelse(Procedimentos == "tipo_filtered_df", "Nenhum procedimento", Procedimentos))
        } else if (input$evolution_filter == "rel") {
          tabela_sinan <- tabela_sinan |>
            rename(`Relacionamento com o agressor` = tipo_filtered_df) |>
            mutate(`Relacionamento com o agressor` = ifelse(`Relacionamento com o agressor` == "tipo_filtered_df", "Nenhum relacionamento registrado", `Relacionamento com o agressor`))
        } else if (input$evolution_filter == "viol") {
          tabela_sinan <- tabela_sinan |>
            rename(`Tipo de violência` = tipo_filtered_df) |>
            mutate(`Tipo de violência` = ifelse(`Tipo de violência` == "tipo_filtered_df", "Nenhum tipo de violência registrado", `Tipo de violência`))
        }
        
        datatable(tabela_sinan, options = list(pageLength = 20))
        
      })
      
      # SINAN download
      output$download_tab_sinan <- downloadHandler(
        filename = function() {
          paste("dados-sinan-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Recriar a lógica de filtragem para obter os dados exatos mostrados em output$sinan
          # Isso assegura que o CSV reflita o estado atual da tabela
          rel <- vitaltable::rel
          
          filtered_df <- get(input$evolution_filter)
          tabela_sinan <- df_sinan |> 
            vitaltable::tab_cat_sinan(filtered_df, input$extrato_sinan_filter, input$valor_sinan_filter) |> as.data.frame()
          
          # Aplicar as mesmas condições de filtragem que você tem em output$sinan
          if (input$evolution_filter == "enc") {
            tabela_sinan <- tabela_sinan |>
              dplyr::rename(Encaminhamentos = tipo_filtered_df) |>
              dplyr::mutate(Encaminhamentos = dplyr::if_else(Encaminhamentos == "tipo_filtered_df", "Nenhum encaminhamento", Encaminhamentos))
          } else if (input$evolution_filter == "proc") {
            tabela_sinan <- tabela_sinan |>
              dplyr::rename(Procedimentos = tipo_filtered_df) |>
              dplyr::mutate(Procedimentos = dplyr::if_else(Procedimentos == "tipo_filtered_df", "Nenhum procedimento", Procedimentos))
          } else if (input$evolution_filter == "rel") {
            tabela_sinan <- tabela_sinan |>
              dplyr::rename(`Relacionamento com o agressor` = tipo_filtered_df) |>
              dplyr::mutate(`Relacionamento com o agressor` = dplyr::if_else(`Relacionamento com o agressor` == "tipo_filtered_df", "Nenhum relacionamento registrado", `Relacionamento com o agressor`))
          } else if (input$evolution_filter == "viol") {
            tabela_sinan <- tabela_sinan |>
              dplyr::rename(`Tipo de violência` = tipo_filtered_df) |>
              dplyr::mutate(`Tipo de violência` = dplyr::if_else(`Tipo de violência` == "tipo_filtered_df", "Nenhum tipo de violência registrado", `Tipo de violência`))
          }
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_sinan, file, row.names = FALSE)
        }
      )
      
      
      # Gráfico sexo do agressor ---- 
      output$sexo_agressor_graf<- renderPlotly({
        
        df_to_graf <- df_sinan |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca
          ) |> 
          vitaltable::tab_1(ds_autor_sexo) |> 
          filter(ds_autor_sexo != "Total")
        
        ordered_levels <- tab_1(df_sinan, ds_autor_sexo) |> filter(ds_autor_sexo != "Total") |> 
          arrange(desc(ds_autor_sexo == "IGNORADO"), `%`) %>% 
          pull(ds_autor_sexo)
        
        df_to_graf$ds_autor_sexo <- factor(df_to_graf$ds_autor_sexo, levels = ordered_levels)
        
        # Garantindo que temos uma coluna 'n' e ajustando o texto dos tooltips
          df_to_graf <- df_to_graf %>%
          mutate(text = paste("Faixa etária:", ds_autor_sexo, "\nFrequência: ", round(`%`, 1), "%", "\nRegistros: ", n))
        
        p <- ggplot(data = df_to_graf, aes(x = factor(1), y = `%`, fill = ds_autor_sexo, text = text)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = c("Masculino" = "#ff5054", 
                                       "Feminino" = "#14147f",
                                       "IGNORADO" = "#9ba2cb", 
                                       "Ambos os sexos" = "#f8d023")) +
          geom_text(aes(label = sprintf("%1.1f%%", `%`)), position = position_stack(vjust = 0.5), colour ="white", size=3) +
          labs(x = "", y = "Proporção entre as categorias", fill = "Sexo do Autor") +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),  # Remove título do eixo X
            axis.text.x = element_blank(),   # Remove texto do eixo X
            axis.ticks.x = element_blank(),  # Remove marcas de tick do eixo X
            legend.position = "bottom"       # Move a legenda para baixo
          ) +
          ggtitle("")
        
        # Convertendo o ggplot para plotly e ajustando os tooltips
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "white", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
        
      })
      
      
      
      
      # Gráfico de suspeita do uso de álcool ----
      output$uso_alc_graf<- renderPlotly({
        
        df_to_graf <- df_sinan |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca
          ) |> 
          vitaltable::tab_1(autor_alco) |> 
          filter(autor_alco != "Total")
        
        ordered_levels <- tab_1(df_sinan, autor_alco) |> filter(autor_alco != "Total") |> 
          arrange(desc(autor_alco == "IGNORADO"), `%`) %>% 
          pull(autor_alco)
        
        df_to_graf$autor_alco <- factor(df_to_graf$autor_alco, levels = ordered_levels)
        
        # Garantindo que temos uma coluna 'n' e ajustando o texto dos tooltips
        df_to_graf <- df_to_graf %>%
          mutate(text = paste("Faixa etária:", autor_alco, "\nFrequência: ", round(`%`, 1), "%", "\nRegistros: ", n))
        
        p <- ggplot(data = df_to_graf, aes(x = factor(1), y = `%`, fill = autor_alco, text = text)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(
            values = c(
              "Não" = "#14147f",
              "IGNORADO" = "#9ba2cb", 
              "Sim" = "#ff5054"
            )
          ) +
          geom_text(aes(
            label = sprintf("%1.1f%%", `%`)), 
            position = position_stack(vjust = 0.5),
            colour ="white",
            size=3
          ) +
          labs(x = "", y = "Proporção entre as categorias", fill = "Sexo do Autor") +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),  # Remove título do eixo X
            axis.text.x = element_blank(),   # Remove texto do eixo X
            axis.ticks.x = element_blank(),  # Remove marcas de tick do eixo X
            legend.position = "bottom"       # Move a legenda para baixo
          ) +
          ggtitle("")
        
        # Convertendo o ggplot para plotly e ajustando os tooltips
        ggplotly(p, tooltip = "text") |> layout(
          hoverlabel = list(
            bgcolor = "white", # Cor de fundo do tooltip
            font = list(color = "black") # Cor do texto
          )
        )
        
      })
      
      
      output$mapa_sinan <- renderLeaflet({
        # Ler o arquivo shapefile
        muni_rn <- st_read("mapas/RN_Municipios_2022/RN_Municipios_2022.shp")
        
        # Transformar as coordenadas para o sistema de referência WGS 84 (CRS 4326)
        muni_rn <- st_transform(muni_rn, crs = 4326)
        muni_rn$ID_MN_RESI <- substr(muni_rn$CD_MUN, 1, nchar(muni_rn$CD_MUN) - 1)
        
        # Taxa
        taxa <- read.csv('dados/prev_mun_br_2010_2022.csv')
        taxa <- taxa |> filter(ANO_NOT==input$ano_filtro)
        
        # Novo df
        para_mapa <- muni_rn |> mutate(ID_MN_RESI = as.integer(ID_MN_RESI)) |> left_join(taxa, by="ID_MN_RESI")
        
        
        # Cores personalizadas
        cores <- c( "#14147f","#f8d023","#f55858")
        
        # Crie sua própria função de cores com base nas suas preferências
        pal <- colorNumeric(palette = cores, domain = range(na.omit(para_mapa$tx_not_pop_fem_100k), na.rm = TRUE))
        
        m <- leaflet(data = para_mapa) |>
          addProviderTiles(providers$CartoDB.Positron) |>
          addPolygons(
            fillColor = ~pal(tx_not_pop_fem_100k),
            fillOpacity = 0.8, 
            color = "#BDBDC3", 
            weight = 1,
            label = ~paste(NM_MUN),
            popup = ~paste("Município: ", NM_MUN, "<br>",
                           "Taxa de notificação por 100k: ", round(tx_not_pop_fem_100k, 2), "<br>",
                           "População: ", POP, "<br>",
                           "Notificações: ", notificacoes),
            labelOptions = labelOptions(style = list("font-weight" = "normal"), 
                                        direction = 'auto')
          ) |>
          addLegend(pal = pal, 
                    values = ~tx_not_pop_fem_100k, 
                    title = "Taxa de Notificação",
                    opacity = 1)
        m
        
      })
      
    }
    
    
  )
}