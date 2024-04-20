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

df_sesed <- df_linkada_fem_2019_2021_2 |>
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
    banco%in%c('SESAP', 'SESAP_OB')
  ) |> 
  mutate(
    banco = ifelse(
      banco == "SESAP", "SESED", ifelse(
        banco == "SESAP_OB", "SESED óbitos", banco
      )
    )
  )



sesed_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width = 12,
          title = "Filtros",
          collapsible = FALSE,
          fluidRow(
            column(6,
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
                     )
                   )
            ),
            column(6,
                   wellPanel(
                     selectInput(
                       inputId = ns("filtro_raca"),
                       label = "Raça/cor",
                       multiple = TRUE,
                       choices = unique(df_sesed$ds_raca),
                       selected = unique(df_sesed$ds_raca))
                   )
            )
          )
      )
    ),
    
    fluidRow(
      tabBox(width = 12,
             tabPanel("SESED Letal",
                      # Linha com os gráficos de idade e raça/cor para mulheres mortas
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
                      # Gráficos menores dentro da área do SESED letal
                      fluidRow(
                        box(width = 4,
                            title = "Tipos penais",
                            status = "secondary",
                            maximizable = TRUE,
                            closable = FALSE,
                            solidHeader = TRUE,
                            plotlyOutput(ns("tp_penais_graf"))
                        ),  
                        
                        box(width = 4, 
                            title = "Local de ocorrência",
                            status = "secondary",
                            maximizable = TRUE,
                            closable = FALSE,
                            solidHeader = TRUE,
                            plotlyOutput(ns("tp_loc_ocor"))
                        ),  
                        
                        box(width = 4,
                            title = "Meio empregado",
                            status = "secondary",
                            maximizable = TRUE,
                            closable = FALSE,
                            solidHeader = TRUE,
                            plotlyOutput(ns("tp_meio_empregado"))
                        )   
                      )),
             # Segunda aba
             tabPanel("SESED Não Letal",
                      fluidRow(
                        box(title = 'Faixa etária',
                            width = 6,
                            status = "secondary",
                            maximizable = TRUE,
                            closable = FALSE,
                            solidHeader = TRUE,
                            plotlyOutput(ns("faixa_etaria_graf_vivas")),
                            downloadButton(outputId = ns("download_tab_faixa_etaria_vivas"), label = "Download da Tabela")),
                        
                        box(title = 'Raça/cor',
                            width = 6,
                            status = "secondary",
                            maximizable = TRUE,
                            closable = FALSE,
                            solidHeader = TRUE,
                            plotlyOutput(ns("raca_cor_graf_vivas")),
                            downloadButton(outputId = ns("download_tab_raca_cor_vivas"), label = "Download da Tabela"))
                      ),
                      fluidRow(
                        box(width = 6,
                            title = "b",
                            status = "secondary",
                            maximizable = TRUE,
                            closable = FALSE,
                            solidHeader = TRUE,
                            plotlyOutput(ns("local_ocor_sesed"))
                        ),
                        box(width = 6,
                            title = "a",
                            status = "secondary",
                            maximizable = TRUE,
                            closable = FALSE,
                            solidHeader = TRUE,
                            plotlyOutput(ns("tp_viol_sesed"))
                        )
                      )
             )
      )
    )
  )
}

sesed_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Faixa etaria grafico obt ----
      output$faixa_etaria_graf <- renderPlotly({
        
        a <-  df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED óbitos") |> 
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
          
          tabela_fxetaria <-  df_sesed |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco == "SESED óbitos"
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
        dados_preparados <- df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED óbitos"
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
          
          tabela_raca <-  df_sesed |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco == "SESED óbitos"
            ) |>
            tab_1(ds_raca) 
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_raca, file, row.names = FALSE)
          
        })
      
      
      # Tipos Penais Gráfico ----
      output$tp_penais_graf <- renderPlotly({
        
        a <-  df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED óbitos") |> 
          tab_1(ds_tp_obito) |>
          filter(ds_tp_obito != "Total") |> 
          #mutate(cor = ifelse(ds_tp_obito == "IGNORADA", "#9ba2cb", "#121e87")) |>
          ggplot(aes(
            x = reorder(ds_tp_obito, -`%`), y = `%`, 
            text = paste("Faixa etária:", ds_tp_obito, "\nFrequência: ", `%`,"%", "\nRegistros: ", n)
          )
          ) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Tipos penais", y = "Frequência") + 
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
      
      
      # Local de ocorrência Gráfico ----
      output$tp_loc_ocor <- renderPlotly({
        
        a <-  df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED óbitos") |> 
          tab_1(ds_local_ocorr) |>
          filter(ds_local_ocorr != "Total") |> 
          ggplot(aes(
            x = reorder(ds_local_ocorr, -`%`), y = `%`, 
            text = paste("Faixa etária:", ds_local_ocorr, "\nFrequência: ", `%`,"%", "\nRegistros: ", n)
          )
          ) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Locais de ocorrência", y = "Frequência") + 
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
      
      
      # Local de ocorrência Gráfico ----
      output$tp_meio_empregado <- renderPlotly({
        
        a <-  df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED óbitos") |> 
          tab_1(ds_meio_emp) |>
          filter(ds_meio_emp != "Total") |> 
          ggplot(aes(
            x = reorder(ds_meio_emp, -`%`), y = `%`, 
            text = paste("Faixa etária:", ds_meio_emp, "\nFrequência: ", `%`,"%", "\nRegistros: ", n)
          )
          ) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Meio empregado", y = "Frequência") + 
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
      
      
      
      
      
      
      
      
      
      
      
      
      # Faixa etaria grafico Vivas----
      output$faixa_etaria_graf_vivas <- renderPlotly({
        
        a <-  df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED") |> 
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
      output$download_tab_faixa_etaria_vivas <- downloadHandler(
        # nome do arquivo
        filename = function() {
          paste("dados-faixa-etaria-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Recriar a lógica de filtragem para obter os dados exatos mostrados em output$faixa_etaria_graf
          
          tabela_fxetaria <-  df_sesed |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco == "SESED"
            ) |>
            #distinct(par_f, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_fxetaria, file, row.names = FALSE)
          
        }
      )
      
      
      # Raça/cor Vivas----
      output$raca_cor_graf_vivas <- renderPlotly({
        # Prepare os dados
        dados_preparados <- df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED"
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
        cores <- setNames(rep("#14147f", length(racas_ordenadas)), racas_ordenadas)
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
      output$download_tab_raca_cor_vivas <- downloadHandler(
        # nome do arquivo
        filename = function() {
          paste("dados-raca-cor-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Recriar a lógica de filtragem para obter os dados exatos mostrados em output$download_tab_raca_cor
          
          tabela_raca <-  df_sesed |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco == "SESED"
            ) |>
            tab_1(ds_raca) 
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_raca, file, row.names = FALSE)
          
        })
      
      
      # LOCAL DE OCORRENCIA SESED ----
      output$local_ocor_sesed <- renderPlotly({
        
        a <-  df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED") |> 
          tab_1(lococor) |>
          filter(lococor != "Total") |> 
          ggplot(aes(
            x = reorder(lococor, -`%`), y = `%`, 
            text = paste("Faixa etária:", lococor, "\nFrequência: ", `%`,"%", "\nRegistros: ", n)
          )
          ) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Locais de ocorrência", y = "Frequência") + 
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
      
      
      # Tipos de ocorrência no SESED ----
      output$tp_viol_sesed <- renderPlotly({
        
        a <-  df_sesed |>
          filter(
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco == "SESED") |> 
          tab_1(ds_tp_ocor) |>
          filter(ds_tp_ocor != "Total") |> 
          ggplot(aes(
            x = reorder(ds_tp_ocor, -`%`), y = `%`, 
            text = paste("Faixa etária:", ds_tp_ocor, "\nFrequência: ", `%`,"%", "\nRegistros: ", n)
          )
          ) + 
          geom_bar(stat = "identity")+
          scale_fill_identity() +
          labs(x = "Tipos de ocorrência", y = "Frequência") + 
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
      
      
      
      
      
      
    }
    
  )
}