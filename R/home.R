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
    
    faixa_etaria_padrao = ifelse(is.na(faixa_etaria_padrao), "IGNORADA", faixa_etaria_padrao)
  )
rm(df_linkada_fem_2019_2021_2)




# Shape do município
muni_rn <- st_read("mapas/RN_Municipios_2022/RN_Municipios_2022.shp", crs = 4326)


home_ui <- function(id) {
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
                 column(4,
                        wellPanel(
                          selectInput(inputId = ns("filtro_raca"),
                                      label = "Raça/cor",
                                      multiple = TRUE,
                                      choices = unique(df_2_$ds_raca),
                                      selected = unique(df_2_$ds_raca))
                        )),
                 column(4,
                        wellPanel(
                          selectInput(inputId = ns("filtro_banco"),
                                      label = "Banco de dados",
                                      multiple = TRUE,
                                      choices = unique(df_2_$banco),
                                      selected = unique(df_2_$banco))
                        )),
                 column(4,
                        wellPanel(
                          selectInput(inputId = ns("filtro_idade"),
                                      label = "Faixa Etária",
                                      multiple = TRUE,
                                      choices = unique(df_2_$faixa_etaria_padrao),
                                      selected = unique(df_2_$faixa_etaria_padrao))
                        ))
               )
             )),
    
    fluidRow(
      bs4Dash::valueBoxOutput(ns("num_registros")),
      bs4Dash::infoBoxOutput(ns("num_mulheres")),
      bs4Dash::infoBoxOutput(ns("reg_pareado"))
    ),
    fluidRow(
      box(
        title = 'Faixa etária',
        width = 6,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("faixa_etaria_graf")),
        downloadButton(outputId = ns("download_tab_faixa_etaria"), label = "Download da Tabela")
        
      ),
      box(
        title = 'Raça/cor',
        width = 6,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        plotlyOutput(ns("raca_cor_graf")),
        downloadButton(outputId = ns("download_tab_raca_cor"), label = "Download da Tabela")
        
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
        plotlyOutput(ns("linha_vida_geral"))
      )
    ),
    fluidRow(
      box(
        title = "Distribuição no Estado",
        width = 12,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        leafletOutput(ns("mapa_estado"))
      )),
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
    ))
  
}

home_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # Output Número de registros ----
      output$num_registros <- renderValueBox({
        valueBox(
          df_2_ |> 
            filter(
              # ano>=min(input$filtro_ano),
              # ano<=max(input$filtro_ano),
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco
            ) |> 
            nrow(),
          "Registros nas bases",
          # fill = TRUE,
          # gradient = TRUE,
          color = "danger",
          icon = icon("address-card"),
        )
      })
      
      # Output para o número de mulheres ----
      output$num_mulheres <- renderValueBox({
        valueBox(
          df_2_ |> 
            filter(
              # ano>=min(input$filtro_ano),
              # ano<=max(input$filtro_ano),
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco) |> 
            distinct(par_f) |>
            nrow(),
          icon = icon("venus"),
          "Mulheres nas bases",
          
          color = "danger"
          
        )
      })
      
      # Output para o número de registros pareados ----
      output$reg_pareado <- renderValueBox({
        valueBox(
          df_2_ |> 
            dplyr::filter(
              !is.na(par_1)
            ) |>
            nrow(),
          "Registros pareados",
          color = "danger",
          icon = icon("code-compare")
        )
      })
      
      
      
      
      # Faixa etaria grafico ----
      output$faixa_etaria_graf <- renderPlotly({
        
        a <-  df_2_ |>
          filter(
            # ano >= min(input$filtro_ano),
            # ano <= max(input$filtro_ano),
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco) |> 
          distinct(par_f, ds_raca, sg_sexo, faixa_etaria_padrao) |>
          tab_1(faixa_etaria_padrao) |>
          filter(faixa_etaria_padrao != "Total") |> 
          mutate(cor = ifelse(faixa_etaria_padrao == "IGNORADA", "gray", "#121e87")) |>
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
          
          tabela_fxetaria <-  df_2_ |>
            filter(
              # ano >= min(input$filtro_ano),
              # ano <= max(input$filtro_ano),
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco) |>
            distinct(par_f, ds_raca, sg_sexo, faixa_etaria_padrao) |>
            tab_1(faixa_etaria_padrao) |>
            arrange(faixa_etaria_padrao)
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_fxetaria, file, row.names = FALSE)
          
        }
      )
      
      
      
      # Raça/cor ----
      output$raca_cor_graf <- renderPlotly({
        # Prepare os dados
        dados_preparados <- df_2_ |>
          filter(
            # ano >= min(input$filtro_ano),
            # ano <= max(input$filtro_ano),
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco
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
        cores["IGNORADA"] <- "gray" # Define explicitamente a cor para IGNORADO
        
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
          
          tabela_raca <-  df_2_ |>
            filter(
              # ano >= min(input$filtro_ano),
              # ano <= max(input$filtro_ano),
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco
            ) |>
            tab_1(ds_raca) 
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_raca, file, row.names = FALSE)
          
        })
      
      
      # Linha da vida gráfico ----
      output$linha_vida_geral <- renderPlotly({
        
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
      
      
      
      output$mapa_estado <- renderLeaflet({
        leaflet() |>
          addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE)) |>
          setView(lng = -36.239436, lat =  -5.699705, zoom = 8)
      })
      
      # Adicionar polígonos ao mapa ----
      observe({
        leafletProxy("mapa_estado", data = muni_rn) |>
          clearShapes() |>
          addPolygons(weight = 1, color = "#666666",
                      fillOpacity = 0.5, opacity = 1,
                      label = ~NM_MUN,
                      labelOptions = labelOptions(direction = 'auto', noHide = FALSE, textOnly = TRUE),
                      layerId = ~NM_MUN) # Usar o nome do município como ID único para cada polígono
      })
      
      # Detectar clique e destacar polígono ----
      observeEvent(input$mapa_estado_shape_click, {
        click_info <- input$mapa_estado_shape_click
        leafletProxy("mapa_estado") |>
          clearShapes() |>
          addPolygons(data = muni_rn, 
                      weight = 2, color = "lightgray", fillColor = "gray",
                      fillOpacity = 0.7, layerId = ~NM_MUN,
                      options = pathOptions(dashArray = "5, 5")) |>
          addPolygons(data = subset(muni_rn, NM_MUN == click_info$id),
                      weight = 3, color = "#666666", fillColor = "#666666",
                      fillOpacity = 0.7, layerId = click_info$id)
      })
      
      
      # SINAN ----
      output$sinan <- renderDataTable({
        
        rel <- vitaltable::rel
        
        
        filtered_df <- get(input$evolution_filter)
        tabela_sinan <- df_2_ |> 
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
          tabela_sinan <- df_2_ |> 
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
      
    })
}
