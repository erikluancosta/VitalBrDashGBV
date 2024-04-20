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




# Shape do município
muni_rn <- st_read("mapas/RN_Municipios_2022/RN_Municipios_2022.shp", crs = 4326)


linkage_ui <- function(id) {
  ns <- NS(id)
  # Incluir CSS personalizado diretamente no UI
  tags$head(
    tags$style(HTML("
            .info-box .info-box-number { font-size: 32px; }
        "))
  )
  tagList(
    
    fluidRow(
      box(width = 12,
          title = "Filtros",
          collapsible = FALSE,
          fluidRow(
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
                   )),
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
                   ))
            
          )
      )
    ),

    fluidRow(
        bs4Dash::infoBoxOutput(ns("num_registros")),
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
      tags$head(
        tags$style(HTML("
            .info-text-box {
                background-color: #dfdfdf;
                padding: 10px;
                border-radius: 5px;
                margin-bottom: 5px;
            }
        "))
      ),
      box(
        title = "Causas de óbito",
        width = 12,
        status = "secondary",
        maximizable = TRUE,
        closable = FALSE,
        solidHeader = TRUE,
        h5("Comparação entre as causas de óbito entre mulheres que tiveram notificação no SINAN e todas as mulheres"),
        plotlyOutput(ns("causas_obito_linkage")),
        div(class = "info-text-box", h6(textOutput(ns("texto_raca_cor"))),
            h6(textOutput(ns("texto_faixa_etaria"))))
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
      ))
  )
  
}

linkage_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      # Output Número de registros ----
      output$num_registros <- renderValueBox({
        
          valueBox(
            df_2_ |> 
              filter(
                faixa_etaria_padrao %in% input$filtro_idade,
                ds_raca %in% input$filtro_raca,
                banco %in% input$filtro_banco
              ) |> 
              nrow(),
            "Registros nas bases",
            color = "danger",
            icon = icon("address-card")
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
            faixa_etaria_padrao %in% input$filtro_idade,
            ds_raca %in% input$filtro_raca,
            banco %in% input$filtro_banco) |> 
          distinct(par_f, ds_raca, sg_sexo, faixa_etaria_padrao) |>
          tab_1(faixa_etaria_padrao) |>
          filter(faixa_etaria_padrao != "Total") |> 
          mutate(cor = ifelse(faixa_etaria_padrao == "IGNORADA", "#9ba2cb", "#14147f")) |>
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
      output$download_tab_raca_cor <- downloadHandler(
        # nome do arquivo
        filename = function() {
          paste("dados-raca-cor-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          # Recriar a lógica de filtragem para obter os dados exatos mostrados em output$download_tab_raca_cor
          
          tabela_raca <-  df_2_ |>
            filter(
              faixa_etaria_padrao %in% input$filtro_idade,
              ds_raca %in% input$filtro_raca,
              banco %in% input$filtro_banco
            ) |>
            tab_1(ds_raca) 
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_raca, file, row.names = FALSE)
          
        })
      
      
      # Linha da vida gráfico ----
      output$causas_obito_linkage <- renderPlotly({
        
        # Preparando o dataframe 'a'
        a <- df_obitos |>
          filter(banco == "SIM",
                 faixa_etaria_padrao %in% input$filtro_idade,
                 ds_raca %in% input$filtro_raca) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          rename("Todos os óbitos" = `%`,
                 "Todos os óbitos (n)" = n) |>
          arrange(desc(`Todos os óbitos`))  # Ordena por 'Todos os óbitos'
        
        # Preparando o dataframe 'b'
        b <- df_obitos |>
          filter(banco == "SIM", FL_SINAN == 1,
                 faixa_etaria_padrao %in% input$filtro_idade,
                 ds_raca %in% input$filtro_raca) |>
          tab_1(causa_resumida) |>
          filter(causa_resumida != "Total") |>
          rename("Óbitos de mulheres com notificação" = `%`,
                 "Óbitos de mulheres com notificação(n)" = n)
        
        # Combinando 'a' e 'b' com uma junção à esquerda
        c <- left_join(b,a,by = "causa_resumida") |>
          pivot_longer(
            cols = c("Todos os óbitos", "Óbitos de mulheres com notificação"),
            names_to = "nivel",
            values_to = "valor")
        
        
        # Reordenando o fator 'causa_resumida' baseado nos valores específicos para "Óbitos de mulheres com notificação"
        c <- c |>
          group_by(causa_resumida) |>
          summarise(max_valor = max(valor[nivel == "Óbitos de mulheres com notificação"]), .groups = 'drop') |>
          arrange(desc(-max_valor)) |>
          left_join(c, by = "causa_resumida") |>
          mutate(causa_resumida = factor(causa_resumida, levels = unique(causa_resumida)))
        
        # Agora, criando o gráfico com a ordenação correta
        min_valor <- min(c$valor, na.rm = TRUE)
        max_valor <- max(c$valor, na.rm = TRUE)
        
        bolha_c <- ggplot(c, aes(x = nivel, 
                                 y = causa_resumida, 
                                 size = valor, 
                                 color = nivel)) +
          geom_point(alpha = 0.9) +
          geom_text(aes(label = round(valor, 2)), 
                    nudge_x = 0.2,  # Nudge para mover o texto para o lado direito
                    hjust = 0,       # Alinha o texto à esquerda do ponto de ancoragem
                    fontface = "bold", # Estilo da fonte
                    size = 3.5) +   # Tamanho da fonte
          scale_size_continuous(range = c(1, 9.2), 
                                limits = c(min_valor, max_valor), 
                                breaks = pretty(c$valor, n = 5))+
          labs(size = "valor",
               x = '', 
               y = '') +
          theme_test() +
          theme(axis.text.x = element_text(hjust = 1, vjust = 1)) +
          guides(size = guide_legend(override.aes = list(alpha = 1))) +
          scale_color_manual(values = c("#f55858", "#14147f"))
        
        ggplotly(bolha_c)
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
      
      
     
      
      ### TEXTOS ----
      # Função reativa para capturar a seleção do filtro de raça/cor ----
      selected_raca_cor <- reactive({
        input$filtro_raca
      })
      
      # Função reativa para a seleção do filtro de faixa etária
      selected_faixa_etaria <- reactive({
        input$filtro_idade
      })
      
      
      # Renderiza o texto com os valores selecionados
      output$texto_raca_cor <- renderText({
        raca_cor <- selected_raca_cor()
        if (is.null(raca_cor)) {
          return("Nenhuma raça/cor selecionada.")
        } else {
          return(paste("Filtro de raça/cor da visualização: ", paste(raca_cor, collapse = ", "), "."))
        }
      })
      
      
      # Renderiza o texto para a faixa etária
      output$texto_faixa_etaria <- renderText({
        faixa_etaria <- selected_faixa_etaria()
        if (is.null(faixa_etaria)) {
          "Nenhuma faixa etária selecionada."
        } else {
          paste("Filtro de faixa etária da visualização:", paste(faixa_etaria, collapse = ", "), ".")
        }
      })
      
      
      
      
      
      
    })
}

