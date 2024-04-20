load('dados/base_linkada_anon_female_2019_2021_14sep2023_final.Rdata')

df_vivas <- df_linkada_fem_2019_2021_2 |>
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
    
    dt_comum = coalesce(dt_obito, dt_notific, dt_internacao),
    banco = ifelse(
      banco == "SESAP", "SESED", ifelse(
        banco == "SESAP_OB", "SESED óbitos", banco
      )
    ),
    grupo_m_vivas = case_when(
      viva_recorte1==1~"g1v",
      viva_recorte2==1~"g2v",
      viva_recorte3==1~"g3v"
    ),
    grupo_m_ob = case_when(
      ob_recorte1==1~"g1o",
      ob_recorte2==1~"g2o",
      ob_recorte3==1~"g3o"
    )
  ) 

rm(df_linkada_fem_2019_2021_2)
vivas_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    fluidRow(box
             (width = 12,
               title = "Filtros",
               collapsible = FALSE,
               fluidRow(
                 column(12,
                        wellPanel(
                          selectInput(inputId = ns("filtro_grupo"),
                                      label = "Grupo de análise",
                                      multiple = FALSE,
                                      choices = c(
                                        "i. Mulheres com notificação de violência no SINAN, mas sem Boletim de Ocorrência" = "g1v", 
                                        "ii. Mulheres com Boletim de Ocorrência, mas que não têm notificação de violência no SINAN" = "g2v", 
                                        "iii. Mulheres que têm Boletim de Ocorrência por violência e notificação de violência no SINAN" = "g3v"
                                      ),
                                      selected = "i. Mulheres com notificação de violência no SINAN, mas sem Boletim de Ocorrência")
                        )
                 )
                 
               )
             )
    ),
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
        title = "Informações do SINAN para mulheres vivas",
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
      box(
        title= "Dado SESED",
        width = 12,
        status = "secondary",
        plotlyOutput(ns("dados_sesed"))
        
      )
    )
  )
}

vivas_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Faixa etaria grafico ----
      output$faixa_etaria_graf <- renderPlotly({
        
        a <-  df_vivas |>
          filter(
            grupo_m_vivas %in% input$filtro_grupo,
           ) |> 
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
          
          tabela_fxetaria <-  df_vivas |>
            filter(
              grupo_m_vivas %in% input$filtro_grupo,
            ) |>
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
        dados_preparados <- df_vivas |>
          filter(
            grupo_m_vivas %in% input$filtro_grupo,
          ) |>
          distinct(par_f, ds_raca, sg_sexo, faixa_etaria_padrao) |> 
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
          
          tabela_raca <-  df_vivas |>
            filter(
              grupo_m_vivas %in% input$filtro_grupo,
            ) |>
            tab_1(ds_raca) 
          
          # Escrever os dados filtrados e modificados no arquivo CSV
          write.csv(tabela_raca, file, row.names = FALSE)
          
        })
      
      
      # SINAN ----
      output$sinan <- renderDataTable({
        tryCatch({
          # Inicializa e tenta obter os dados filtrados
          rel <- vitaltable::rel
          filtered_df <- get(input$evolution_filter)
          
          # Processa os dados com base no filtro escolhido pelo usuário
          tabela_sinan <- df_vivas |>
            filter(grupo_m_vivas %in% input$filtro_grupo) |>
            vitaltable::tab_cat_sinan(filtered_df, input$extrato_sinan_filter, input$valor_sinan_filter) |>
            as.data.frame()
          
          # Aplica transformações condicionais com base no filtro de evolução
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
          
          # Verifica se a tabela está vazia após as transformações
          if (nrow(tabela_sinan) == 0) {
            return(datatable(data.frame(Mensagem = "Não há registros neste grupo"), options = list(dom = 't')))
          }
          
          datatable(tabela_sinan, options = list(pageLength = 20))
        }, error = function(e) {
          # Retorna uma tabela vazia com uma mensagem de erro personalizada se houver falha no processamento
          datatable(data.frame(Mensagem = "Este grupo não contém registros no SINAN Violências"), options = list(dom = 't'))
        })
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
          tabela_sinan <- df_vivas |> 
            filter(grupo_m_vivas %in% input$filtro_grupo) |> 
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
      
      
    }
    
  )
}