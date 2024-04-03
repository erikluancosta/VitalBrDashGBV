library(sf)
library(DT)

capa_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    tabItem(tabName="inicio",
            box(
              collapsible = FALSE,
              h3("Introdução ao estudo"),
              p("O presente estudo foi realizado em parceria com a Secretaria Estadual de Saúde do Estado
            do Rio Grande do Norte (SESAP/RN) e teve como principal objetivo parear e analisar diferentes
            fontes de dados disponíveis nos sistemas de informação do Estado. Para isso, foi realizado um
            linkage determinístico entre cinco importantes bases de dados de três setores do governo: da
            Saúde foram utilizados o SINAN Violências, SIH (Sistema de Informações Hospitalares) e SIM
            (Sistema de Informações sobre Mortalidade); da Segurança Pública, foram utilizados os dados de
            Boletim de Ocorrência registrados pela SESED (Secretaria de Estado da Segurança Pública e da
            Defesa Social) e da Assistência Social foram utilizados os dados do Cadastro Único (CadÚnico). O
            pareamento destes bancos de dados busca possibilitar a análise da trajetória de mulheres que
            foram vítimas de violência entre os serviços de saúde, segurança pública e assistência,
            possibilitando o desenho de hipóteses sobre o comportamento das vítimas na busca por serviços
            públicos, o funcionamento e eficiência da rede de atendimento, mas também a análise da
            qualidade dos registros nos diferentes setores do governo."),
            
            p("As análises descritas neste relatório foram divididas nas seguintes etapas: (i) breve análise
            descritiva da base pareada, com o objetivo de apresentar um panorama geral dos dados entre os
            diferentes sistemas; (ii) análise de coortes, com a realização de análises de diferentes grupos de
            mulheres a partir de características comuns; (iii) análises epidemiológicas. Para este trabalho,
            foram desenvolvidas funções auxiliares em linguagem R para facilitar a análise dos dados e gerar
            tabelas padronizadas."),
            
            p("Essa abordagem potencializa análises inovadoras sobre as experiências e desafios
            enfrentados pelas mulheres vítimas de violência e pelos profissionais da rede de atendimento no
            Rio Grande do Norte. Estas análises também têm o potencial de informar decisões estratégicas e
            políticas nos diferentes setores de governo, visando aprimorar a assistência e prevenção, bem
            como fortalecer as respostas apropriadas a essa questão sensível e crucial."),
            
            p(""),
            p(""),
            h3("Análises do banco de dados pareados"),
            
            h5('Exploração da base pareada - sexo feminino de 2019 a 2021'),
            
            p("As bases de dados foram disponibilizadas por diferentes setores do governo – saúde,
            segurança e assistência – para diferentes períodos temporais, de acordo com a disponibilidade
            de dados e o histórico de cada um dos sistemas. No entanto, os sistemas possuem quantidades
            de registros diversas ao longo dos anos: alguns sistemas datam de 2010, como o SINAN, enquanto
            outros passam a ter um volume maior de registros em períodos mais recentes, como o da
            Segurança Pública. Por isso, o estudo foi realizado para o período de três anos (2019-2021), por
            ser o período com maior completude de informações entre os diferentes sistemas."),
            
            h5("Tabela 1: Distribuição de registros por ano e por banco de dados. Sexo feminino, Rio Grande do Norte de 2019 a 2021."),
            
            title = "",
            status = "info",
            maximizable = FALSE,
            closable = FALSE,
            #collapsible = TRUE,
            #collapsed = TRUE,
            #solidHeader = TRUE,
            width = 12,
            dataTableOutput(ns("bancos")),
            p(""),
            h3("Faça download do relatório completo no link abaixo:"),
            p(""),
            downloadButton(outputId = ns("download_relatorio"), label = "Download do relatório completo")
            )
    )
  )
}

capa_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # SINAN ----
      output$bancos <- renderDataTable({
        # Verifica se 'df_2_' e os parâmetros estão corretos para 'tab_cat_sinan'
        tabela_bancos <- df_2_ |> 
          vitaltable::tab_2(ano,banco) |> 
          arrange(ano) |> 
          as.data.frame() |>
          rename("Ano" = ano)
        
        # Renderiza a tabela
        datatable(tabela_bancos, options = list(pageLength = 4))
      })
      
      output$download_relatorio <- downloadHandler(
        filename = function() {
          "analise_linkage_rn.docx"
        },
        content = function(file) {
          # Copiar o arquivo para o caminho de destino
          file.copy(from = "dados/analise_linkage_rn.docx", to = file)
        }
      )}
  )}