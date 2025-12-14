# ui.R

library(shiny)
library(tidyverse)
library(bslib)

### フロントエンド
ui <- page_sidebar(
    
    side = "left",
    width = "10%",
    theme = bs_theme(
        bootswatch = "cerulean",
        bg = "#FFFFFF",
        fg = "#212529",
        primary = "#007bff"
    ),
    
    title = "R_analyze",
    
    sidebar = sidebar(
        title = "データ操作",
        
        fileInput(
            "file_upload", "csvファイルをアップロードをしてください",
            accept = c("text/csv", ".csv")
        ),
        hr(),
        uiOutput("dynamic_sidebar_ui"),
    ),
    
    # メインコンテンツ
    conditionalPanel(
        condition = "output.data_loaded == false",
        div(
            class = "text-center py-5",
            h1("データ分析を開始する", class = "display-4"),
            p("左側のサイドバーからCSVファイルをアップロードしてください。", class = "lead"),
            p("ファイルが読み込まれると、グラフのタブが表示されます。", class = "text-muted")
        )
    ),
    
    conditionalPanel(
        condition = "output.data_loaded == true",
        tabsetPanel(
            id = "main_tabs",
            
            # タブ1: データプレビュー
            tabPanel("データプレビュー",
                     h2("データの先頭と末尾", class = "my-3"),
                     tableOutput("data_preview"),
                     hr(),
                     h3("使用された R コード", class = "mt-4"),
                     htmlOutput("preview_code_output")
            ),
            
            # タブ2: ヒストグラム
            tabPanel("ヒストグラム",
                     h2("ヒストグラム", class = "my-3"),
                     plotOutput("histogram_plot"),
                     hr(),
                     h3("使用された R コード (ggplot2)", class = "mt-4"),
                     htmlOutput("hist_code_output")
            ),
            
            # タブ3:散布図
            tabPanel("散布図",
                     h2("散布図", class = "my-3"),
                     plotOutput("scatter_plot"),
                     hr(),
                     h3("使用された R コード (ggplot2)", class = "mt-4"),
                     htmlOutput("scatter_code_output")
            ),
            
            # タブ4:箱ひげ図
            tabPanel("箱ひげ図",
                     h2("箱ひげ図", class = "my-3"),
                     h2("この機能は好都合に未完成"),
                     plotOutput("box_plot")
            )
        )
    )
)