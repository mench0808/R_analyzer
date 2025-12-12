# install.packages(c("shiny", "tidyverse", "shinythemes"))

library(shiny)
library(tidyverse)
library(shinythemes) 

### フロントエンド
ui <- fluidPage(
    titlePanel("Shiny application"),
    sidebarLayout(
        sidebarPanel(
            # sliderInput(
            #   inputId = "slider_score",
            #   label = "数値を選択してください",
            #   min = 1,
            #   max = 100,
            #   value = 50
            # ),
            fileInput(
                "file_upload", "csvファイルをアップロードをしてください",
                accept = c("text/csv", ".csv")
            ),
            hr(),
            #ヒストグラムの行列を指定するためのinputを形成
            h3("ヒストグラムにしたい要素を選択してください"),
            uiOutput("hist_var_selector"),
            hr(),
            
            h3("散布図にしたい要素を2つ選択してください"),
            h4("縦軸の要素を選択してください"),
            uiOutput("scatter_ylab_selector"),
            
            h4("横軸の要素を選択してください"),
            uiOutput("scatter_xlab_selector"),
        ),
        mainPanel(
            # h3("選択された値"),
            # textOutput("selected_value"),
            tabsetPanel( 
                id = "main_tabs",
                
                # タブ1: データプレビュー
                tabPanel("データプレビュー",
                         h2("データの先頭と末尾"),
                         tableOutput("data_preview")
                ),
                
                # タブ2: ヒストグラム
                tabPanel("ヒストグラム",
                         h2("ヒストグラム"),
                         plotOutput("histogram_plot")
                ),
                
                #　タブ3:散布図
                tabPanel("散布図",
                         h2("散布図"),
                         plotOutput("scatter_plot")
                )
            )
        )
    )
)


###バックエンド
server <- function(input, output) {
    # ★★★ リアクティブな出力ロジック ★★★
    # output$selected_value にレンダリング（描画）するテキストを定義
    # output$selected_value <- renderText({
    #   
    #   # input$my_slider は、ユーザーがスライダーで選んだ値（リアクティブな値）
    #   # このコードブロックは、input$my_slider が変更されるたびに自動で再実行されます
    #   paste("あなたが選んだ値は", input$slider_score, "です。")
    # })
    
    data_input <- reactive({
        # req() は、ファイルがアップロードされるまで処理を待機させます
        req(input$file_upload)
        
        # CSVファイルを読み込む
        df <- read.csv(input$file_upload$datapath,
                       header = TRUE,
                       stringsAsFactors = FALSE)
        # 列名クリーンアップ（空白や特殊文字対策）
        #name(df) <- make.names(names(df), unique = TRUE)
        
        return(df)
    })
    
    ##データプレビューロジック
    
    #renderTableはreactiveの派生系でdfを返すと勝手に図を作成してくれる
    output$data_preview <- renderTable({
        df <- data_input()
        df_head <- head(df, n=10)
        df_tail <- tail(df, n=4)
        ## headとtailの間に省略記号を挟みたい
        num_cols <- ncol(df)
        separator_data <- rep("...", num_cols)
        separator <- data.frame(as.list(separator_data), stringsAsFactors = FALSE)
        names(separator) <- names(df_head)
        df_bind <- rbind(df_head, separator, df_tail)
        return (df_bind)
    })
    
    # データ読み込み時に列名を取得しておく
    col_names <- reactive({
        # data_input()の結果（整形済みのデータフレーム）に依存する
        req(data_input())
        
        # data_input() が df を返した時点で、列名を取得する
        names(data_input()) 
    })
    
    ##ヒストグラムロジック
    
    #renderUIは返り値をドロップダウンとかに変換してくれる
    #出力されるものは全てoutputのカラムとして保存する。これがないと出力できない。
    output$hist_var_selector <- renderUI({
        selectInput("hist_var", "ヒストグラムの変数を選択 (数値列推奨):", 
                    choices = col_names(), 
                    selected = col_names()[1])
    })
    
    #renderPlotはヒストグラム表示ロジック
    #最後必ずprintする必要あり
    output$histogram_plot <- renderPlot({
        df <- data_input()
        
        req(input$hist_var)
        hist_var_name <- input$hist_var
        
        #aes_stringは変数をxに代入する際に使用
        #関数,使用目的,必要な引数
        #aes() (標準),コードを書いた時点で列名が固定されている場合。,列名（変数）を裸の名前で渡す。例: aes(x = Sepal.Length)
        #aes_string() (非標準),アプリの実行中に列名が文字列として変更される場合。,"列名（変数）を文字列で渡す。例: aes_string(x = ""Sepal.Length"")"
        p <- ggplot(df, aes_string(x = hist_var_name)) +
            geom_histogram(bins = 45, fill = "skyblue", color = "white") +
            labs(title = paste0(hist_var_name, " の度数分布"), 
                 x = hist_var_name,
                 y = "度数")# +
        #theme_minimal()
        
        print(p) #これないとrenderPlotは動かない
    })
    
    ##散布図ロジック
    output$scatter_ylab_selector <- renderUI({
        selectInput("scatter_ylab", "散布図の縦軸の変数を選択 (数値列推奨):", 
                    choices = col_names() 
                    #selected = col_names()[1]
        )
    })
    
    output$scatter_xlab_selector <- renderUI({
        selectInput("scatter_xlab", "散布図の横軸の変数を選択 (数値列推奨):", 
                    choices = col_names() 
                    #selected = col_names()[1]
        )
    })
    
    output$scatter_plot <- renderPlot({
        df <- data_input()
        
        req(input$scatter_xlab, input$scatter_ylab)
        
        x_lab <- input$scatter_xlab
        y_lab <- input$scatter_ylab
        
        p <- ggplot(df, aes_string(x = x_lab, y = y_lab)) +
            geom_point() + 
            labs(
                title = paste0(x_lab, "と", y_lab, "の散布図"),
                x = x_lab,
                y = y_lab
            )
        
        print(p)
    })
}

shinyApp(ui = ui, server = server)
