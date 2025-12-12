# install.packages(c("shiny", "tidyverse", "shinythemes"))

library(shiny)
library(tidyverse)
library(shinythemes) 

### フロントエンド
ui <- fluidPage(
    titlePanel("R_analyze"),
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "file_upload", "csvファイルをアップロードをしてください",
                accept = c("text/csv", ".csv")
            ),
            hr(),
            #　UIの動的制御
            uiOutput("dynamic_sidebar_ui"),
        ),
        mainPanel(
            conditionalPanel(
                condition = "output.data_loaded == false", # Serverからのフラグがfalseのとき
                wellPanel(
                    h1("📊 データ分析を開始する"),
                    p("左側のサイドバーからCSVファイルをアップロードしてください。"),
                    p("ファイルが読み込まれると、以下のタブが表示されます。")
                )
            ),
            
            conditionalPanel(
                condition = "output.data_loaded == true", # Serverからのフラグがtrueのとき
                tabsetPanel( 
                    id = "main_tabs",
                    
                    # h3("選択された値"),
                    # textOutput("selected_value"),
                    
                    # タブ1: データプレビュー
                    tabPanel("データプレビュー",
                             h2("データの先頭と末尾"),
                             tableOutput("data_preview"),
                             hr(),
                             h3("使用された R コード (ggplot2)"),
                             htmlOutput("preview_code_output")
                    ),
                    
                    # タブ2: ヒストグラム
                    tabPanel("ヒストグラム",
                             h2("ヒストグラム"),
                             plotOutput("histogram_plot"),
                             hr(),
                             h3("使用された R コード (ggplot2)"),
                             htmlOutput("hist_code_output")
                    ),
                    
                    #　タブ3:散布図
                    tabPanel("散布図",
                             h2("散布図"),
                             plotOutput("scatter_plot"),
                             hr(),
                             h3("使用された R コード (ggplot2)"),
                             htmlOutput("scatter_code_output")
                    ),
                    
                    #  タブ4:箱ひげ図
                    tabPanel("箱ひげ図",
                             h2("箱ひげ図"),
                             h2("この機能は好都合に未完成"),
                             plotOutput("box_plot")
                    )
                )
            )
        )
    )
)


###　バックエンド
server <- function(input, output) {
    
    data_input <- reactive({
        # req() は、ファイルがアップロードされるまで処理を待機させる
        req(input$file_upload)
        
        # CSVファイルを読み込む
        df <- read.csv(input$file_upload$datapath,
                       header = TRUE,
                       stringsAsFactors = FALSE)
        # 列名クリーンアップ（空白や特殊文字対策）
        #name(df) <- make.names(names(df), unique = TRUE)
        
        return(df)
    })
    
    # データ読み込み時に列名を取得しておく
    col_names <- reactive({
        # data_input()の結果（整形済みのデータフレーム）に依存する
        req(data_input())
        
        # data_input() が df を返した時点で、列名を取得する
        names(data_input()) 
    })
    
    ## 動的画面切り替えロジック
    
    output$data_loaded <- reactive({
        # data_input()がエラーなく実行できる（＝ファイルがアップロードされた）場合に TRUE を返す
        return(!is.null(data_input())) 
    })
    
    # このフラグを conditionalPanel で使えるようにする設定
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    output$dynamic_sidebar_ui <- renderUI({
        req(data_input())
        current_tab <- input$main_tabs 
        
        if (current_tab == "ヒストグラム") {
            tagList( 
                h3("ヒストグラム設定"),
                selectInput("hist_var", "ヒストグラムの変数を選択:", 
                            choices = col_names(), 
                            selected = col_names()[1])
            )
        } else if (current_tab == "散布図") {
            tagList(
                h3("散布図設定"),
                selectInput("scatter_ylab", "縦軸 (Y) の変数を選択:", 
                            choices = col_names(), 
                            selected = col_names()[2]), 
                selectInput("scatter_xlab", "横軸 (X) の変数を選択:", 
                            choices = col_names(), 
                            selected = col_names()[1])
            )
        } else if (current_tab == "箱ひげ図") {
            tagList(
                h3("箱ひげ図設定"),
                # selectInput("box_ylab", "縦軸 (Y) の変数を選択:", 
                #             choices = col_names(), 
                #             selected = col_names()[2]), 
                # selectInput("box_xlab", "横軸 (X) の変数を選択:", 
                #             choices = col_names(), 
                #             selected = col_names()[1])
                
            )
        }
    })
    
    ##　データプレビューロジック
    
    #renderTableはreactiveの派生系でdfを返すと勝手に図を作成してくれる
    output$data_preview <- renderTable({
        
        req(data_input())
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
    
    output$preview_code_output <- renderUI({
        # データがロードされるまで待機（データプレビューテーブルと同じ）
        req(data_input())
        
        # Rコード文字列を生成
        code_str <- paste0(
            "# データの先頭（10行）を表示", "\n",
            "head(df, n = 10)", "\n\n",
            "# データの末尾（4行）を表示", "\n",
            "tail(df, n = 4)"
        )
        
        # HTMLタグでコードを囲んで出力
        code_html <- paste0("<pre><code>", code_str, "</code></pre>")
        return(HTML(code_html)) 
    })
    
    
    ##　ヒストグラムロジック
    
    #renderPlotはヒストグラム表示ロジック
    #最後必ずprintする必要あり
    output$histogram_plot <- renderPlot({
        
        req(input$hist_var)
        df <- data_input()
        # req(input$hist_var)
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
    
    output$hist_code_output <- renderUI({
        req(input$hist_var)
        
        var_name <- input$hist_var
        
        # Rコード文字列を生成 (以前と同じ)
        code_str <- paste0(
            "# データの読み込みは省略しています", "\n",
            "ggplot(データ名, aes(x = ", var_name, ")) +", "\n",
            "  geom_histogram(bins = 45, fill = \"skyblue\", color = \"white\") +", "\n",
            "  labs(", "\n",
            "    title = \"", var_name, " の度数分布\",", "\n",
            "    x = \"", var_name, "\",", "\n",
            "    y = \"度数\"", "\n",
            "  ) +", "\n",
            "  theme_minimal()"
        )
        
        # preタグ：改行とスペースを保持
        # codeタグ：コードであることを示す
        code_html <- paste0("<pre><code>", code_str, "</code></pre>")
        
        # HTML()で、Rがこの文字列をHTMLコードとして解釈するように指定
        return(HTML(code_html)) 
    })
    
    ##　散布図ロジック
    
    output$scatter_plot <- renderPlot({
        
        req(input$scatter_xlab, input$scatter_ylab)
        df <- data_input()
        
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
    
    output$scatter_code_output <- renderUI({
        # データの有無と、X軸・Y軸の入力値の有無をチェック
        validate(
            need(input$scatter_xlab, NULL),
            need(input$scatter_ylab, NULL)
        )
        
        x_lab <- input$scatter_xlab
        y_lab <- input$scatter_ylab
        
        # Rコード文字列を生成
        code_str <- paste0(
            "# データの読み込みは省略しています", "\n",
            "ggplot(data, aes(x = ", x_lab, ", y = ", y_lab, ")) +", "\n",
            "  geom_point() +", "\n",
            "  labs(", "\n",
            "    title = \"", x_lab, " vs ", y_lab, " の散布図\",", "\n",
            "    x = \"", x_lab, "\",", "\n",
            "    y = \"", y_lab, "\"", "\n",
            "  ) +", "\n",
            "  theme_minimal()"
        )
        
        # HTMLタグでコードを囲んで出力
        code_html <- paste0("<pre><code>", code_str, "</code></pre>")
        return(HTML(code_html)) 
    })
    
    ##　箱ひげ図
    
    output$box_plot <- renderPlot({
        df <- data.frame(
            Group = factor(rep(c("A", "B", "C"), each = 10)), 
            Value = c(runif(10, 10, 20), runif(10, 15, 25), runif(10, 20, 30)),
            stringsAsFactors = FALSE
        )
        
        x_lab <- "Group"
        y_lab <- "Value"
        
        p <- ggplot(df, aes_string(x = x_lab, y = y_lab, fill = x_lab)) +
            geom_boxplot() +
            labs(
                title = paste0("デモ箱ひげ図 (固定データ)"),
                x = x_lab,
                y = y_lab
            ) +
            theme_minimal() +
            theme(legend.position = "none")
        
        print(p)
    })
}

shinyApp(ui = ui, server = server)