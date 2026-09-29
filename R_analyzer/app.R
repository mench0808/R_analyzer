library(shiny)
library(ggplot2)
library(dplyr)
library(broom)
library(tidyr)
library(lme4)
library(survival)

# -------------------------------------------------------------
# GLOBAL HELPER FUNCTIONS
# -------------------------------------------------------------
reshape_wide_to_long <- function(df, ans_cols, attributes, alt1_format, alt2_format, alt1_match_val, alt2_match_val, match_type = "exact", covariates = NULL) {
  task_dfs <- lapply(seq_along(ans_cols), function(idx) {
    t_val <- idx - 1  # 0-indexed task ID
    current_ans <- ans_cols[idx]
    
    # Construct column names for this task
    attr_cols_1 <- unname(sapply(attributes, function(attr) {
      col <- alt1_format
      col <- gsub("\\{attribute\\}", attr, col)
      col <- gsub("\\{task\\}", as.character(idx), col)
      col
    }))
    
    attr_cols_2 <- unname(sapply(attributes, function(attr) {
      col <- alt2_format
      col <- gsub("\\{attribute\\}", attr, col)
      col <- gsub("\\{task\\}", as.character(idx), col)
      col
    }))
    
    # Check if all constructed columns exist in df
    missing_cols_1 <- attr_cols_1[!attr_cols_1 %in% names(df)]
    missing_cols_2 <- attr_cols_2[!attr_cols_2 %in% names(df)]
    
    if (length(missing_cols_1) > 0 || length(missing_cols_2) > 0) {
      stop(paste("列名パターンに一致する列が見つかりません。現在のCSVに含まれる列名か確認してください。\n見つからない列:", 
                 paste(c(missing_cols_1, missing_cols_2), collapse = ", ")))
    }
    
    # Select columns
    keep_cols <- c(current_ans, attr_cols_1, attr_cols_2)
    if (!is.null(covariates)) {
      valid_covariates <- covariates[covariates %in% names(df)]
      keep_cols <- c(keep_cols, valid_covariates)
    } else {
      valid_covariates <- character(0)
    }
    
    # Extract sliced df
    task_df <- df %>%
      mutate(task_id = t_val,
             res_id = row_number()) %>%
      select(res_id, task_id, ans = all_of(current_ans), all_of(attr_cols_1), all_of(attr_cols_2), all_of(valid_covariates))
    
    # Helper to check choice matches
    is_match <- function(val, match_val) {
      if (is.na(val) || val == "") return(FALSE)
      if (match_type == "exact") {
        return(val == match_val)
      } else if (match_type == "contains") {
        return(grepl(match_val, val, fixed = TRUE))
      } else if (match_type == "regex") {
        return(grepl(match_val, val, perl = TRUE))
      }
      return(FALSE)
    }
    
    # Filter for valid choices only
    task_df$is_alt1 <- sapply(task_df$ans, is_match, match_val = alt1_match_val)
    task_df$is_alt2 <- sapply(task_df$ans, is_match, match_val = alt2_match_val)
    
    task_df <- task_df %>% filter(is_alt1 | is_alt2)
    
    if (nrow(task_df) == 0) {
      return(NULL)
    }
    
    # Rename columns to standard form before pivoting
    task_df_renamed <- task_df
    for (i in seq_along(attributes)) {
      attr <- attributes[i]
      col1 <- attr_cols_1[i]
      col2 <- attr_cols_2[i]
      names(task_df_renamed)[names(task_df_renamed) == col1] <- paste0(attr, "_alt1")
      names(task_df_renamed)[names(task_df_renamed) == col2] <- paste0(attr, "_alt2")
    }
    
    pivot_cols <- c(paste0(attributes, "_alt1"), paste0(attributes, "_alt2"))
    
    long_task_df <- task_df_renamed %>%
      pivot_longer(
        cols = all_of(pivot_cols),
        names_to = c(".value", "alt"),
        names_pattern = "(.*)_(alt1|alt2)"
      ) %>%
      # Add choice column
      mutate(choice = case_when(
        is_alt1 & alt == "alt1" ~ 1,
        is_alt2 & alt == "alt2" ~ 1,
        TRUE ~ 0
      )) %>%
      select(-is_alt1, -is_alt2)
    
    return(long_task_df)
  })
  
  task_dfs <- task_dfs[!sapply(task_dfs, is.null)]
  if (length(task_dfs) == 0) {
    stop("有効な回答を含むタスクが見つかりませんでした。回答判定条件が正しいか確認してください。")
  }
  
  combined_df <- do.call(rbind, task_dfs)
  rownames(combined_df) <- NULL
  
  # Remove rows with NA or empty strings in attributes and covariates
  clean_cols <- c(attributes, covariates[covariates %in% names(combined_df)])
  combined_df <- combined_df %>%
    filter(if_all(all_of(clean_cols), ~ .x != "" & !is.na(.x)))
  
  return(combined_df)
}

# Set high-quality styling defaults for ggplot2
theme_set(theme_minimal(base_size = 12))

# -------------------------------------------------------------
# USER INTERFACE (UI)
# -------------------------------------------------------------
ui <- fluidPage(
  # Load beautiful fonts
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Outfit:wght@400;600;800&display=swap"),
    tags$style(HTML("
      /* Premium Styling & Theme Override */
      body {
        background-color: #f6f8fb;
        font-family: 'Inter', sans-serif;
        color: #2c3e50;
      }
      
      h1, h2, h3, h4 {
        font-family: 'Outfit', sans-serif;
        font-weight: 600;
        color: #1a365d;
      }
      
      .title-banner {
        background: linear-gradient(135deg, #1e3c72, #2a5298);
        color: white;
        padding: 30px 25px;
        border-radius: 16px;
        margin-bottom: 25px;
        box-shadow: 0 4px 15px rgba(30, 60, 114, 0.15);
      }
      
      .title-banner h1 {
        color: white;
        margin-top: 0;
        font-size: 2.2rem;
        font-weight: 800;
      }
      
      .title-banner p {
        font-size: 1.1rem;
        opacity: 0.9;
        margin-bottom: 0;
      }
      
      /* Card Layouts */
      .card {
        background: white;
        border-radius: 12px;
        border: 1px solid #e2e8f0;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05), 0 2px 4px -1px rgba(0, 0, 0, 0.03);
        padding: 24px;
        margin-bottom: 24px;
      }
      
      .card-title {
        font-size: 1.25rem;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 20px;
        padding-bottom: 12px;
        border-bottom: 2px solid #edf2f7;
        color: #2b6cb0;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      
      /* Value Box / KPI styling */
      .kpi-container {
        display: flex;
        gap: 16px;
        margin-bottom: 20px;
        flex-wrap: wrap;
      }
      
      .kpi-card {
        flex: 1;
        min-width: 180px;
        background: white;
        border-radius: 10px;
        border: 1px solid #e2e8f0;
        padding: 16px 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.02);
      }
      
      .kpi-value {
        font-size: 1.8rem;
        font-weight: 800;
        color: #2b6cb0;
        line-height: 1.2;
      }
      
      .kpi-label {
        font-size: 0.85rem;
        color: #718096;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        margin-top: 4px;
      }
      
      /* Control styling */
      .well {
        background-color: white !important;
        border: 1px solid #e2e8f0 !important;
        border-radius: 12px !important;
        box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.05) !important;
        padding: 20px !important;
      }
      
      /* Button custom styles */
      .btn-primary-custom {
        background: linear-gradient(135deg, #3182ce, #2b6cb0);
        color: white;
        border: none;
        border-radius: 8px;
        padding: 12px 20px;
        font-weight: 600;
        width: 100%;
        transition: all 0.2s ease;
        box-shadow: 0 4px 6px rgba(49, 130, 206, 0.2);
      }
      
      .btn-primary-custom:hover {
        background: linear-gradient(135deg, #2b6cb0, #2c5282);
        color: white;
        transform: translateY(-1px);
        box-shadow: 0 6px 12px rgba(49, 130, 206, 0.3);
      }
      
      .btn-secondary-custom {
        background: #edf2f7;
        color: #4a5568;
        border: 1px solid #cbd5e0;
        border-radius: 8px;
        padding: 8px 16px;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      
      .btn-secondary-custom:hover {
        background: #e2e8f0;
        color: #2d3748;
      }
      
      /* Custom Table View */
      .table-responsive {
        overflow-x: auto;
      }
      
      .shiny-table {
        width: 100% !important;
        border-collapse: collapse;
      }
      
      .shiny-table th {
        background-color: #f7fafc !important;
        color: #4a5568 !important;
        font-weight: 600 !important;
        border-bottom: 2px solid #edf2f7 !important;
        padding: 12px 16px !important;
        text-align: left;
      }
      
      .shiny-table td {
        padding: 12px 16px !important;
        border-bottom: 1px solid #edf2f7 !important;
        color: #2d3748 !important;
      }
      
      /* Help block text */
      .help-block-custom {
        font-size: 0.85rem;
        color: #718096;
        margin-top: 6px;
      }
      
      /* Tab panels */
      .nav-tabs {
        border-bottom: 2px solid #e2e8f0;
        margin-bottom: 20px;
      }
      
      .nav-tabs > li > a {
        font-weight: 600;
        color: #718096;
        border: none;
        padding: 12px 20px;
        border-radius: 0;
        margin-right: 10px;
        transition: all 0.2s;
      }
      
      .nav-tabs > li > a:hover {
        background: none;
        color: #2b6cb0;
        border-bottom: 3px solid #cbd5e0;
      }
      
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:hover, 
      .nav-tabs > li.active > a:focus {
        border: none;
        border-bottom: 3px solid #2b6cb0;
        color: #2b6cb0 !important;
        background: none !important;
      }
      
      .welcome-banner {
        text-align: center;
        padding: 60px 40px;
        background: white;
        border-radius: 16px;
        border: 2px dashed #cbd5e0;
      }
    "))
  ),
  
  # Banner Header
  div(class = "title-banner",
      h1("Conjoint Analysis Dashboard"),
      p("CSVファイルを読み込むだけで、自動で部分効用値（Part-worth Utilities）、重要度、および有意水準・信頼区間付きの係数プロット（Coefplot）を算出・可視化します。")
  ),
  
  sidebarLayout(
    # -------------------------------------------------------------
    # SIDEBAR PANEL (Controls)
    # -------------------------------------------------------------
    sidebarPanel(
      width = 4,
      div(class = "card-title", "1. データソース設定"),
      
      fileInput("file1", "CSVファイルをアップロード",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                buttonLabel = "ファイル選択",
                placeholder = "ファイルをドラッグ＆ドロップ"),
      
      # Sample Data Trigger Buttons
      fluidRow(
        column(6, actionButton("btn_sample", "サンプル(縦持ち)", 
                               class = "btn btn-default btn-secondary-custom", 
                               icon = icon("table"), style = "width: 100%; margin-bottom: 15px; font-size: 0.8rem; padding: 6px 2px;")),
        column(6, actionButton("btn_sample_wide", "サンプル(横持ち)", 
                               class = "btn btn-default btn-secondary-custom", 
                               icon = icon("table"), style = "width: 100%; margin-bottom: 15px; font-size: 0.8rem; padding: 6px 2px;"))
      ),
      
      # CSV Parsing Settings (Collapsible to keep UI clean)
      tags$details(
        tags$summary("詳細インポート設定 (区切り文字・エンコーディング等)", style = "cursor: pointer; color: #4a5568; font-weight: 500; margin-bottom: 15px; font-size: 0.9rem;"),
        div(style = "padding-top: 10px;",
            selectInput("encoding", "ファイルエンコーディング",
                        choices = c("UTF-8" = "UTF-8", "Shift-JIS (CP932)" = "CP932", "EU-JP" = "EUC-JP")),
            checkboxInput("header", "1行目を列名（ヘッダー）とする", TRUE),
            radioButtons("sep", "区切り文字",
                         choices = c("カンマ (,)" = ",",
                                     "セミコロン (;)" = ";",
                                     "タブ (\\t)" = "\t"),
                         selected = ","),
            radioButtons("quote", "引用符",
                         choices = c("二重引用符 (\")" = '"',
                                     "一重引用符 (\')" = "'",
                                     "なし" = ""),
                         selected = '"')
        )
      ),
      
      # Dynamic Analysis Configuration (rendered when data exists)
      uiOutput("analysis_controls")
    ),
    
    # -------------------------------------------------------------
    # MAIN PANEL (Tabs / Visuals)
    # -------------------------------------------------------------
    mainPanel(
      width = 8,
      
      # Conditional Landing UI if no data is loaded
      uiOutput("main_content_ui")
    )
  )
)

# -------------------------------------------------------------
# SERVER LOGIC
# -------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive values to hold different stages of data
  raw_data_holder <- reactiveVal(NULL)
  cleaned_data_holder <- reactiveVal(NULL)
  data_holder <- reactiveVal(NULL)
  
  # Reactive value to track if the current data is the sample data
  is_sample_active <- reactiveVal(FALSE)
  
  # 1a. Load Long Sample Data
  observeEvent(input$btn_sample, {
    tryCatch({
      # Shiny sets wd to app.R folder, so sample_data.csv should be right there.
      path <- "sample_data.csv"
      if (!file.exists(path)) {
        path <- "/Users/tamagakisou/R_dashbord/sample_data.csv"
      }
      
      if (file.exists(path)) {
        df <- read.csv(path, header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
        raw_data_holder(df)
        cleaned_data_holder(df)
        data_holder(df)
        is_sample_active(TRUE)
        showNotification("サンプルデータ(縦持ち)をロードしました。", type = "message")
      } else {
        showNotification("サンプルデータファイルが見つかりません。", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("エラーが発生しました:", e$message), type = "error")
    })
  })
  
  # 1b. Load Wide Sample Data
  observeEvent(input$btn_sample_wide, {
    tryCatch({
      path <- "sample_data_wide.csv"
      if (!file.exists(path)) {
        path <- "/Users/tamagakisou/R_dashbord/sample_data_wide.csv"
      }
      
      if (file.exists(path)) {
        df <- read.csv(path, header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
        raw_data_holder(df)
        cleaned_data_holder(df)
        data_holder(df)
        is_sample_active(TRUE)
        showNotification("サンプルデータ(横持ち)をロードしました。「データ編集」タブでクレンジングと縦持ち変換をテストできます。", type = "message")
      } else {
        showNotification("サンプルデータ(横持ち)ファイルが見つかりません。", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("エラーが発生しました:", e$message), type = "error")
    })
  })
  
  # 2. Load Uploaded File
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      # Load file with specified encoding and options
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote,
                     fileEncoding = input$encoding,
                     stringsAsFactors = FALSE)
      raw_data_holder(df)
      cleaned_data_holder(df)
      data_holder(df)
      is_sample_active(FALSE)
      showNotification("CSVファイルを正常に読み込みました。", type = "message")
    }, error = function(e) {
      showNotification(paste("CSVの読み込みに失敗しました。エンコーディング等の設定を確認してください。\nエラー:", e$message), type = "error", duration = 8)
    })
  })
  
  # 3. Dynamic UI Controls for Analysis
  
  output$analysis_controls <- renderUI({
    req(data_holder())
    df <- data_holder()
    cols <- names(df)
    
    # Auto-guess default Y and X variables
    default_y <- if("Rating" %in% cols) "Rating" else if("Choice" %in% cols) "Choice" else if("choice" %in% cols) "choice" else cols[length(cols)]
    default_x <- cols[!cols %in% c("Respondent_ID", "Profile_ID", "Respondent", "Profile", "res_id", "task_id", "ans", "alt", default_y)]
    
    tagList(
      hr(),
      div(class = "card-title", "2. 分析設定"),
      
      selectInput("y_var", "被説明変数 (Y / 評価値・選択フラグ)", 
                  choices = cols, selected = default_y),
      
      selectizeInput("x_vars", "説明変数 (X / 属性群)", 
                     choices = cols, selected = character(0),
                     multiple = TRUE, 
                     options = list(placeholder = '分析に含める属性を選択...')),
      div(style = "display: flex; gap: 8px; margin-top: -10px; margin-bottom: 12px; justify-content: flex-end;",
          actionButton("btn_select_all_x", "全て選択", class = "btn btn-default btn-xs btn-secondary-custom", style = "padding: 2px 8px; font-size: 0.75rem;"),
          actionButton("btn_clear_x", "クリア", class = "btn btn-default btn-xs btn-secondary-custom", style = "padding: 2px 8px; font-size: 0.75rem;")
      ),
      uiOutput("ref_levels_ui"),
      p(class = "help-block-custom", "※選択された説明変数は、すべて質的カテゴリ（要因）として自動処理されます。"),
      
      radioButtons("model_type", "回帰モデルの選択",
                   choices = c("線形回帰 (Rating/Ranking等)" = "lm",
                               "ロジスティック回帰 (Choice 0/1)" = "glm",
                               "混合効果ロジスティック回帰 (lme4::glmer)" = "glmer",
                               "条件付きロジスティック回帰 (survival::clogit)" = "clogit"),
                   selected = if(default_y %in% c("Choice", "choice")) "glm" else "lm"),
      
      conditionalPanel(
        condition = "input.model_type == 'glmer' || input.model_type == 'clogit'",
        selectInput("res_id_col", "回答者ID (Respondent ID) 列", choices = cols, selected = if("res_id" %in% cols) "res_id" else cols[1])
      ),
      conditionalPanel(
        condition = "input.model_type == 'clogit'",
        selectInput("task_id_col", "タスクID (Task ID) 列", choices = cols, selected = if("task_id" %in% cols) "task_id" else cols[1])
      ),
      
      radioButtons("contrast_type", "ダミー化の手法",
                   choices = c("効果コード化 (効用和=0, 平均値比)" = "sum",
                               "ダミーコード化 (基準水準との比較)" = "treatment"),
                   selected = "treatment"),
      
      sliderInput("conf_level", "信頼区間の信頼水準",
                  min = 0.80, max = 0.99, value = 0.95, step = 0.01),
      
      actionButton("btn_run", "コンジョイント分析を実行",
                   class = "btn-primary-custom",
                   icon = icon("play"))
    )
  })
  
  # Conjoint results reactive state
  conjoint_data <- reactiveValues(results = NULL, error_msg = NULL, warnings = NULL)
  
  observeEvent(input$btn_run, {
    req(data_holder(), input$y_var, input$x_vars)
    df <- data_holder()
    y_var <- input$y_var
    x_vars <- input$x_vars
    model_type <- input$model_type
    contrast_type <- input$contrast_type
    conf_level <- input$conf_level
    
    # Validations
    if(length(x_vars) == 0) {
      conjoint_data$results <- NULL
      conjoint_data$warnings <- NULL
      conjoint_data$error_msg <- "説明変数（属性）を少なくとも1つ選択してください。"
      showNotification("エラー：説明変数を選択してください。", type = "error")
      return()
    }
    
    withProgress(message = 'コンジョイント分析を実行中...', value = 0, {
      tryCatch({
        # Pre-checks before subsetting or fitting
        # 1. Column existence check
        req_cols <- c(y_var, x_vars)
        if (model_type %in% c("glmer", "clogit")) {
          req_cols <- c(req_cols, input$res_id_col)
        }
        if (model_type == "clogit") {
          req_cols <- c(req_cols, input$task_id_col)
        }
        req_cols <- unique(req_cols)
        
        missing_cols <- req_cols[!req_cols %in% names(df)]
        if (length(missing_cols) > 0) {
          stop(paste0("選択された変数の一部がデータセットに見つかりません: [", 
                      paste(missing_cols, collapse = ", "), 
                      "]。別のCSVファイルをアップロードした場合は、左側の選択ボックスで変数を再設定してください。"))
        }
        
        # 2. Y-Variable Numeric & Variance check
        numeric_y <- suppressWarnings(as.numeric(as.character(df[[y_var]])))
        non_na_y <- numeric_y[!is.na(numeric_y)]
        if (length(non_na_y) == 0) {
          stop(paste0("被説明変数(Y)「", y_var, "」を数値として解釈できません。不要なヘッダー行や説明行（Qualtricsメタデータ等）が残っていないか、「データ編集」タブで確認してください。"))
        }
        
        unique_y <- unique(non_na_y)
        if (length(unique_y) < 2) {
          stop(paste0("被説明変数(Y)「", y_var, "」の値が一種類しかありません（値: ", paste(unique_y, collapse = ", "), "）。分析には異なる複数の回答値が必要です。"))
        }
        
        # 3. Y-Variable Binary check for glm, glmer, clogit
        if (model_type %in% c("glm", "glmer", "clogit")) {
          invalid_y_vals <- unique_y[!unique_y %in% c(0, 1)]
          if (length(invalid_y_vals) > 0) {
            stop(paste0("選択されたモデル（", model_type, "）の実行には、被説明変数(Y)の値が 0 または 1 のバイナリデータである必要があります。現在のデータには以下の値が含まれています: [", 
                        paste(unique_y, collapse = ", "), 
                        "]。線形回帰(lm)を使用するか、「データ編集」タブ等でデータを0/1に変換してください。"))
          }
        }
        
        # 4. Explanatory Variables level count check
        for(x in x_vars) {
          vals <- as.character(df[[x]])
          unique_vals <- unique(vals)
          unique_vals <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
          if(length(unique_vals) < 2) {
            stop(paste0("属性「", x, "」には有効な水準が1つしか存在しません。分析には少なくとも2つ以上の水準が必要です。"))
          }
        }
        
        # 5. ID Column checks for glmer / clogit
        if (model_type %in% c("glmer", "clogit")) {
          res_id_col <- input$res_id_col
          if (is.null(res_id_col) || res_id_col == "" || !res_id_col %in% names(df)) {
            stop("混合効果モデルまたは条件付きロジスティック回帰を実行するには、有効な回答者ID列の指定が必要です。")
          }
        }
        if (model_type == "clogit") {
          task_id_col <- input$task_id_col
          if (is.null(task_id_col) || task_id_col == "" || !task_id_col %in% names(df)) {
            stop("条件付きロジスティック回帰を実行するには、有効なタスクID列の指定が必要です。")
          }
        }
        
        # Data preparation
        analysis_df <- df[, req_cols, drop = FALSE]
        
        # Trim whitespace and convert empty strings to NA
        for(col in names(analysis_df)) {
          if(is.character(analysis_df[[col]])) {
            analysis_df[[col]] <- trimws(analysis_df[[col]])
            analysis_df[[col]][analysis_df[[col]] == ""] <- NA
          } else if(is.factor(analysis_df[[col]])) {
            char_col <- trimws(as.character(analysis_df[[col]]))
            char_col[char_col == ""] <- NA
            analysis_df[[col]] <- as.factor(char_col)
          }
        }
        
        # Coerce Y to numeric
        analysis_df[[y_var]] <- suppressWarnings(as.numeric(as.character(analysis_df[[y_var]])))
        
        n_before <- nrow(analysis_df)
        analysis_df <- na.omit(analysis_df)
        n_after <- nrow(analysis_df)
        
        if(n_after < 5) {
          stop(paste0("有効なデータ行数が少なすぎます（欠損値を除いた結果、", n_after, "行しかありません。5行以上必要です）。"))
        }
        
        # Convert to factors and order levels according to chosen reference
        for(x in x_vars) {
          ref_val <- input[[paste0("ref_", x)]]
          vals <- as.character(analysis_df[[x]])
          unique_vals <- unique(vals)
          unique_vals <- unique_vals[!is.na(unique_vals) & unique_vals != ""]
          
          if(length(unique_vals) < 2) {
            stop(paste0("属性「", x, "」の水準が分析用サブセットで1つになりました。データ数や欠損値を確認してください。"))
          }
          
          if (contrast_type == "treatment") {
            if (!is.null(ref_val) && ref_val %in% unique_vals) {
              other_vals <- unique_vals[unique_vals != ref_val]
              level_order <- c(ref_val, other_vals)
            } else {
              level_order <- unique_vals
            }
          } else { # sum contrasts (reference/omitted level at the end)
            if (!is.null(ref_val) && ref_val %in% unique_vals) {
              other_vals <- unique_vals[unique_vals != ref_val]
              level_order <- c(other_vals, ref_val)
            } else {
              level_order <- unique_vals
            }
          }
          
          analysis_df[[x]] <- factor(vals, levels = level_order)
        }
        
        # Apply contrast coding
        for(x in x_vars) {
          if(contrast_type == "sum") {
            contrasts(analysis_df[[x]]) <- "contr.sum"
          } else {
            contrasts(analysis_df[[x]]) <- "contr.treatment"
          }
        }
        
        # Fit Model and capture warnings
        fit <- NULL
        fit_warnings <- character(0)
        
        fit <- withCallingHandlers(
          expr = {
            if(model_type == "lm") {
              formula_str <- paste(y_var, "~", paste(x_vars, collapse = " + "))
              formula_obj <- as.formula(formula_str)
              lm(formula_obj, data = analysis_df)
            } else if (model_type == "glm") {
              formula_str <- paste(y_var, "~", paste(x_vars, collapse = " + "))
              formula_obj <- as.formula(formula_str)
              glm(formula_obj, data = analysis_df, family = binomial(link = "logit"))
            } else if (model_type == "glmer") {
              res_id_col <- input$res_id_col
              formula_str <- paste(y_var, "~", paste(x_vars, collapse = " + "), "+ (1 |", res_id_col, ")")
              formula_obj <- as.formula(formula_str)
              lme4::glmer(formula_obj, data = analysis_df, family = binomial(link = "logit"))
            } else if (model_type == "clogit") {
              res_id_col <- input$res_id_col
              task_id_col <- input$task_id_col
              formula_str <- paste(y_var, "~", paste(x_vars, collapse = " + "), "+ strata(", res_id_col, ",", task_id_col, ")")
              formula_obj <- as.formula(formula_str)
              survival::clogit(formula_obj, data = analysis_df)
            }
          },
          warning = function(w) {
            fit_warnings <<- c(fit_warnings, w$message)
            invokeRestart("muffleWarning")
          }
        )
        
        # Extract Model metrics
        coef_fit <- if (model_type == "glmer") {
          lme4::fixef(fit)
        } else {
          coef(fit)
        }
        
        # Safe covariance extraction
        vcov_fit <- tryCatch(vcov(fit), error = function(e) NULL)
        summary_fit <- summary(fit)
        
        # Confidence interval critical value
        alpha <- 1 - conf_level
        crit_val <- if(model_type == "lm") {
          if (!is.null(fit$df.residual)) {
            qt(1 - alpha/2, df = fit$df.residual)
          } else {
            qnorm(1 - alpha/2)
          }
        } else {
          qnorm(1 - alpha/2)
        }
        
        # Reconstruct utilities for all levels (including omitted/reference)
        results_list <- list()
        
        for(x in x_vars) {
          lvls <- levels(analysis_df[[x]])
          k <- length(lvls)
          
          attr_results <- data.frame(
            Attribute = x,
            Level = lvls,
            Utility = 0,
            StdError = 0,
            pValue = NA,
            CI_Lower = 0,
            CI_Upper = 0,
            IsReference = FALSE,
            stringsAsFactors = FALSE
          )
          
          if(contrast_type == "treatment") {
            # Dummy Coding (treatment contrasts): Reference is level 1
            attr_results$IsReference[1] <- TRUE
            
            for(i in 2:k) {
              lvl_name <- lvls[i]
              coef_name <- paste0(x, lvl_name)
              
              # Match coefficient name safely
              c_name <- NULL
              if(coef_name %in% names(coef_fit)) {
                c_name <- coef_name
              } else {
                # Try fuzzy matching in case of R escapes
                matching <- names(coef_fit)[grepl(paste0("^", x), names(coef_fit)) & grepl(lvl_name, names(coef_fit), fixed = TRUE)]
                if(length(matching) > 0) c_name <- matching[1]
              }
              
              if(!is.null(c_name) && !is.na(coef_fit[c_name])) {
                val <- coef_fit[c_name]
                se_val <- if(!is.null(vcov_fit) && c_name %in% colnames(vcov_fit)) vcov_fit[c_name, c_name] else NA
                se <- if(!is.na(se_val) && se_val > 0) sqrt(se_val) else NA
                
                pval_col <- if(model_type == "lm") "Pr(>|t|)" else {
                  if ("Pr(>|z|)" %in% colnames(summary_fit$coefficients)) "Pr(>|z|)" else "Pr(>|t|)"
                }
                pval <- if(c_name %in% rownames(summary_fit$coefficients) && pval_col %in% colnames(summary_fit$coefficients)) {
                  summary_fit$coefficients[c_name, pval_col]
                } else {
                  NA
                }
                
                attr_results$Utility[i] <- val
                attr_results$StdError[i] <- se
                attr_results$pValue[i] <- pval
                attr_results$CI_Lower[i] <- if(!is.na(se)) val - crit_val * se else NA
                attr_results$CI_Upper[i] <- if(!is.na(se)) val + crit_val * se else NA
              } else {
                # Perfect collinearity fallback
                attr_results$Utility[i] <- NA
                attr_results$StdError[i] <- NA
                attr_results$pValue[i] <- NA
                attr_results$CI_Lower[i] <- NA
                attr_results$CI_Upper[i] <- NA
              }
            }
          } else {
            # Effect Coding (sum contrasts): Omitted reference is level k
            coef_names <- paste0(x, 1:(k-1))
            
            for(i in 1:(k-1)) {
              c_name <- coef_names[i]
              if(c_name %in% names(coef_fit) && !is.na(coef_fit[c_name])) {
                val <- coef_fit[c_name]
                se_val <- if(!is.null(vcov_fit) && c_name %in% colnames(vcov_fit)) vcov_fit[c_name, c_name] else NA
                se <- if(!is.na(se_val) && se_val > 0) sqrt(se_val) else NA
                
                pval_col <- if(model_type == "lm") "Pr(>|t|)" else {
                  if ("Pr(>|z|)" %in% colnames(summary_fit$coefficients)) "Pr(>|z|)" else "Pr(>|t|)"
                }
                pval <- if(c_name %in% rownames(summary_fit$coefficients) && pval_col %in% colnames(summary_fit$coefficients)) {
                  summary_fit$coefficients[c_name, pval_col]
                } else {
                  NA
                }
                
                attr_results$Utility[i] <- val
                attr_results$StdError[i] <- se
                attr_results$pValue[i] <- pval
                attr_results$CI_Lower[i] <- if(!is.na(se)) val - crit_val * se else NA
                attr_results$CI_Upper[i] <- if(!is.na(se)) val + crit_val * se else NA
              } else {
                attr_results$Utility[i] <- NA
                attr_results$StdError[i] <- NA
                attr_results$pValue[i] <- NA
                attr_results$CI_Lower[i] <- NA
                attr_results$CI_Upper[i] <- NA
              }
            }
            
            # Calculate Omitted level (kth) utility (sum is NA if any is NA)
            if(any(is.na(attr_results$Utility[1:(k-1)]))) {
              attr_results$Utility[k] <- NA
              attr_results$StdError[k] <- NA
              attr_results$pValue[k] <- NA
              attr_results$CI_Lower[k] <- NA
              attr_results$CI_Upper[k] <- NA
            } else {
              u_k <- -sum(attr_results$Utility[1:(k-1)])
              attr_results$Utility[k] <- u_k
              
              # Standard error of omitted level
              valid_coefs <- coef_names[coef_names %in% colnames(vcov_fit)]
              valid_coefs <- valid_coefs[!is.na(coef_fit[valid_coefs])]
              
              if(!is.null(vcov_fit) && length(valid_coefs) > 0 && all(valid_coefs %in% colnames(vcov_fit))) {
                sub_vcov <- vcov_fit[valid_coefs, valid_coefs, drop = FALSE]
                var_k <- sum(sub_vcov, na.rm = TRUE)
                
                if (!is.na(var_k) && var_k > 0) {
                  se_k <- sqrt(var_k)
                  attr_results$StdError[k] <- se_k
                  
                  # p-value of omitted level
                  if (se_k > 0) {
                    t_k <- u_k / se_k
                    pval_k <- if(model_type == "lm") {
                      if (!is.null(fit$df.residual)) {
                        2 * pt(-abs(t_k), df = fit$df.residual)
                      } else {
                        2 * pnorm(-abs(t_k))
                      }
                    } else {
                      2 * pnorm(-abs(t_k))
                    }
                    attr_results$pValue[k] <- pval_k
                    attr_results$CI_Lower[k] <- u_k - crit_val * se_k
                    attr_results$CI_Upper[k] <- u_k + crit_val * se_k
                  } else {
                    attr_results$StdError[k] <- NA
                    attr_results$pValue[k] <- NA
                    attr_results$CI_Lower[k] <- NA
                    attr_results$CI_Upper[k] <- NA
                  }
                } else {
                  attr_results$StdError[k] <- NA
                  attr_results$pValue[k] <- NA
                  attr_results$CI_Lower[k] <- NA
                  attr_results$CI_Upper[k] <- NA
                }
              } else {
                attr_results$StdError[k] <- NA
                attr_results$pValue[k] <- NA
                attr_results$CI_Lower[k] <- NA
                attr_results$CI_Upper[k] <- NA
              }
            }
          }
          results_list[[x]] <- attr_results
        }
        
        utilities_df <- do.call(rbind, results_list)
        rownames(utilities_df) <- NULL
        
        # Calculate Importance (Range of Utilities per Attribute)
        importance_list <- list()
        for(x in x_vars) {
          lvl_utils <- utilities_df$Utility[utilities_df$Attribute == x]
          lvl_utils <- lvl_utils[!is.na(lvl_utils)]
          if (length(lvl_utils) > 0) {
            importance_list[[x]] <- max(lvl_utils) - min(lvl_utils)
          } else {
            importance_list[[x]] <- 0
          }
        }
        ranges <- unlist(importance_list)
        sum_ranges <- sum(ranges)
        
        importance_df <- if(sum_ranges > 0) {
          data.frame(
            Attribute = names(ranges),
            Range = ranges,
            Importance = (ranges / sum_ranges) * 100,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Attribute = names(ranges),
            Range = 0,
            Importance = 0,
            stringsAsFactors = FALSE
          )
        }
        importance_df <- importance_df[order(-importance_df$Importance), ]
        
        # Update reactive values with successful results
        conjoint_data$results <- list(
          utilities = utilities_df,
          importance = importance_df,
          model = fit,
          summary = summary_fit,
          rows_removed = n_before - n_after
        )
        conjoint_data$error_msg <- NULL
        conjoint_data$warnings <- if(length(fit_warnings) > 0) fit_warnings else NULL
        showNotification("分析が完了しました。", type = "message")
      }, error = function(e) {
        conjoint_data$results <- NULL
        conjoint_data$warnings <- NULL
        conjoint_data$error_msg <- e$message
        showNotification("分析中にエラーが発生しました。結果タブを確認してください。", type = "error", duration = 10)
      })
    })
  })
  
  # Helper reactive so other parts calling conjoint_results() continue to work
  conjoint_results <- reactive({
    req(conjoint_data$results)
    conjoint_data$results
  })
  
  # Check if Analysis has been run successfully
  analysis_run <- reactive({
    !is.null(conjoint_data$results)
  })
  
  # 6. Dynamic Main Content UI (Show instructions vs actual tabs)
  output$main_content_ui <- renderUI({
    if(is.null(data_holder())) {
      # Show Welcome landing page
      div(class = "welcome-banner",
          icon("upload", style = "font-size: 4rem; color: #a0aec0; margin-bottom: 20px;"),
          h2("分析データの準備ができていません"),
          p("左側のパネルからご自身のCSVファイルをアップロードするか、\n「サンプルデータをロード」ボタンをクリックして、すぐにコンジョイント分析をお試しください。"),
          br(),
          p(style = "color: #718096; max-width: 500px; margin: 0 auto; font-size: 0.9rem;",
            "※ CSVファイルは、被説明変数（例：評価値 1-10、または選択フラグ 0/1）と、各製品プロファイルの属性を表す列（ブランド、価格、カメラ画素数など）が含まれる形式を用意してください。")
      )
    } else {
      # Show main analysis tabs
      tabsetPanel(
        id = "main_tabs",
        tabPanel("データプレビュー", 
                 # Row 1: KPI overview
                 div(class = "card",
                     div(class = "card-title", icon("eye"), "データ概要"),
                     div(class = "kpi-container",
                         div(class = "kpi-card",
                             div(class = "kpi-value", textOutput("kpi_rows", inline=TRUE)),
                             div(class = "kpi-label", "総サンプル行数")
                         ),
                         div(class = "kpi-card",
                             div(class = "kpi-value", textOutput("kpi_cols", inline=TRUE)),
                             div(class = "kpi-label", "列数（変数）")
                         ),
                         div(class = "kpi-card",
                             div(class = "kpi-value", textOutput("kpi_nas", inline=TRUE)),
                             div(class = "kpi-label", "欠損レコード数")
                         )
                     )
                 ),
                 # Row 2: Preview Data Frame
                 div(class = "card",
                     div(class = "card-title", icon("table"), "データプレビュー（先頭10レコード）"),
                     div(class = "table-responsive",
                         tableOutput("table_preview")
                     )
                 )
        ),
        tabPanel("データ編集",
                 # Row 0: Cleansing and Wide-to-Long Reshaping
                 fluidRow(
                   column(5,
                          div(class = "card",
                              div(class = "card-title", icon("filter"), "1. データクレンジング（Qualtrics対応）"),
                              p(style = "color: #718096; font-size: 0.85rem; margin-bottom: 10px;",
                                "Qualtricsのメタデータ行除去や特定の回答ステータス絞り込みを行います。"),
                              checkboxInput("clean_skip_headers", "Qualtricsのメタデータ行（先頭2行）を除去する", FALSE),
                              checkboxInput("clean_filter_status", "特定の列値でレコードを絞り込む", FALSE),
                              conditionalPanel(
                                condition = "input.clean_filter_status == true",
                                selectizeInput("clean_filter_col", "フィルター対象の列", choices = NULL),
                                textInput("clean_filter_val", "一致する値", value = "IP Address")
                              ),
                              actionButton("btn_apply_clean", "データクレンジングを適用する", 
                                           class = "btn btn-primary", 
                                           style = "margin-top: 15px; width: 100%; font-weight: 600; background: linear-gradient(135deg, #319795, #2c7a7b); border: none;")
                          )
                   ),
                   column(7,
                          div(class = "card",
                              div(class = "card-title", icon("arrows-alt-v"), "2. 選択型コンジョイント(横持ち)の縦持ち変換"),
                              p(style = "color: #718096; font-size: 0.85rem; margin-bottom: 10px;",
                                "アンケート時の横並びデータを、分析可能な縦持ち（long）形式へ一括変換します。"),
                              
                              tags$details(
                                tags$summary("詳細パラメータ設定を展開する", style = "cursor: pointer; font-weight: 600; color: #2b6cb0; margin-bottom: 10px;"),
                                fluidRow(
                                  column(6,
                                         selectizeInput("cbc_ans_cols", "回答列の選択 (タスク質問の列名)", choices = NULL, multiple = TRUE, options = list(placeholder = '例: Q4, Q5, Q12')),
                                         selectizeInput("cbc_covariates", "残す回答者属性（性別や年代など）", choices = NULL, multiple = TRUE, options = list(placeholder = '例: Q18, Q19.1')),
                                         textInput("cbc_attributes", "製品属性名 (カンマ区切り)", placeholder = "例: flavor, sweet, intensity")
                                  ),
                                  column(6,
                                         fluidRow(
                                           column(6, textInput("cbc_alt1_format", "選択肢1(左)列形式", value = "{attribute}A{task}")),
                                           column(6, textInput("cbc_alt2_format", "選択肢2(右)列形式", value = "{attribute}B{task}"))
                                         ),
                                         selectInput("cbc_match_type", "回答判定ルール", choices = c("完全一致" = "exact", "部分一致" = "contains", "正規表現" = "regex"), selected = "exact"),
                                         fluidRow(
                                           column(6, textInput("cbc_alt1_match_val", "選択肢1判定値", value = "炭酸飲料1")),
                                           column(6, textInput("cbc_alt2_match_val", "選択肢2判定値", value = "炭酸飲料2"))
                                         )
                                  )
                                ),
                                actionButton("btn_run_reshape", "縦持ちデータへの変換を実行する", 
                                             class = "btn btn-primary", 
                                             style = "margin-top: 15px; width: 100%; font-weight: 600; background: linear-gradient(135deg, #38a169, #2f855a); border: none;")
                              )
                          )
                   )
                 ),
                 # Row 1: Rename Panel & Variable type summary side by side
                 fluidRow(
                   column(6,
                          div(class = "card",
                              div(class = "card-title", icon("edit"), "変数名（カラム名）のリネーム・編集"),
                              p(style = "color: #718096; font-size: 0.85rem; margin-bottom: 15px;", 
                                "データフレーム内の各列名をわかりやすい名前に変更できます。変更適用後に自動で更新されます。"),
                              uiOutput("rename_panel"),
                              actionButton("btn_apply_rename", "変数名変更を適用する", 
                                           class = "btn btn-primary", 
                                           style = "margin-top: 15px; width: 100%; font-weight: 600; background: linear-gradient(135deg, #2b6cb0, #2c5282); border: none;")
                          )
                   ),
                   column(6,
                          div(class = "card",
                              div(class = "card-title", icon("info-circle"), "変数の型とサマリー一覧"),
                              p(style = "color: #718096; font-size: 0.85rem; margin-bottom: 15px;", 
                                "現在のデータフレーム内の各変数の型および概要です。"),
                              div(style = "max-height: 310px; overflow-y: auto;",
                                  tableOutput("vars_summary_table")
                              )
                          )
                   )
                 )
        ),
        tabPanel("コンジョイント分析結果",
                 uiOutput("tab_results_ui")
        ),
        tabPanel("視覚化グラフ",
                 uiOutput("tab_plots_ui")
        )
      )
    }
  })
  
  # KPI Calculations
  output$kpi_rows <- renderText({
    req(data_holder())
    nrow(data_holder())
  })
  output$kpi_cols <- renderText({
    req(data_holder())
    ncol(data_holder())
  })
  output$kpi_nas <- renderText({
    req(data_holder())
    sum(!complete.cases(data_holder()))
  })
  
  output$table_preview <- renderTable({
    req(data_holder())
    head(data_holder(), 10)
  }, class = "shiny-table table")
  
  # Server-side Rename UI Renderer
  output$rename_panel <- renderUI({
    req(data_holder())
    df <- data_holder()
    cols <- names(df)
    
    tags$div(
      style = "max-height: 250px; overflow-y: auto; padding-right: 10px;",
      lapply(cols, function(col) {
        fluidRow(
          style = "margin-bottom: 10px; display: flex; align-items: center;",
          column(5, tags$span(style = "font-weight: 600; font-size: 0.85rem; word-break: break-all; color: #4a5568;", col)),
          column(2, tags$div(style = "text-align: center; color: #cbd5e0;", icon("arrow-right"))),
          column(5, textInput(paste0("rename_", col), label = NULL, value = col, width = "100%"))
        )
      })
    )
  })
  
  # Server-side Variables Type Summary Table
  output$vars_summary_table <- renderTable({
    req(data_holder())
    df <- data_holder()
    
    data.frame(
      `変数名 (Variable)` = names(df),
      `データ型 (Type)` = sapply(df, function(x) class(x)[1]),
      `ユニーク値数 (Unique)` = sapply(df, function(x) length(unique(x))),
      `欠損値数 (NAs)` = sapply(df, function(x) sum(is.na(x))),
      check.names = FALSE
    )
  }, class = "shiny-table table")
  
  # Apply Column Renaming Observer
  observeEvent(input$btn_apply_rename, {
    req(data_holder())
    df <- data_holder()
    cols <- names(df)
    
    # Retrieve new names from text inputs
    new_names <- sapply(cols, function(col) {
      val <- input[[paste0("rename_", col)]]
      if(is.null(val) || trimws(val) == "") {
        col
      } else {
        trimws(val)
      }
    })
    
    # Validation checks
    if(any(duplicated(new_names))) {
      showNotification("エラー：重複する変数名があります。それぞれ異なる名前を指定してください。", type = "error")
      return()
    }
    
    if(any(new_names == "")) {
      showNotification("エラー：変数名を空欄にすることはできません。", type = "error")
      return()
    }
    
    # Apply and trigger reactivity
    names(df) <- new_names
    data_holder(df)
    showNotification("変数名を正常に変更しました。", type = "message")
  })
  
  # Select all X variables observer
  observeEvent(input$btn_select_all_x, {
    req(data_holder())
    cols <- names(data_holder())
    y_var <- input$y_var
    x_choices <- cols[!cols %in% c("Respondent_ID", "Profile_ID", "Respondent", "Profile", "res_id", "task_id", "ans", "alt", y_var)]
    updateSelectizeInput(session, "x_vars", selected = x_choices)
  })
  
  # Clear X variables observer
  observeEvent(input$btn_clear_x, {
    updateSelectizeInput(session, "x_vars", selected = character(0))
  })
  
  # Update preprocessing dropdowns when raw data updates
  observe({
    req(raw_data_holder())
    df <- raw_data_holder()
    cols <- names(df)
    
    # Update cleansing filter column selection
    updateSelectizeInput(session, "clean_filter_col", choices = cols, selected = if("Status" %in% cols) "Status" else cols[1])
    
    # Update wide-to-long answer columns & covariates selection choices
    updateSelectizeInput(session, "cbc_ans_cols", choices = cols, selected = character(0))
    updateSelectizeInput(session, "cbc_covariates", choices = cols, selected = character(0))
  })
  
  # Data Cleansing Observer
  observeEvent(input$btn_apply_clean, {
    req(raw_data_holder())
    tryCatch({
      df <- raw_data_holder()
      
      # 1. Skip top 2 metadata rows if checked
      if (input$clean_skip_headers) {
        if (nrow(df) <= 2) {
          stop("データ行数が少ないため、ヘッダー行を除外できません。")
        }
        df <- df[-c(1, 2), , drop = FALSE]
      }
      
      # 2. Filter status
      if (input$clean_filter_status && nzchar(input$clean_filter_col)) {
        col <- input$clean_filter_col
        val <- input$clean_filter_val
        df <- df[df[[col]] == val, , drop = FALSE]
      }
      
      cleaned_data_holder(df)
      data_holder(df)
      showNotification("データクレンジングを適用しました。", type = "message")
    }, error = function(e) {
      showNotification(paste("クレンジング適用エラー:", e$message), type = "error")
    })
  })
  
  # Wide-to-Long Reshaping Observer
  observeEvent(input$btn_run_reshape, {
    req(cleaned_data_holder())
    tryCatch({
      df <- cleaned_data_holder()
      ans_cols <- input$cbc_ans_cols
      covariates <- input$cbc_covariates
      
      # Parse attributes
      attr_str <- input$cbc_attributes
      if (!nzchar(trimws(attr_str))) {
        stop("製品属性名をカンマ区切りで入力してください。")
      }
      attributes <- trimws(unlist(strsplit(attr_str, ",")))
      attributes <- attributes[attributes != ""]
      
      if (length(attributes) == 0) {
        stop("製品属性名が入力されていません。")
      }
      if (length(ans_cols) == 0) {
        stop("回答列を少なくとも1つ選択してください。")
      }
      
      # Run reshaping logic
      long_df <- reshape_wide_to_long(
        df = df,
        ans_cols = ans_cols,
        attributes = attributes,
        alt1_format = input$cbc_alt1_format,
        alt2_format = input$cbc_alt2_format,
        alt1_match_val = input$cbc_alt1_match_val,
        alt2_match_val = input$cbc_alt2_match_val,
        match_type = input$cbc_match_type,
        covariates = covariates
      )
      
      # Update active data holder
      data_holder(long_df)
      showNotification(paste("縦持ち変換に成功しました！ 総行数:", nrow(long_df)), type = "message")
    }, error = function(e) {
      showNotification(paste("縦持ち変換エラー:", e$message), type = "error", duration = 10)
    })
  })
  
  # Dynamic Reference Levels UI Renderer
  output$ref_levels_ui <- renderUI({
    req(data_holder(), input$x_vars)
    df <- data_holder()
    x_vars <- input$x_vars
    
    if (length(x_vars) == 0) return(NULL)
    
    tags$div(
      style = "background: #f7fafc; padding: 12px; border-radius: 8px; margin-bottom: 15px; border: 1px solid #edf2f7; max-height: 200px; overflow-y: auto;",
      tags$strong("属性ごとの基準水準（Reference）設定", style = "font-size: 0.85rem; color: #4a5568; display: block; margin-bottom: 8px;"),
      lapply(x_vars, function(x) {
        lvls <- unique(as.character(df[[x]]))
        lvls <- lvls[!is.na(lvls) & lvls != ""]
        selectInput(paste0("ref_", x), paste0(x, " の基準水準"), choices = lvls, selected = lvls[1], width = "100%")
      })
    )
  })
  
  # Dynamic tab ui for results
  output$tab_results_ui <- renderUI({
    if(!is.null(conjoint_data$error_msg)) {
      div(class = "alert alert-danger", style = "background-color: #fff5f5; border-color: #fed7d7; color: #9b2c2c; padding: 25px; border-radius: 12px; border: 1px solid #fed7d7; box-shadow: 0 4px 6px rgba(0,0,0,0.02);",
          div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 12px;",
              icon("exclamation-triangle", style = "font-size: 1.8rem; color: #e53e3e;"),
              h3(style = "margin: 0; font-weight: 700; color: #9b2c2c;", "分析の実行に失敗しました")
          ),
          p(style = "font-weight: 600; font-size: 1rem; margin-bottom: 15px; color: #e53e3e;", paste("エラーメッセージ:", conjoint_data$error_msg)),
          tags$hr(style = "border-color: #fed7d7; margin: 15px 0;"),
          tags$h5(style = "font-weight: 700; color: #742a2a; margin-top: 0;", "一般的な解決のためのヒントとチェック項目:"),
          tags$ul(style = "padding-left: 20px; color: #742a2a; line-height: 1.6;",
                  tags$li(tags$strong("被説明変数(Y)の要件 (ロジスティック回帰 / clogit / glmerの場合) :"), " Y列（例: choice）の値がすべて数値の 0 または 1 になっているか確認してください。文字データや空欄が含まれているとエラーになります。データ編集タブの『変数の型とサマリー一覧』でユニーク値が 0 と 1 のみであることを確認してください。"),
                  tags$li(tags$strong("多重共線性 (Perfect Multicollinearity) :"), " 完全に同じタイミングで出現する属性（例: 2つの列が完全に同一）があるか、またはデータサンプル数が少なすぎます。一部の説明変数（属性）を外して再度実行してください。"),
                  tags$li(tags$strong("被験者ID / タスクID列 :"), " glmer や clogit の場合、指定した回答者ID列（res_id）やタスクID列（task_id）が正しく選択されているか確認してください。縦持ち変換を実行するとこれらの列は自動で生成されます。"),
                  tags$li(tags$strong("カテゴリ水準の確認 :"), " 選択した属性（説明変数）のレベルの中に、欠損値(NA)や空文字、または1つの水準（データが1種類だけ）しかない列がないかご確認ください。")
          )
      )
    } else if(!analysis_run()) {
      div(style = "text-align: center; padding: 40px;",
          icon("calculator", style = "font-size: 3rem; color: #cbd5e0; margin-bottom: 15px;"),
          h4("分析が実行されていません"),
          p("左側パネルの「分析を実行」ボタンを押すと、こちらに計算結果テーブルが出力されます。")
      )
    } else {
      # We have results!
      tryCatch({
        res <- conjoint_results()
        
        tagList(
          # Convergence warnings alert card if warnings exist
          if(!is.null(conjoint_data$warnings)) {
            div(class = "alert alert-warning", style = "background-color: #fffaf0; border-color: #feebc8; color: #c05621; padding: 15px; border-radius: 8px; border: 1px solid #feebc8; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.01);",
                div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 5px;",
                    icon("exclamation-triangle", style = "font-size: 1.3rem; color: #dd6b20;"),
                    h4(style = "margin: 0; font-weight: 700; color: #c05621;", "分析モデルの警告（収束・適合に関する注意）")
                ),
                tags$ul(style = "margin: 0; padding-left: 20px; font-size: 0.9rem;",
                        lapply(conjoint_data$warnings, function(w) tags$li(w))
                ),
                p(style = "margin-top: 10px; font-size: 0.85rem; color: #7b341e; margin-bottom: 0;",
                  "※モデルの推定自体は完了しましたが、特定のグループやカテゴリのデータ数が極端に少ない、または多重共線性の影響がある可能性があります。推定値の有意性や標準誤差の大きさに注意してください。")
            )
          } else {
            NULL
          },
          
          # Model fit summary card
          div(class = "card",
              div(class = "card-title", icon("info-circle"), "モデル適合度"),
              uiOutput("model_fit_stats")
          ),
          
          # Utilities table card
          div(class = "card",
              div(class = "card-title", icon("list-ol"), "部分効用値テーブル"),
              p(style = "color: #718096; font-size: 0.9rem; margin-bottom: 15px;",
                "モデルから推定された各属性・水準の部分効用値（Part-worth Utility）です。係数の正負および絶対値は、その水準に対する選好の強さを表します。"),
              div(class = "table-responsive",
                  tableOutput("table_utilities")
              )
          ),
          
          # Raw outputs
          tags$details(
            tags$summary("回帰分析モデルの生出力 (R Summary)", style = "cursor: pointer; color: #4a5568; font-weight: 600; padding: 10px 0; font-size: 0.95rem;"),
            div(class = "card", style = "background-color: #f7fafc;",
                verbatimTextOutput("raw_summary")
            )
          )
        )
      }, error = function(e) {
        div(class = "alert alert-danger",
            h4("結果の処理中に予期せぬエラーが発生しました"),
            p(e$message)
        )
      })
    }
  })
  
  # Model fit stats formatter
  output$model_fit_stats <- renderUI({
    res <- conjoint_results()
    model_type <- input$model_type
    
    if(model_type == "lm") {
      # Linear model metrics
      r_sq <- res$summary$r.squared
      adj_r_sq <- res$summary$adj.r.squared
      f_val <- res$summary$fstatistic
      
      div(class = "kpi-container",
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.4f", r_sq)),
              div(class = "kpi-label", "決定係数 (R-squared)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.4f", adj_r_sq)),
              div(class = "kpi-label", "自由度調整済決定係数")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", if(!is.null(f_val)) sprintf("%.2f", f_val[1]) else "N/A"),
              div(class = "kpi-label", "F値 (F-statistic)")
          )
      )
    } else if (model_type == "glm") {
      # Logistic regression metrics (standard GLM)
      null_dev <- res$model$null.deviance
      resid_dev <- res$model$deviance
      mcfadden_r2 <- 1 - (resid_dev / null_dev)
      aic <- res$summary$aic
      
      div(class = "kpi-container",
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.4f", mcfadden_r2)),
              div(class = "kpi-label", "疑似決定係数 (McFadden R2)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.1f", aic)),
              div(class = "kpi-label", "赤池情報量基準 (AIC)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.1f", resid_dev)),
              div(class = "kpi-label", "残差逸脱度 (Residual Deviance)")
          )
      )
    } else if (model_type == "glmer") {
      # Mixed-effects model metrics
      aic <- AIC(res$model)
      bic <- BIC(res$model)
      loglik <- as.numeric(logLik(res$model))
      ngrps <- res$summary$ngrps
      
      div(class = "kpi-container",
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.1f", aic)),
              div(class = "kpi-label", "赤池情報量基準 (AIC)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.1f", bic)),
              div(class = "kpi-label", "ベイズ情報量基準 (BIC)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.2f", loglik)),
              div(class = "kpi-label", "対数尤度 (Log-Likelihood)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", if(!is.null(ngrps)) paste(ngrps, collapse = ", ") else "N/A"),
              div(class = "kpi-label", "被験者数 (Groups)")
          )
      )
    } else if (model_type == "clogit") {
      # Conditional logit model metrics
      c_index <- res$summary$concordance["C"]
      aic <- AIC(res$model)
      lr_stat <- res$summary$logtest["test"]
      
      div(class = "kpi-container",
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.4f", c_index)),
              div(class = "kpi-label", "一致度指数 (Concordance / C-Index)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.1f", aic)),
              div(class = "kpi-label", "赤池情報量基準 (AIC)")
          ),
          div(class = "kpi-card",
              div(class = "kpi-value", sprintf("%.2f", lr_stat)),
              div(class = "kpi-label", "尤度比検定統計量 (LR Stat)")
          )
      )
    }
  })
  
  # Format Utilities Table
  output$table_utilities <- renderTable({
    res <- conjoint_results()
    df <- res$utilities
    
    # Format columns for printing
    df_print <- df %>%
      mutate(
        Utility_Display = ifelse(is.na(Utility), "推定不可 (多重共線性)", sprintf("%.4f", Utility)),
        StdError_Display = ifelse(IsReference & input$contrast_type == "treatment", "-", 
                                  ifelse(is.na(StdError), "推定不可", sprintf("%.4f", StdError))),
        CI_Display = ifelse(IsReference & input$contrast_type == "treatment", "-", 
                            ifelse(is.na(CI_Lower) | is.na(CI_Upper), "推定不可", sprintf("[ %.4f,  %.4f ]", CI_Lower, CI_Upper))),
        pValue_Display = ifelse(IsReference & input$contrast_type == "treatment", "-", 
                                ifelse(is.na(pValue), "推定不可",
                                       ifelse(pValue < 0.0001, "< 0.0001", sprintf("%.4f", pValue)))),
        Significance = ifelse(IsReference & input$contrast_type == "treatment", "基準水準",
                              ifelse(is.na(pValue), "非定常 / 多重共線性",
                                     ifelse(pValue < 0.001, "*** (p<0.001)",
                                            ifelse(pValue < 0.01, "** (p<0.01)",
                                                   ifelse(pValue < 0.05, "* (p<0.05)",
                                                          ifelse(pValue < 0.1, ". (p<0.1)", "有意差なし"))))))
      ) %>%
      select(
        `属性 (Attribute)` = Attribute,
        `水準 (Level)` = Level,
        `部分効用値 (Utility)` = Utility_Display,
        `標準誤差 (Std.Error)` = StdError_Display,
        `信頼区間 (CI)` = CI_Display,
        `p値 (p-value)` = pValue_Display,
        `有意性 (Sig.)` = Significance
      )
    df_print
  }, class = "shiny-table table")
  
  # Raw regression summary
  output$raw_summary <- renderPrint({
    res <- conjoint_results()
    res$summary
  })
  
  # Reactive function to generate the Coefplot (exactly as requested in the image)
  make_coef_plot <- reactive({
    req(conjoint_data$results)
    res <- conjoint_results()
    df <- res$utilities
    
    # Filter out reference levels for dummy coding (treatment contrast)
    plot_df <- if(input$contrast_type == "treatment") {
      df %>% filter(!IsReference)
    } else {
      df
    }
    
    # Construct comparison label
    if(input$contrast_type == "treatment") {
      ref_levels <- df %>% 
        filter(IsReference) %>% 
        select(Attribute, RefLevel = Level)
      
      plot_df <- plot_df %>% 
        left_join(ref_levels, by = "Attribute") %>% 
        mutate(Label = paste0("(", RefLevel, ") → ", Level))
    } else {
      plot_df <- plot_df %>% 
        mutate(Label = paste0(Level, " (vs 平均)"))
    }
    
    # Order attributes as they appear in selected explanatory variables
    attr_order <- data.frame(
      Attribute = input$x_vars,
      Attr_Idx = 1:length(input$x_vars),
      stringsAsFactors = FALSE
    )
    
    plot_df <- plot_df %>% 
      left_join(attr_order, by = "Attribute") %>% 
      arrange(Attr_Idx, Level)
    
    # Set factor levels for Label in reverse order so the first attribute appears at the top
    plot_df$Label <- factor(plot_df$Label, levels = rev(unique(plot_df$Label)))
    
    # Titles and labels
    title_text <- paste0("望ましい", input$y_var, "の属性別インパクト")
    subtitle_text <- if(input$contrast_type == "treatment") "表記例 : (基準レベル) → 比較対象レベル" else "表記例 : 各水準の平均値からの乖離"
    x_label <- if (input$model_type %in% c("glm", "glmer", "clogit")) "推定値 (Log-odds / 効用値)" else "推定値 (部分効用値)"
    
    # Categorize statistical significance
    plot_df$Sig_Cat <- ifelse(
      !is.na(plot_df$pValue) & plot_df$pValue < 0.05,
      "統計的有意 (p < 0.05)",
      "有意差なし (p >= 0.05)"
    )
    plot_df$Sig_Cat <- factor(plot_df$Sig_Cat, levels = c("統計的有意 (p < 0.05)", "有意差なし (p >= 0.05)"))
    
    # ggplot
    ggplot(plot_df, aes(x = Utility, y = Label, color = Attribute)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
      geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.2, size = 0.8) +
      geom_point(aes(shape = Sig_Cat, alpha = Sig_Cat), size = 4) +
      scale_shape_manual(values = c("統計的有意 (p < 0.05)" = 16, "有意差なし (p >= 0.05)" = 1)) +
      scale_alpha_manual(values = c("統計的有意 (p < 0.05)" = 1.0, "有意差なし (p >= 0.05)" = 0.5)) +
      labs(
        title = title_text,
        subtitle = subtitle_text,
        x = x_label,
        y = "属性 (変化の方向)",
        color = "カテゴリ",
        shape = "統計的有意性",
        alpha = "統計的有意性"
      ) +
      theme_minimal(base_family = "Hiragino Sans") +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#1a365d", hjust = 0),
        plot.subtitle = element_text(size = 10, color = "#4a5568", margin = margin(b = 15), hjust = 0),
        axis.title.x = element_text(face = "bold", size = 11, color = "#2d3748"),
        axis.title.y = element_text(face = "bold", size = 11, color = "#2d3748"),
        axis.text.y = element_text(size = 10, color = "#2d3748"),
        axis.text.x = element_text(size = 10, color = "#2d3748"),
        panel.grid.major.y = element_line(color = "#edf2f7"),
        panel.grid.major.x = element_line(color = "#edf2f7"),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 10, color = "#2d3748"),
        legend.text = element_text(size = 9, color = "#2d3748")
      )
  })

  # Reactive function to generate the Importance plot
  make_importance_plot <- reactive({
    req(conjoint_data$results)
    res <- conjoint_results()
    df <- res$importance
    
    ggplot(df, aes(x = reorder(Attribute, Importance), y = Importance, fill = Importance)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = sprintf("%.1f%%", Importance)), 
                hjust = -0.15, size = 3.5, fontface = "bold", color = "#2d3748") +
      coord_flip() +
      scale_fill_gradient(low = "#90cdf4", high = "#2b6cb0") +
      labs(
        title = "属性の重要度比率",
        subtitle = "選好に与える影響力のウェイト (%)",
        x = "",
        y = "重要度 (%)"
      ) +
      scale_y_continuous(limits = c(0, max(df$Importance) * 1.15)) +
      theme_minimal(base_family = "Hiragino Sans") +
      theme(
        plot.title = element_text(face = "bold", size = 13, color = "#1a365d"),
        plot.subtitle = element_text(size = 9, color = "#4a5568", margin = margin(b = 10)),
        axis.text.y = element_text(face = "bold", size = 10, color = "#2d3748"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )
  })

  # Reactive function to generate the Utilities Horizontal Plot (exactly like the first plot, but for all levels)
  make_utilities_bar_plot <- reactive({
    req(conjoint_data$results)
    res <- conjoint_results()
    df <- res$utilities
    
    # We keep all levels (including reference levels)
    plot_df <- df
    
    # Construct label: "Attribute: Level" to avoid duplicate names and clearly group them
    plot_df <- plot_df %>% 
      mutate(Label = paste0(Attribute, ": ", Level))
    
    # Order attributes as they appear in selected explanatory variables
    attr_order <- data.frame(
      Attribute = input$x_vars,
      Attr_Idx = 1:length(input$x_vars),
      stringsAsFactors = FALSE
    )
    
    plot_df <- plot_df %>% 
      left_join(attr_order, by = "Attribute") %>% 
      arrange(Attr_Idx, Level)
    
    # Set factor levels for Label in reverse order so the first attribute appears at the top
    plot_df$Label <- factor(plot_df$Label, levels = rev(unique(plot_df$Label)))
    
    # Titles and labels
    title_text <- paste0(input$y_var, "の部分効用値（全水準一覧）")
    subtitle_text <- if(input$contrast_type == "treatment") "※基準水準（第一水準）の効用値は 0 となります" else "※各水準の平均値からの乖離"
    x_label <- if (input$model_type %in% c("glm", "glmer", "clogit")) "部分効用値 (Log-odds)" else "部分効用値 (Utility)"
    
    # Categorize statistical significance
    # For reference levels in dummy coding, pValue is NA, let's categorize them as "基準水準"
    plot_df$Sig_Cat <- ifelse(
      plot_df$IsReference & input$contrast_type == "treatment",
      "基準水準 (Reference)",
      ifelse(
        !is.na(plot_df$pValue) & plot_df$pValue < 0.05,
        "統計的有意 (p < 0.05)",
        "有意差なし (p >= 0.05)"
      )
    )
    plot_df$Sig_Cat <- factor(plot_df$Sig_Cat, levels = c("統計的有意 (p < 0.05)", "有意差なし (p >= 0.05)", "基準水準 (Reference)"))
    
    # ggplot
    ggplot(plot_df, aes(x = Utility, y = Label, color = Attribute)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
      geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), width = 0.2, size = 0.8) +
      geom_point(aes(shape = Sig_Cat, alpha = Sig_Cat), size = 4) +
      scale_shape_manual(values = c("統計的有意 (p < 0.05)" = 16, "有意差なし (p >= 0.05)" = 1, "基準水準 (Reference)" = 15)) +
      scale_alpha_manual(values = c("統計的有意 (p < 0.05)" = 1.0, "有意差なし (p >= 0.05)" = 0.5, "基準水準 (Reference)" = 0.8)) +
      labs(
        title = title_text,
        subtitle = subtitle_text,
        x = x_label,
        y = "属性と水準 (Attribute & Level)",
        color = "カテゴリ",
        shape = "統計的有意性",
        alpha = "統計的有意性"
      ) +
      theme_minimal(base_family = "Hiragino Sans") +
      theme(
        plot.title = element_text(face = "bold", size = 14, color = "#1a365d", hjust = 0),
        plot.subtitle = element_text(size = 10, color = "#4a5568", margin = margin(b = 15), hjust = 0),
        axis.title.x = element_text(face = "bold", size = 11, color = "#2d3748"),
        axis.title.y = element_text(face = "bold", size = 11, color = "#2d3748"),
        axis.text.y = element_text(size = 10, color = "#2d3748"),
        axis.text.x = element_text(size = 10, color = "#2d3748"),
        panel.grid.major.y = element_line(color = "#edf2f7"),
        panel.grid.major.x = element_line(color = "#edf2f7"),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 10, color = "#2d3748"),
        legend.text = element_text(size = 9, color = "#2d3748")
      )
  })

  
  # Dynamic tab ui for plots
  output$tab_plots_ui <- renderUI({
    if(!is.null(conjoint_data$error_msg)) {
      div(class = "alert alert-danger", style = "background-color: #fff5f5; border-color: #fed7d7; color: #9b2c2c; padding: 25px; border-radius: 12px; border: 1px solid #fed7d7; box-shadow: 0 4px 6px rgba(0,0,0,0.02);",
          div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 12px;",
              icon("exclamation-triangle", style = "font-size: 1.8rem; color: #e53e3e;"),
              h3(style = "margin: 0; font-weight: 700; color: #9b2c2c;", "分析エラーのためグラフを表示できません")
          ),
          p(style = "font-weight: 600; font-size: 1rem; margin-bottom: 15px; color: #e53e3e;", paste("エラーメッセージ:", conjoint_data$error_msg)),
          p("「コンジョイント分析結果」タブに詳細なチェック項目とエラー対処方法が記載されています。データ編集タブやサイドバーの設定を見直してください。")
      )
    } else if(!analysis_run()) {
      div(style = "text-align: center; padding: 40px;",
          icon("chart-bar", style = "font-size: 3rem; color: #cbd5e0; margin-bottom: 15px;"),
          h4("分析が実行されていません"),
          p("左側パネルの「分析を実行」ボタンを押すと、こちらにプロットが描画されます。")
      )
    } else {
      tryCatch({
        tagList(
          # Row with Coefplot and Importance
          fluidRow(
            column(7,
                   div(class = "card",
                       div(class = "card-title", icon("chart-line"), "有意水準と信頼区間のグラフ (Coefplot)"),
                       plotOutput("plot_coef", height = "480px"),
                       div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 15px;",
                           p(class = "help-block-custom", style = "margin: 0; max-width: 65%;", "丸点は推定値、横線は信頼区間（CI）。白抜きは有意差なし(p>=0.05)を表します。"),
                           downloadButton("download_coef", "PDFをダウンロード", class = "btn btn-default btn-secondary-custom", icon = icon("download"))
                       )
                   )
            ),
            column(5,
                   div(class = "card",
                       div(class = "card-title", icon("percent"), "属性の相対重要度"),
                       plotOutput("plot_importance", height = "480px"),
                       div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 15px;",
                           p(class = "help-block-custom", style = "margin: 0; max-width: 55%;", "全体の重要度合計が100%になる割合。"),
                           downloadButton("download_importance", "PDFをダウンロード", class = "btn btn-default btn-secondary-custom", icon = icon("download"))
                       )
                   )
            )
          ),
          # Full Width Part-Worth bar chart
          div(class = "card",
              div(class = "card-title", icon("chart-bar"), "属性別の部分効用値（全水準一覧）"),
              plotOutput("plot_utilities_bar", height = "520px"),
              div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 15px;",
                  p(class = "help-block-custom", style = "margin: 0; max-width: 70%;", "全属性の各水準における部分効用値（Part-worth Utility）を同一グラフ上で一覧比較したものです。"),
                  downloadButton("download_utilities", "PDFをダウンロード", class = "btn btn-default btn-secondary-custom", icon = icon("download"))
              )
          )
        )
      }, error = function(e) {
        div(class = "alert alert-danger",
            h4("プロット描画中にエラーが発生しました"),
            p(e$message)
        )
      })
    }
  })
  
  # Render the plots using reactive functions
  output$plot_coef <- renderPlot({
    make_coef_plot()
  })
  
  output$plot_importance <- renderPlot({
    make_importance_plot()
  })
  
  output$plot_utilities_bar <- renderPlot({
    make_utilities_bar_plot()
  })
  
  # Download Handlers for PDFs
  output$download_coef <- downloadHandler(
    filename = function() {
      paste0("coefplot-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 11, height = 8, family = "Japan1GothicBBB")
      print(make_coef_plot())
      dev.off()
    }
  )
  
  output$download_importance <- downloadHandler(
    filename = function() {
      paste0("importance-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 8, height = 5, family = "Japan1GothicBBB")
      print(make_importance_plot())
      dev.off()
    }
  )
  
  output$download_utilities <- downloadHandler(
    filename = function() {
      paste0("utilities-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 11, height = 6, family = "Japan1GothicBBB")
      print(make_utilities_bar_plot())
      dev.off()
    }
  )
  
}

# -------------------------------------------------------------
# RUN APPLICATION
# -------------------------------------------------------------
shinyApp(ui = ui, server = server)
