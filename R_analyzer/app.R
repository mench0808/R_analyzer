# app.R (修正後)

# ライブラリの読み込みは ui.R と server.R にも移動しましたが、
# ここにも残しておくことで、スタンドアロン実行時の安全性が高まります。
library(shiny)
library(tidyverse)
library(bslib)

# ★★★ 修正箇所: ui.R および server.R の内容を読み込むコマンドを削除 ★★★

# Shinyは、引数に ui と server を渡すことで、
# 同じディレクトリにある "ui.R" および "server.R" を自動的に探して実行します。
# ※ ui と server の定義がファイル内にある必要はありません。
shinyApp(ui = ui, server = server)