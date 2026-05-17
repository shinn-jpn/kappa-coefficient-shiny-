library(shiny)

ui <- fluidPage(
  titlePanel("カッパ係数計算アプリ"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("2被験者のデータを縦に並べて貼り付けてください（例：A列が評価者1、B列が評価者2）"),
      
      textAreaInput("data_input", 
                    "データ入力（タブ or カンマ区切り）", 
                    rows = 10,
                    placeholder = "1\t0\n0\t1\n1\t1\n0\t0"),
      
      actionButton("calc", "計算")
    ),
    
    mainPanel(
      h3("結果"),
      verbatimTextOutput("result"),
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$calc, {
    
    # データ読み込み（タブ or カンマ対応）
    df <- tryCatch({
      read.table(text = input$data_input, 
                 sep = "", 
                 header = FALSE)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(df) || ncol(df) < 2) {
      output$result <- renderText("データ形式が不正です（2列必要）")
      return()
    }
    
    # 1列目と2列目を使用
    rater1 <- df[,1]
    rater2 <- df[,2]
    
    # 数値化（1/0に変換）
    rater1 <- as.numeric(rater1)
    rater2 <- as.numeric(rater2)
    
    # NA除去
    valid <- complete.cases(rater1, rater2)
    rater1 <- rater1[valid]
    rater2 <- rater2[valid]
    
    # 分割表
    tab <- table(rater1, rater2)
    
    # 観察一致率
    Po <- sum(diag(tab)) / sum(tab)
    
    # 期待一致率
    row_marg <- rowSums(tab) / sum(tab)
    col_marg <- colSums(tab) / sum(tab)
    Pe <- sum(row_marg * col_marg)
    
    # カッパ係数
    kappa <- (Po - Pe) / (1 - Pe)
    
    # 出力
    output$result <- renderText({
      paste0(
        "一致率 (Po): ", round(Po, 3), "\n",
        "期待一致率 (Pe): ", round(Pe, 3), "\n",
        "カッパ係数: ", round(kappa, 3)
      )
    })
    
    output$table <- renderTable({
      tab
    })
    
  })
}

shinyApp(ui = ui, server = server)