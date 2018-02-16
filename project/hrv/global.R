
# Debug mode switch
debug <- F

user_accounts <- list(
  "markpeng"="47ef8bc6f8eda0a983ebe7f9fe02e59b38be6ff4ee212a34451e96c505a77c28",
  "rudy"="b8c8147fb8064fc222363492e7f362fe5c05a024cfc32400e22a06ee73320770"
)

# Utilit Functions #

validate_password = function(username, password) {
  if(username %in% names(user_accounts)) {
    check = digest(password, algo="sha256", serialize=F)
    if(check == user_accounts[username]) {
      return(TRUE)
    }
  }
  return(FALSE)
}


# UI Pages #

login_page <- fluidPage(
  useShinyjs(),  # Include shinyjs
  column(
    width = 4,
    offset = 5,
    box(title = enc2utf8("請輸入帳號密碼:"),
        status = "primary",
        textInput("username", enc2utf8("帳號")),
        passwordInput("password", enc2utf8("密碼")),
        shinyjs::hidden(
          p(id = "login_failed", enc2utf8("帳號密碼錯誤!"),
            style = "color: red; font-size: 18px;")
        ),
        actionButton("login_button", enc2utf8("登入"),
                     icon = icon("sign-in-alt"),
                     class = "btn-primary"))
  )
)

main_page <- fluidPage(sidebarLayout(
  sidebarPanel(
    width = 3,
    textInput(inputId = "input_heart",
              label = enc2utf8("心跳速率(HEART):"),
              value = "",
              width = "95%"),
    textInput(inputId = "input_sex",
              label = enc2utf8("副交感神經(SEX):"),
              value = "",
              width = "95%"),
    textInput(inputId = "input_health",
              label = enc2utf8("自律神經整體活性(HEALTH):"),
              value = "",
              width = "95%"),
    textInput(inputId = "input_fight",
              label = enc2utf8("交感神經(FIGHT):"),
              value = "",
              width = "95%"),
    textInput(inputId = "input_vital",
              label = enc2utf8("整體神經功能(VITAL):"),
              value = "",
              width = "95%"),
    div(actionButton("clear_button", enc2utf8("清除"),
                     icon = icon("trash"),
                     class = "btn-primary"),
        actionButton("run_button", enc2utf8("診斷"),
                     icon = icon("refresh"),
                     class = "btn-primary"),
        style = "text-align: right")
  ),
  mainPanel(
    width = 9,
    column(
      width = 12,
      align="center",
      wellPanel(
        h3(enc2utf8("五力圖數據判讀結果"),
           style = "text-align: center;"),
        chartJSRadarOutput("diagnoseRadar"),
        # chartJSRadarOutput("diagnoseRadar", width = "300", height = "200"),
        style = "background-color: #fcfcfc;"
      ),
      wellPanel(
        dataTableOutput("diagnoseTable"),
        style = "height: 100%;"
      )
    )
  )
), title = enc2utf8("五力圖數值診斷"))

diagnosis_rules <- function(type, value) {
  
  result = switch(type,
                  "heart"={
                    # 心跳速率 (HEART)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("風險Ⅰ型"),
                           paste0(enc2utf8("心跳數值↑↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("平均心跳偏高，心臟負荷大，會快速跳動提供身體氧氣，留意可能偏嚴重傾向，"),
                                  "<font color=\"red\">建議就醫追縱。</font>"))  
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意Ⅰ型"),
                           paste0(enc2utf8("心跳速率↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("平靜時心跳高於正常值，有焦慮傾向，留意心血管健康問題，例如血壓、血糖、血脂指數偏高。"),
                                  "<font color=\"red\">建議健康管理。</font>"))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           paste0("健康良好，數值在安全範圍以內。"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意Ⅱ型"),
                           paste0(enc2utf8("心跳速率↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("平均心跳偏低，速率較慢，有暈眩傾向，留意心臟健康問題，例如心臟無力。"),
                                  "<font color=\"red\">建議健康管理。</font>"))
                    } else {
                      list(enc2utf8("風險Ⅱ型"),
                           paste0(enc2utf8("心跳數值↓↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("平均心跳過低，留意可能有心臟無力問題，"),
                                  "<font color=\"red\">建議儘速就醫治療。</font>"))
                    }
                  },
                  "sex"={
                    # 副交感神經 (SEX)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("風險Ⅰ型"),
                           paste0(enc2utf8("副交感神經數值↓↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值過低，有經常性失眠、腸胃功能衰弱現象，留意可能有精神倦怠、身體機能下降問題。"),
                                  "<font color=\"red\">建議積極健康管理及就醫追蹤。</font>"))  
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意Ⅰ型"),
                           paste0(enc2utf8("副交感神經數值↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值低於安全範圍數值，屬於亞健康，留意有入眠不易，精神不濟、胃腸蠕動緩慢等問題。")))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           paste0("健康良好，數值在安全範圍以內。"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意Ⅱ型"),
                           paste0(enc2utf8("副交感神經數值↑↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值偏低，表示身體可能有長期發炎現象、免疫反應過強傾向，留意心血管、過敏健康問題。")))
                    } else {
                      list(enc2utf8("風險Ⅱ型"),
                           paste0(enc2utf8("副交感神經數值↑↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8( "數值過高，表示身體有長期發炎、免疫失調問題，留意可能有血壓低、抵抗力不足、過敏等現象。"),
                                  "<font color=\"red\">建議就醫治療。</font>"))
                    }
                  },
                  "health"={
                    # 自律神經整體活性 (HEALTH)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("風險Ⅰ型"),
                           paste0(enc2utf8("自律神經整體活性數值↓↓安全數值"),
                                  "<br/>",
                                  enc2utf8("數值嚴重偏低，出現免疫功能低落，可能有演變慢性病高風險傾向，"),
                                  "<font color=\"red\">建議進一步追蹤檢查及治療。</font>")) 
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意Ⅰ型"),
                           paste0(enc2utf8("自律神經整體活性數值↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值偏低，出現身心老化速度下滑，可能有心有餘力不足、慢性病困擾。"),
                                  "<font color=\"red\">建議積極健康管理。</font>"))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           paste0("健康良好，數值在安全範圍以內。"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意Ⅱ型"),
                           paste0(enc2utf8("自律神經整體活性數值↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值偏高，出現機能掏空感，耗盡體能傾向，可能有心律不整或心肌梗塞風險，"),
                                  "<font color=\"red\">建議積極健康管理</font>",
                                  "，並進一步追蹤治療。"))
                    } else {
                      list(enc2utf8("風險Ⅱ型"),
                           paste0(enc2utf8("自律神經整體活性數值↑↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值嚴重偏高，留意整體機能衰退現象，心血管疾病風險高，"),
                                  "<font color=\"red\">建議儘速就醫治療。</font>"))
                    }
                  },
                  "fight"={
                    # 交感神經 (FIGHT)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("風險Ⅰ型"),
                           paste0(enc2utf8("交感神經數值↓↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值過低，可能有身體防禦力低落傾向，例如過敏、精神不濟、暈眩、消化不良等症狀。"),
                                  "<font color=\"red\">建議就醫檢查及追蹤。</font>"))
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意Ⅰ型"),
                           paste0(enc2utf8("交感神經數值↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值偏低，容易有心有餘力不足，未老先衰傾向，留意可能有慢性病困擾、應變力下降傾向。"),
                                  "<font color=\"red\">建議積極健康管理。</font>"))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           paste0("健康良好，數值在安全範圍以內。"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意Ⅱ型"),
                           paste0(enc2utf8("交感神經數值↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值偏高，可能有過度耗損，體力無濟困擾，例如淺眠、心跳過快、血壓高、易出汗、易煩躁等問題。"),
                                  "<font color=\"red\">建議積極健康管理。</font>"))
                    } else {
                      list(enc2utf8("風險Ⅱ型"),
                           paste0(enc2utf8("交感神經數值↑↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值過高，可能有長期焦慮、緊張，免疫功能失衡情況。"),
                                  "<font color=\"red\">建議就醫檢查及追蹤。</font>"))
                    }
                  },
                  "vital"={
                    # 總體神經功能 (VITAL)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("健康高風險"),
                           paste0(enc2utf8("數值↓↓↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("指數過低，身體元氣力嚴重不足，生理年齡老化10歲左右，可能有疾病困擾。"),
                                  "<font color=\"red\">建議就醫治療。</font>"))
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("亞健康"),
                           paste0(enc2utf8("數值↓安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值偏低，表示老化急速衰退，生理年齡比實際年齡老化，"),
                                  "<font color=\"red\">建議積極健康管理</font>",
                                  enc2utf8("，進一步就醫追蹤。")))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           paste0(enc2utf8("數值正常"),
                                  "<br/>",
                                  enc2utf8("自律神經總體功能正常。")))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("健康"),
                           paste0(enc2utf8("數值↑↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值偏高，表示自律神經總體功能高，比同年齡來得好。")))
                    } else {
                      list(enc2utf8("健康"),
                           paste0(enc2utf8("數值↑↑↑安全範圍數值"),
                                  "<br/>",
                                  enc2utf8("數值極高，表示自律神經總體功能強，元氣十足、精力旺盛。")))
                    }
                  })
  return(result)
}
