
diagnosis_rules <- function(type, value) {
  
  result = switch(type,
                  "heart"={
                    # 心跳速率 (HEART)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("嚴重"),
                           enc2utf8("表示平均心跳嚴重偏高，導致心臟負荷極大，需要快速跳動來提供身體氧氣，可能有嚴重的心臟病問題，有猝死風險。"))  
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意"),
                           enc2utf8("平靜時心跳高於正常值，身體恐有心血管方面問題，甚至隱性心臟病。"))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           enc2utf8("正常數值"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意"),
                           enc2utf8("平均心跳偏低，速率較慢，除少數運動員外，恐有心臟無力問題，若有暈眩現象，留意隱性心臟衰竭傾向。"))
                    } else {
                      list(enc2utf8("嚴重"),
                           enc2utf8("表示平均心跳過低，心臟嚴重無力、供血不足、腦部缺氧、恐有慢性冠狀動脈心臟病。"))
                    }
                  },
                  "sex"={
                    # 副交感神經 (SEX)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("嚴重"),
                           enc2utf8("副交感神經過低，表示精神不濟、睡眠品質不好，需要調整生活作息及睡眠品質。"))  
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意"),
                           enc2utf8("副交感神經過低，表示精神不濟，建議調整生活作息，使身體或得充分休息。")) 
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           enc2utf8("正常數值"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意"),
                           enc2utf8("數值偏高表示身體可能已經有一些慢性發炎的情況產生，建議多休息。"))
                    } else {
                      list(enc2utf8("嚴重"),
                           enc2utf8("數值過高表示身體發炎長期抵抗力不足，容易產生過敏、精神不濟、消化不良、血壓偏低等問題。"))
                    }
                  },
                  "health"={
                    # 自律神經整體活性 (HEALTH)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("嚴重"),
                           enc2utf8("健康指數較低，可能成為慢性疾病的風險族群。")) 
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意"),
                           enc2utf8("健康指數稍微偏低，建議可做適度運動。"))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           enc2utf8("正常數值"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意"),
                           enc2utf8("有心律不整現象，易發生心肌梗塞之風險，建議進一步追蹤治療。"))
                    } else {
                      list(enc2utf8("嚴重"),
                           enc2utf8("數值偏高嚴重心律不整的問題，恐有猝死風險，建議進一步向醫師諮詢。"))
                    }
                  },
                  "fight"={
                    # 交感神經 (FIGHT)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("嚴重"),
                           enc2utf8("交感神經過低，可能有過敏體質、精神不振、易冷、暈眩、消化不良等症狀。"))
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意"),
                           enc2utf8("交感神經偏低，身體容易無精打采，需要多注意身體狀況。"))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           enc2utf8("正常數值"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("注意"),
                           enc2utf8("數值偏高，可能會有焦慮緊張等問題，須多加注意。"))
                    } else {
                      list(enc2utf8("嚴重"),
                           enc2utf8("數值過高，容易造成焦慮緊張，苦能會有失眠、易怒、煩躁等問題，長期下來恐造成精神衰弱。")) 
                    }
                  },
                  "vital"={
                    # 總體神經功能 (VITAL)
                    if (value >= 1 && value <= 20) {
                      list(enc2utf8("嚴重"),
                           enc2utf8("數值過低，表示精力虛衰，對應生理年齡嚴重老化，身體元氣力嚴重不足，恐有重大隱疾。"))
                    } else if (value >= 21 && value <= 40) {
                      list(enc2utf8("注意"),
                           enc2utf8("數值偏低，表示精力急速衰退，建議改變生活形態，身體進一步進廠保養。"))
                    } else if (value >= 41 && value <= 60) {
                      list(enc2utf8("正常"),
                           enc2utf8("正常數值"))
                    } else if(value >= 61 && value <= 80) {
                      list(enc2utf8("健康"),
                           enc2utf8("數值偏高，表示自律神經活性較一般人高，自律神經狀況較同年齡好。"))
                    } else {
                      list(enc2utf8("健康"),
                           enc2utf8("數值極高，表示自律神經總體功能愈強，顯示元氣強、精力旺盛。"))
                    }
                  })
  return(result)
}