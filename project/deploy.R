
library(rsconnect)

# setwd("~/Github/natt/")
setwd("E:/Github/natt/")


# change instance type
# rsconnect::configureApp(account="guitarmind", "utc-demo", size="large")

# deploy to remote app
rsconnect::deployApp(account="guitarmind", appDir='project/hrv', appName="hrv")

# show logs
rsconnect::showLogs(account="guitarmind", appName="hrv")
