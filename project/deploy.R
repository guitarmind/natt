
library(rsconnect)

# setwd("~/Github/natt/")
setwd("E:/Github/natt/")


# change instance type
# rsconnect::configureApp(account="natt", "hrv-analysis", size="large")

# deploy to remote app
rsconnect::deployApp(account="natt", appDir='project/hrv', appName="hrv-analysis")

# show logs
rsconnect::showLogs(account="natt", appName="hrv-analysis")
