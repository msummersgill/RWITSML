## Related Examples:
## http://blog.oilgainsanalytics.com/publications/exploring-drilling-data-from-volve-dataset-witsml-r/

library(magrittr)
library(data.table)

url = "https://hub.us.pason.com/hub/witsml/store"
user = hpidc::GetKeys()$pason$user
password = hpidc::GetKeys()$pason$password
verbose = FALSE
uidWell = "us_27990298"
# uid = "us_27990298_wb1_log_dfr_time_1s"
# curves = c("TIME","DEPT","BDEP")

uid = "us_27990298_wb1_log_1"
curves = c("GAM","INCL","AZ")
startIndex = 1
endIndex = 24645
printProgress = TRUE
