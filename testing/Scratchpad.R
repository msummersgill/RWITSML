library(magrittr)
library(data.table)

url = "https://hub.us.pason.com/hub/witsml/store"
user = hpidc::GetKeys()$pason$user
password = hpidc::GetKeys()$pason$password
verbose = FALSE
uidWell = "us_27990298"
uid = "us_27990298_wb1_log_dfr_time_1s"
curves = c("TIME","DEPT","BDEP")
