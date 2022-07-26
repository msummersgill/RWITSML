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



url = "https://witsml.welldata.net/witsml/wmls.asmx"
user = hpidc::GetKeys()$totco$user
password = hpidc::GetKeys()$totco$password
verbose = TRUE
uidWell = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e"
uidWellbore = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e"


uid = "Connect-Surveys"


uid = "Depth1"
curves = c("GAMMA_RAY_DEPTH","GAMMA_RAY")
startIndex = NULL
endIndex = NULL


uid = "Time_120_Min"
curves = c("BIT_DEPTH")
startDateTimeIndex = "2022-07-20T00:00:00+00:00"
endDateTimeIndex = "2022-07-20T23:59:59+00:00"



RWITSML::TestHub(url,user,password)

Wells <- RWITSML::GetWells(url,user,password)


RWITSML::GetLogs(url,user,password,uidWell)

RWITSML::GetWellBores(url,user,password,uidWell,verbose)


Handle <- curl::new_handle(verbose = verbose)

curl::handle_setheaders(Handle,
                        "Authorization" = paste0("Basic ",jsonlite::base64_enc(paste0(user,":",password))),
                        "Accept-Encoding" = "gzip",
                        "Accept" = "application/json, text/xml, application/xml, */*",
                        "Content-Type" = "text/xml")

curl::handle_setopt(Handle,.list = list(customrequest = "POST",
                                        postfields = Q))

Result <- curl::curl_fetch_memory(url,Handle)

if(!Result$status_code == 200){
  stop(paste0("HTTP Status Code: ",Result$status_code,
              "\n\nContent: ",rawToChar(Result$content)))
}

Result$content %>%
  rawToChar() %>%
  gsub("&lt;","<", .,fixed = T) %>%
  gsub("&gt;",">", .,fixed = T) -> Raw


substr(Raw,1,10000) %>% cat

Start <- stringi::stri_locate_first_fixed(Raw,"<well ")[1,"start"]
End <- stringi::stri_locate_last_fixed(Raw,"</well>")[1,"end"]

X <- XML::xmlParseString(substr(Raw,Start,End))
Root <- XML::xmlRoot(X)

WellList <- XML::xmlToList(Root,addAttributes = TRUE)

Wells <- data.table(uid = sapply(WellList, `[[`, ".attrs"),
                    name = sapply(WellList, `[[`, "name"),
                    field = sapply(WellList, `[[`, "field"),
                    country = sapply(WellList, `[[`, "country"),
                    state = sapply(WellList, `[[`, "state"),
                    county = sapply(WellList, `[[`, "county"),
                    timeZone = sapply(WellList, `[[`, "timeZone"),
                    operator = sapply(WellList, `[[`, "operator"),
                    numAPI = sapply(WellList, `[[`, "numAPI"),
                    statusWell = sapply(WellList, `[[`, "statusWell"),
                    dTimSpud = sapply(WellList, `[[`, "dTimSpud"),
                    dTimPa = sapply(WellList, `[[`, "dTimPa"))

