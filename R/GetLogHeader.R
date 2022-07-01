#' Pull List of Wells from Motive's REST API for a given customer
#'
#' @param hub Which customer hub to use
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @param uidWell Unique Identifier for the Parent Well
#' @param uid Unique Identifier for the Log
#' @return A nested list with all log headers for a given well
#' @examples
#' RWITSML::GetLogHeader(url = "https://hub.us.pason.com/hub/witsml/store",
#'                        user = hpidc::GetKeys()$pason$user,
#'                        password = hpidc::GetKeys()$pason$password,
#'                        uidWell = "us_27990298",
#'                        uid = "us_27990298_wb1_log_dfr_time_1s",
#'                        verbose = FALSE)
#'

GetLogHeader <- function(url = NULL,
                         user = NULL,
                         password = NULL,
                         uidWell = NULL,
                         uid = NULL,
                         verbose = FALSE) {

  Q <- paste0('
  <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
    <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
      <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
        <WMLtypeIn xsi:type="xsd:string">log</WMLtypeIn>
        <QueryIn xsi:type="xsd:string">&lt;logs xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
        &lt;log uidWell="',uidWell,'" uidWellbore="" uid="',uid,'"&gt;
          &lt;nameWell /&gt;
          &lt;nameWellbore /&gt;
          &lt;name /&gt;
          &lt;objectGrowing /&gt;
          &lt;dataRowCount /&gt;
          &lt;serviceCompany /&gt;
          &lt;runNumber /&gt;
          &lt;bhaRunNumber /&gt;
          &lt;pass /&gt;
          &lt;creationDate /&gt;
          &lt;description /&gt;
          &lt;indexType /&gt;
          &lt;startIndex uom="" /&gt;
          &lt;endIndex uom="" /&gt;
          &lt;stepIncrement uom="" numerator="" denominator="" /&gt;
          &lt;startDateTimeIndex /&gt;
          &lt;endDateTimeIndex /&gt;
          &lt;direction /&gt;
          &lt;indexCurve columnIndex="" /&gt;
          &lt;nullValue /&gt;
          &lt;logParam index="" name="" uom="" description="" /&gt;
          &lt;logCurveInfo uid=""&gt;
            &lt;mnemonic /&gt;
            &lt;classWitsml /&gt;
            &lt;unit /&gt;
            &lt;mnemAlias /&gt;
            &lt;nullValue /&gt;
            &lt;alternateIndex /&gt;
            &lt;wellDatum uidRef="" /&gt;
            &lt;minIndex uom="" /&gt;
            &lt;maxIndex uom="" /&gt;
            &lt;minDateTimeIndex /&gt;
            &lt;maxDateTimeIndex /&gt;
            &lt;columnIndex /&gt;
            &lt;curveDescription /&gt;
            &lt;sensorOffset uom="" /&gt;
            &lt;dataSource /&gt;
            &lt;densData uom="" /&gt;
            &lt;traceState /&gt;
            &lt;traceOrigin /&gt;
            &lt;typeLogData /&gt;
            &lt;axisDefinition uid=""&gt;
              &lt;order /&gt;
              &lt;count /&gt;
              &lt;name /&gt;
              &lt;propertyType /&gt;
              &lt;uom /&gt;
              &lt;doubleValues /&gt;
              &lt;stringValues /&gt;
            &lt;/axisDefinition&gt;
          &lt;/logCurveInfo&gt;
          &lt;commonData&gt;
            &lt;sourceName /&gt;
            &lt;dTimCreation /&gt;
            &lt;dTimLastChange /&gt;
            &lt;itemState /&gt;
            &lt;comments /&gt;
          &lt;/commonData&gt;
          &lt;customData /&gt;
          &lt;/log&gt;
        &lt;/logs&gt;</QueryIn>
        <OptionsIn xsi:type="xsd:string">returnElements=requested;compression=gzip;</OptionsIn>
      </q1:WMLS_GetFromStore>
    </soap:Body>
  </soap:Envelope>
  ')

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

  Start <- stringi::stri_locate_first_fixed(Raw,"<log ")[1,"start"]
  End <- stringi::stri_locate_last_fixed(Raw,"</log>")[1,"end"]

  X <- XML::xmlParseString(substr(Raw,Start,End))
  Root <- XML::xmlRoot(X)

  LogDetails <- XML::xmlToList(Root)[['log']]

  logCurveInfoDT <- rbindlist(LogDetails[which(names(LogDetails) == "logCurveInfo")])
  LogDetails$logCurveInfoDT <- logCurveInfoDT

  return(LogDetails)
}
