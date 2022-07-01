#' Pull List of Wells from Motive's REST API for a given customer
#'
#' @param hub Which customer hub to use
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @return A data.table with all wells available
#' @examples
#' RWITSML::GetWells(url = "https://hub.us.pason.com/hub/witsml/store",
#'                   user = hpidc::GetKeys()$pason$user,
#'                   password = hpidc::GetKeys()$pason$password,
#'                   verbose = TRUE)
#'

GetWells <- function(url = NULL,
                     user = NULL,
                     password = NULL,
                     verbose = FALSE) {
  Q <- '
  <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
    <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
      <WMLtypeIn xsi:type="xsd:string">well</WMLtypeIn>
      <QueryIn xsi:type="xsd:string">&lt;wells xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
      &lt;well uid=""&gt;
      &lt;/well&gt;
      &lt;/wells&gt;</QueryIn>
      <OptionsIn xsi:type="xsd:string">returnElements=all</OptionsIn>
    </q1:WMLS_GetFromStore>
  </soap:Body>
</soap:Envelope>
  '

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

  Start <- stringi::stri_locate_first_fixed(Raw,"<well ")[1,"start"]
  End <- stringi::stri_locate_last_fixed(Raw,"</well>")[1,"end"]

  X <- XML::xmlParseString(substr(Raw,Start,End))
  Root <- XML::xmlRoot(X)

  WellList <- XML::xmlToList(Root,addAttributes = TRUE)

  Wells <- data.table(uid = sapply(Wells, `[[`, ".attrs"),
                      name = sapply(Wells, `[[`, "name"),
                      field = sapply(Wells, `[[`, "field"),
                      country = sapply(Wells, `[[`, "country"),
                      state = sapply(Wells, `[[`, "state"),
                      county = sapply(Wells, `[[`, "county"),
                      timeZone = sapply(Wells, `[[`, "timeZone"),
                      operator = sapply(Wells, `[[`, "operator"),
                      numAPI = sapply(Wells, `[[`, "numAPI"),
                      statusWell = sapply(Wells, `[[`, "statusWell"),
                      dTimSpud = sapply(Wells, `[[`, "dTimSpud"),
                      dTimPa = sapply(Wells, `[[`, "dTimPa")
  )


  return(Wells)
}
