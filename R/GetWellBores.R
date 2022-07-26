#' Pull a list of wellbores for a given well from a WITSML Store
#'
#' @param url url for the WITSML Data Store
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @param uidWell Unique Identifier for the Parent Well
#' @param verbose if TRUE, verbose output from underlying curl functions will be printed to console. Useful for troubleshooting
#' @return A nested list with all wellbores for a given well
#' @examples
#' RWITSML::GetWellBores(url = "https://hub.us.pason.com/hub/witsml/store",
#'                       user = hpidc::GetKeys()$pason$user,
#'                       password = hpidc::GetKeys()$pason$password,
#'                       uidWell = "us_28147307",
#'                       verbose = FALSE)
#'
#' RWITSML::GetWellBores(url = "https://witsml.welldata.net/witsml/wmls.asmx",
#'                       user = hpidc::GetKeys()$totco$user,
#'                       password = hpidc::GetKeys()$totco$password,
#'                       uidWell = "9ce20147-e1d4-45db-a8cb-bedb3e50b65e",
#'                       verbose = FALSE)

GetWellBores <- function(url = NULL,
                         user = NULL,
                         password = NULL,
                         uidWell = NULL,
                         verbose = FALSE) {
  Q <- paste0('
    <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
      <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
        <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
          <WMLtypeIn xsi:type="xsd:string">wellbore</WMLtypeIn>
          <QueryIn xsi:type="xsd:string">&lt;wellbores xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
          &lt;wellbore uidWell="',uidWell,'" uid=""&gt;
      &lt;nameWell /&gt;
      &lt;name /&gt;
      &lt;parentWellbore uidRef="" /&gt;
      &lt;number /&gt;
      &lt;suffixAPI /&gt;
      &lt;numGovt /&gt;
      &lt;statusWellbore /&gt;
      &lt;purposeWellbore /&gt;
      &lt;typeWellbore /&gt;
      &lt;shape /&gt;
      &lt;dTimKickoff /&gt;
      &lt;achievedTD /&gt;
      &lt;mdCurrent uom="" datum="" /&gt;
      &lt;tvdCurrent uom="" datum="" /&gt;
      &lt;mdKickoff uom="" datum="" /&gt;
      &lt;tvdKickoff uom="" datum="" /&gt;
      &lt;mdPlanned uom="" datum="" /&gt;
      &lt;tvdPlanned uom="" datum="" /&gt;
      &lt;mdSubSeaPlanned uom="" datum="" /&gt;
      &lt;tvdSubSeaPlanned uom="" datum="" /&gt;
      &lt;dayTarget uom="" /&gt;
      &lt;commonData&gt;
        &lt;sourceName /&gt;
        &lt;dTimCreation /&gt;
        &lt;dTimLastChange /&gt;
        &lt;itemState /&gt;
        &lt;comments /&gt;
      &lt;/commonData&gt;
      &lt;customData /&gt;
    &lt;/wellbore&gt;
  &lt;/wellbores&gt;</QueryIn>
        <OptionsIn xsi:type="xsd:string">returnElements=all</OptionsIn>
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

  Start <- stringi::stri_locate_first_fixed(Raw,"<wellbore ")[1,"start"]
  End <- stringi::stri_locate_last_fixed(Raw,"</wellbore>")[1,"end"]

  X <- XML::xmlParseString(substr(Raw,Start,End))
  Root <- XML::xmlRoot(X)

  WellBoreList <- XML::xmlToList(Root,addAttributes = TRUE)

  return(WellBoreList)
}
