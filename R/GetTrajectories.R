#' Pull List of Wells from Motive's REST API for a given customer
#'
#' @param hub Which customer hub to use
#' @param user Username for the authenticating user
#' @param password Username for the authenticating user
#' @param uidWell Unique Identifier for the Parent Well
#' @return A nested list with all trajectories for a given well
#' @examples
#' RWITSML::GetTrajectories(url = "https://hub.us.pason.com/hub/witsml/store",
#'                          user = hpidc::GetKeys()$pason$user,
#'                          password = hpidc::GetKeys()$pason$password,
#'                          uidWell = "us_28147307"
#'                          verbose = FALSE)
#'

GetTrajectories <- function(url = NULL,
                            user = NULL,
                            password = NULL,
                            uidWell = NULL,
                            verbose = FALSE) {
  Q <- paste0('
<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:soapenc="http://schemas.xmlsoap.org/soap/encoding/" xmlns:tns="http://www.witsml.org/wsdl/120" xmlns:types="http://www.witsml.org/wsdl/120/encodedTypes" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <soap:Body soap:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">
    <q1:WMLS_GetFromStore xmlns:q1="http://www.witsml.org/message/120">
      <WMLtypeIn xsi:type="xsd:string">trajectory</WMLtypeIn>
      <QueryIn xsi:type="xsd:string">&lt;trajectorys xmlns="http://www.witsml.org/schemas/131" version="1.3.1.1"&gt;
      &lt;trajectory uidWell="',uidWell,'" uidWellbore="" uid=""&gt;
      &lt;/trajectory&gt;
      &lt;/trajectorys&gt;</QueryIn>
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

  Start <- stringi::stri_locate_first_fixed(Raw,"<trajectory ")[1,"start"]
  End <- stringi::stri_locate_last_fixed(Raw,"</trajectory>")[1,"end"]

  X <- XML::xmlParseString(substr(Raw,Start,End))
  Root <- XML::xmlRoot(X)

  TrajectoryList <- XML::xmlToList(Root,addAttributes = TRUE)

  return(TrajectoryList)
}
