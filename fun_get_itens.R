fun_get_itens = function(){
  
  require(rvest)
  require(dplyr)
  require(httr)
  require(xml2)
  require(purrr)
  
  df_itens_final = purrr::map_dfr(.x = 1:10, .f = ~{
    url = paste0(
      'https://www.guiamais.com.br/sao-goncalo-rj/tudo-na-regiao?page=',
      .x
    )
    
    usethis::ui_todo(paste0('getting page: ',.x))
    
    html = httr::GET(url)  %>%  # status 200 deu certo
      httr::content() 
    
    itens = html %>% 
      #rvest::html_nodes('div.free')
      rvest::html_nodes('div[itemprop="itemListElement"]')
    
    names = itens %>% 
      rvest::html_nodes('div.left') %>% 
      rvest::html_nodes('h2.aTitle') %>% 
      rvest::html_nodes('a') %>% 
      rvest::html_text() %>% 
      stringr::str_replace_all('\r|\t|\n','')
    
    categories = itens %>% 
      rvest::html_nodes('div.left') %>% 
      rvest::html_nodes('p.advCategory') %>% 
      rvest::html_nodes('a') %>% 
      rvest::html_text() %>% 
      stringr::str_replace_all('\r|\t|\n','')
    
    addresses = itens %>% 
      rvest::html_nodes('div.bottom') %>% 
      rvest::html_nodes('div.advAdress') %>% 
      rvest::html_nodes('address > span') %>% 
      rvest::html_text() %>% 
      stringr::str_replace_all('\r|\t|\n','')
    
    df_itens = tryCatch({
      tibble::tibble(names, categories, addresses)
    },
    error = function(e){
      tibble::tibble(names = NA, categories = NA, addresses = NA)
    })
  })
  
  return(df_itens_final) 
  }


itens = fun_get_itens()    
saveRDS(itens, '~/itens.rds')



require(mapsapi)
require(leaflet)

fun_add_point = function(address){
  api_key = '' #colocar aqui a chave
  
  doc = mp_geocode(
    addresses = address,
    key = api_key,
    quiet = TRUE
  )
  
  pnt = mapsapi::mp_get_points(doc)
  return(pnt) 
  
}


latlong = itens$addresses %>% 
  purrr::imap( ~{
    message(.y)
    fun_add_point(.x)
  })


saveRDS(latlong,'~/latlong.rds')

df = purrr::map_dfr(.x = latlong, .f =~{
  tibble::tibble(
    long = .x$pnt$`1`[1],
    lat = .x$pnt$`1`[2],
    address = latlong[[1]]$address
  )
})

leaflet(df) %>% addTiles() %>%
  addMarkers(~long, ~lat)
