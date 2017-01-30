library(rvest)
library(httr)#GET POST
library(magrittr)# %>%
library(xml2)# read_html , read_nodes ,read_httr
library(stringr)#str_sub

URL_lists = data.frame()

URL_HisLists = data.frame()

for(i in 1:19){ #page crawler 
  
  url <- 'http://class.ruten.com.tw/category/sub00.php?c=00110022&p='
  
  URL <- paste0(url,i)
  
  URL_list <- read_html(URL) %>% html_nodes("a.item-name-anchor") %>% html_attr('href')
  
  URL_lists <- append(URL_lists,URL_list) #%>% as.data.frame(., as.data.frame(., stringsAsFactors = F))
  
  for(i in length(URL_lists)){
    
    history <-'#history&p=1'
    
    URL_HisLists <- paste0(URL_lists,history)
  
  }
  Sys.sleep(runif(1,2,4))
}  

URL_HisLists <- as.character(URL_HisLists)
URL_lists <- URL_HisLists
class(URL_lists)

cookies <- httr::set_cookies("URL_lists[i]" , cookie = "_ts_id=34033506320B3F073001")
uagent <- httr::user_agent("Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Mobile Safari/537.36")

# uagent = session , My request data to HTML server . 

rutenCrawler_OK <- function(ruten_url){
  
  session <- GET((URL_lists[i]),add_headers('user-agent' = 'uagent'))
  
  CMTitle <- read_html(session) %>%
    
    html_nodes(".item-title") %>% 
    
    html_text() #as.numeric()
  
  CMTime <- read_html(session) %>% 
    
    html_nodes(".upload-time") %>%
    
    html_text() %>% 
    
    str_sub(start = 7 ,end = 25) 
  
    #%>% as.POSIXct()
    
  CMPrice <- read_html(session) %>% 
      
    html_nodes(".rt-text-xx-large") %>% 
      
    html_text()
  
  CMSole <- read_html(session) %>% 
    
    html_nodes(".rt-text-x-large") %>% 
      
    html_text() %>% .[4]
    
  CMRemainig <- read_html(session) %>% 
    
    html_nodes(".rt-text-x-large") %>% 
    
    html_text() %>% .[3]
    
  CMSellerProducts <- read_html(session) %>% 
    
    html_nodes(".rt-text-x-large") %>% 
    
    html_text() %>% .[2]
    
  CMSellerEvaluation <- read_html(session) %>% 
    
    html_nodes(".rt-text-x-large") %>% 
    
    html_text() %>% .[1]
  
  CMask <- read_html(session) %>% 
    
    html_nodes(xpath='//*[@class="count"]') %>% 
    
    html_text() %>% .[1]
  
  CManswer <- read_html(session) %>% 
    
    html_nodes(xpath='//*[@class="count"]') %>% 
    
    html_text() %>% .[2]
  
  data.frame(CMTitle,
             CMTime,
             CMPrice,
             CMSole,
             CMRemainig,
             CMSellerProducts,
             CMSellerEvaluation,
             CMask,
             CManswer)
}
  
for(i in 1:length(URL_lists)){
  
    if(i == 1){
    
      finalData <- rutenCrawler_OK(i)
    
      }else{
    
        finalData <- rbind(finalData,rutenCrawler_OK(i))
    }
    
    Sys.sleep(runif(1,2,5))  
}

