library(rvest)
library(xml2)

get_museum_links <- function(page_number) {
  url <- paste0("https://www.culture.ru/museums/institutes/location-moskva?sort=-views&page=", page_number)
  page <- read_html(url, encoding = "UTF-8")
  
  museum_cards <- html_elements(page, ".A5Si4")
  museum_links <- html_attr(html_elements(museum_cards, "a"), "href")
  
  museum_links <- paste0("https://www.culture.ru", museum_links)
  
  return(museum_links)
}

get_museum_info <- function(museum_url) {
  page <- read_html(museum_url, encoding = "UTF-8")
  
  name <- html_text(html_element(page, "h1.zn6w2"))
  address <- html_text(html_element(page, ".K3Yd6"))
  website <- html_text(html_element(page, ".v9LJT .d4gsb a"))
  email <- html_text(html_element(page, ".v9LJT .d4gsb a[href^='mailto:']"))
  phone <- html_text(html_element(page, ".v9LJT .d4gsb a[href^='tel:']"))
  
  if (length(phone) == 0) phone <- NA
  if (length(email) == 0) email <- NA
  if (length(website) == 0) website <- NA
  if (length(address) == 0) address <- NA
  
  name <- iconv(name, from = "UTF-8", to = "UTF-8")
  address <- iconv(address, from = "UTF-8", to = "UTF-8")
  
  return(list(name = name, phone = phone, address = address, website = website))
}

collect_museum_data <- function() {
  all_museums <- data.frame(name = character(), phone = character(), address = character(), website = character(), stringsAsFactors = FALSE)
  
  for (page_number in 1:10) {
    cat(sprintf("Обрабатывается страница %d...\n", page_number))
    museum_links <- get_museum_links(page_number)
    
    for (museum_url in museum_links) {
      cat(sprintf("Загружается информация с '%s'...\n", museum_url))
      museum_info <- get_museum_info(museum_url)
      
      all_museums <- rbind(all_museums, data.frame(
        name = museum_info$name, 
        phone = museum_info$phone, 
        address = museum_info$address, 
        website = museum_info$website, 
        stringsAsFactors = FALSE))
    }
  }
  
  return(all_museums)
}

museums_df <- collect_museum_data()
print(museums_df)