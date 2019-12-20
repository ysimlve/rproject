

###############################################GOOGLE API Search###############################################
################textSearch - Google Places API
################input - query e.g. c("淞沪路234号", "杨浦",             "上海"      ,       "中国" )
################output - PlaceID e.g. ChIJZ-hP579zsjURalSz06B0qOE
google.api.textSearch <- function(query, key, language){
  placeid = ""
  
  query_1 <- paste0(query[1],",",query[2],",",query[3],",",query[4])
  root <- "https://maps.googleapis.com/maps/api/place/textsearch/json"
  u <- paste0(root, "?query=", query_1, "&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status == "OK"){
    #select the best matched result
    result <- x$results[[1]]   
    rsl_placeid <- result$place_id
    placeid <- rsl_placeid
    }else{
    print("Google can not find anything according to your input!")
  }
  return(placeid)
}


################textSearch - Google Places API
################input - PlaceID e.g. ChIJZ-hP579zsjURalSz06B0qOE
################output - v_output
################potential types:
################types : [ "point_of_interest", "establishment" ]  
################types : [ "street_address" ]
################types : [ "route" ]
################types : [ "premise" ]
google.api.detailSearch <- function(placeid, key, language, in_pty_name){
  #####Define output
  v_output <- rep("",11)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website")
  
  root <- "https://maps.googleapis.com/maps/api/place/details/json?placeid="
  u <- paste0(root,placeid,"&language=", language, "&key=", key)
  doc <- getURL(URLencode(u))
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status == "OK"){
    result <- x$result
    rsl_types <- result$types
    
    if(any(rsl_types == "establishment")){
      #print("The found place is an establishment!")
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- result$name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      if(v_output[which(attr(v_output,"Elements") == "ADDRESS")] == ""){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- result$vicinity
      }
      
    }else if(any(rsl_types == "street_address")){
      #print("The found place is a street_address")
      
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- ""
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
                 v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      property_name <- google.api.reverse.geocode(latlng,language = language, key = key)
      if(property_name == ""){  #if property_name is null, try nearbysearch
        property_name = google.api.nearbysearch(latlng,language = language, key = key,placetype = "street_address",in_pty_name = in_pty_name)
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
      }else{
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
      }
    }else if(any(rsl_types == "route")){
      #print("The found place is a route")
      
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
                 v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      
      property_name = google.api.nearbysearch(latlng,language = language, key = key,placetype = "route",in_pty_name = in_pty_name)
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
    }else if(any(rsl_types == "premise")){
      #print("The found place is a premise!")
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- result$name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- result$geometry$location$lat  
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- result$geometry$location$lng  
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- ifelse(is.null(result$formatted_phone_number), "", result$formatted_phone_number)
      v_output[which(attr(v_output,"Elements") == "place_id")] <- ifelse(is.null(result$place_id), "", result$place_id)
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- ifelse(is.null(result$url), "", result$url)
      v_output[which(attr(v_output,"Elements") == "website")] <- ifelse(is.null(result$website), "", result$website)
      
      street_number <- ""
      route <- ""
      district <- ""
      city <- ""
      postal_code <- ""
      address_components <- result$address_components
      for(i in 1:length(address_components)){
        if(any(address_components[[i]]$types == "street_number")){
          street_number <-  address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "route")){
          route <- address_components[[i]]$long_name
        }else if(any(address_components[[i]]$types == "sublocality")){
          district <- address_components[[i]]$long_name
          district <- gsub("(区$|\\sQu)","",district)
        }else if(any(address_components[[i]]$types == "locality")){
          city <- address_components[[i]]$long_name
          city <- gsub("(市$|\\s{0,1}[Ss]hi)","",city)
        }else if(any(address_components[[i]]$types == "postal_code")){
          postal_code <- address_components[[i]]$long_name
        }
      }
      if(language == "zh-CN"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(route,street_number)
      }else if(language == "en"){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- paste0(street_number," ", route)
      }
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
      if(gsub("\\s{1,}","",v_output[which(attr(v_output,"Elements") == "ADDRESS")]) == ""){
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- result$vicinity
      }
      
      #if(v_output[which(attr(v_output,"Elements") == "ADDRESS")] == ""){
      #  latlng = c(v_output[which(attr(v_output,"Elements") == "GEO_LAT")],
      #             v_output[which(attr(v_output,"Elements") == "GEO_LONG")])
      #  v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- google.api.nearbysearch(latlng, language = language, key = key,placetype = "premise", in_pty_name = in_pty_name)
      #}
      
    }
  }else{
    print("Placeid you inputed is incorrect!")
  }
  return(v_output)
}


################reverse.geocode - Google GeoCoding API
################input - latlng e.g. c(31.30482 121.51377)
################output - property_name
################potential types: https://developers.google.com/maps/documentation/geocoding/intro?hl=zh-tw#Types
################types : [ "premise"]  
################types : ["point_of_interest"]
google.api.reverse.geocode <- function(latlng,language,key,result_type="premise|point_of_interest|establishment"){
  #####Define output
  property_name = ""
  
  if(length(latlng) != 2){
    print("Please input correct Lat and Long value!")
    return()
  }else{
    root = "https://maps.googleapis.com/maps/api/geocode/json?latlng="
    u <- paste0(root,paste0(latlng[1],",",latlng[2]),"&result_type=",result_type,"&language=",language,"&key=",key)
    doc <- getURL(URLencode(u))
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status == "OK"){
      result <- x$results[[1]]
      
      if(length(result$types)<=2 & (any(result$types == "premise") | any(result$types == "point_of_interest") | any(result$types == "establishment"))){
        for(i in 1:length(result$address_components)){
          type_ac <- result$address_components[[i]]$types
          if(any(type_ac == "premise")){
            property_name <- result$address_components[[i]]$long_name
            break
          }
        }
      }
      
    }else{
      print("Google can not find anything according to your input!")
    }
  }
  
  return(property_name)
}


################nearbysearch - Google Place API
################input - location = c("31.2596854","121.5127786")
################input - radius e.g. 100
################output - property_name
google.api.nearbysearch <- function(location,language, key, placetype = "route", radius = 1000, types="establishment|point_of_interest", in_pty_name){
  #placetype - route,street_address,premise
  
  #####Define output
  s_output = ""
  
  if(length(location) != 2){
    print("Please input correct Lat and Long value!")
    return()
  }else{
    root = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
    u <- paste0(root,paste0(location[1],",",location[2]),"&radius=",radius,"&types=",types,"&language=",language,"&key=",key)
    doc <- getURL(URLencode(u))
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status == "OK"){
      results_pre <- x$results
      n_result <- length(results_pre)
      list_num <- rep(0,n_result)
      for(i in 1:n_result){
        rlt_type <- results_pre[[i]]$types
        if(length(rlt_type)<=2 & (any(rlt_type=="establishment") | any(rlt_type=="point_of_interest"))){
          list_num[i] <- 1
        }
      }
      results <- results_pre[which(list_num==1)]
      result <- results[[1]]
      
      if(placetype == "street_address"){
        rlt_pty_name <- rep("",length(results))
        str_dist_pty_name <- rep(1,length(results))
        dst_diff <- rep(1,length(results))
        for(i in 1:length(results)){
          rlt_pty_name[i] <- results[[i]]$name
          str_dist_pty_name[i] <- stringdist(rlt_pty_name[i],in_pty_name)/max(nchar(rlt_pty_name[i]),nchar(in_pty_name))
          
          r_lat <- results[[i]]$geometry$location$lat
          r_lat <- ifelse(is.null(r_lat), 180, r_lat)
          r_long <- results[[i]]$geometry$location$lng
          r_long <- ifelse(is.null(r_long), 180, r_long)
          diff_lat <- abs(r_lat - as.numeric(location[1]))
          diff_long <- abs(r_long - as.numeric(location[2]))
          diff_3 <- sqrt(diff_lat^2 + diff_long^2)
          
          dst_diff[i] <- diff_3
        }
        
        rlt_num <- 1
        if(any(str_dist_pty_name < 0.7)){
          rlt_num <- which(str_dist_pty_name == min(str_dist_pty_name))[1]
        }else{
          rlt_num <- which(dst_diff == min(dst_diff))[1]
        }
        
        result <- results[[rlt_num]]
        s_output = result$name
      }
      
    }else{
      print("Google can not find anything according to your input!")
    }
  }
  return(s_output)
}


################google.api - a general function
################output - v_output
google.api <- function(query, key, language){
  placeid <- google.api.textSearch(query, key, language)
  out_put <- google.api.detailSearch(placeid, key, language)
  return(out_put)
}



###############################################BAIDU API Search###############################################
baidu.api.poiSearch <- function(query,region,key,tag="房地产,酒店,写字楼",scope=2,page_size=1,output="json"){
  uid = ""
  
  root <- "http://api.map.baidu.com/place/v2/search"
  u <- paste0(root, "?q=", query, "&region=",region,"&city_limit=true","&tag=", tag, "&scope=",scope,"&page_size=",page_size,"&output=",output,"&ak=", key)
  doc <- getURL(u)
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(length(x$results) > 0){
      if(x$status==0 & !is.null(x$results[[1]]$uid)){
        results <- x$results
        result <- results[[1]]
        uid <- result$uid
    }
  }
  }else{
    print("Google can not find anything according to your input!")
  }
  return(uid)
}


baidu.api.detailSearch <- function(uid,region,key,output="json",scope=2){
  #####Define output
  v_output <- rep("",11)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website")
  
  if(uid != ""){
    root <- "http://api.map.baidu.com/place/v2/detail?"
    u <- paste0(root, "uid=", uid, "&output=",output,"&scope=",scope,"&ak=", key)
    doc <- getURL(URLencode(u))
    
    if(doc != "" & !is.null(doc)){
      x <- fromJSON(doc,simplify = FALSE)
      if(x$status==0 & x$result$uid == uid){
        result <- x$result
        place_id <- result$uid
        property_name <- ifelse(is.null(result$name), "", result$name) 
        address <- ifelse(is.null(result$address), "", result$address) 
        website <- ifelse(is.null(result$detail_info$detail_url), "", result$detail_info$detail_url)  
        phone_number <- ifelse(is.null(result$telephone), "", result$telephone) 
        geo_lat <- ifelse(is.null(result$location$lat), "", result$location$lat)
        geo_long <- ifelse(is.null(result$location$lng), "", result$location$lng)
        gmap_url <- ""
        district <- ""
        postal_code <- ""
        
        v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
        v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- geo_lat
        v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- geo_long
        v_output[which(attr(v_output,"Elements") == "phone_number")] <- phone_number
        v_output[which(attr(v_output,"Elements") == "place_id")] <- place_id
        v_output[which(attr(v_output,"Elements") == "gmap_url")] <- gmap_url
        v_output[which(attr(v_output,"Elements") == "website")] <- website
        v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- address
        v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
        v_output[which(attr(v_output,"Elements") == "CITY")] <- region
        v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
        
      }
    }else{
      print("Google can not find anything according to your input!")
    }
  }
  return(v_output)
  
}


baidu.api.geocoder <- function(address,city,key,output="json"){
  location = ""
  
  root <- "http://api.map.baidu.com/geocoder/v2/?"
  u <- paste0(root, "&address=", address, "&city=",city,"&output=",output,"&ak=", key)
  doc <- getURL(u)
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(length(x$result) > 0){
      if(x$status==0){
        geo_lat <- x$result$location$lat
        geo_long <- x$result$location$lng
        
        location <- c(as.character(geo_lat),as.character(geo_long))
      }
    }
  }else{
    print("Google can not find anything according to your input!")
  }
  return(location)
}


baidu.api.nearbySearch <- function(location,city,key,type="写字楼$房地产$酒店",scope=2,radius=1000,output="json"){
  #####Define output
  v_output <- rep("",11)
  attr(v_output, "Elements") <- c("PROPERTY_NAME", "ADDRESS", "DISTRICT", "CITY", "GEO_LAT", "GEO_LONG", "postal_code", "phone_number","place_id","gmap_url","website")
  
  root <- "http://api.map.baidu.com/place/v2/search?"
  u <- paste0(root, "query=", type, "&scope=",scope,"&location=",paste0(location[1],",",location[2]),"&radius=",radius,"&output=",output,"&ak=", key)
  doc <- getURL(URLencode(u))
  
  if(doc != "" & !is.null(doc)){
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status==0 & length(x$results) > 0){
      results <- x$results
      n_rlt <- length(results)
      result <- results[[1]]
      
      dst_diff <- rep(1,n_rlt)
      for(i in 1:n_rlt){
        r_lat <- results[[i]]$location$lat
        r_lat <- ifelse(is.null(r_lat), 180, r_lat)
        r_long <- results[[i]]$location$lng
        r_long <- ifelse(is.null(r_long), 180, r_long)
        diff_lat <- abs(r_lat - as.numeric(location[1]))
        diff_long <- abs(r_long - as.numeric(location[2]))
        diff_3 <- sqrt(diff_lat^2 + diff_long^2)
        
        dst_diff[i] <- diff_3
      }
      rlt_num <- which(dst_diff == min(dst_diff))[1]
      result <- results[[rlt_num]]
      
      place_id <- result$uid
      property_name <- ifelse(is.null(result$name), "", result$name) 
      address <- ifelse(is.null(result$address), "", result$address) 
      website <- ifelse(is.null(result$detail_info$detail_url), "", result$detail_info$detail_url)  
      phone_number <- ifelse(is.null(result$telephone), "", result$telephone) 
      geo_lat <- ifelse(is.null(result$location$lat), "", result$location$lat)
      geo_long <- ifelse(is.null(result$location$lng), "", result$location$lng)
      gmap_url <- ""
      district <- ""
      postal_code <- ""
      
      v_output[which(attr(v_output,"Elements") == "PROPERTY_NAME")] <- property_name
      v_output[which(attr(v_output,"Elements") == "GEO_LAT")] <- geo_lat
      v_output[which(attr(v_output,"Elements") == "GEO_LONG")] <- geo_long
      v_output[which(attr(v_output,"Elements") == "phone_number")] <- phone_number
      v_output[which(attr(v_output,"Elements") == "place_id")] <- place_id
      v_output[which(attr(v_output,"Elements") == "gmap_url")] <- gmap_url
      v_output[which(attr(v_output,"Elements") == "website")] <- website
      v_output[which(attr(v_output,"Elements") == "ADDRESS")] <- address
      v_output[which(attr(v_output,"Elements") == "DISTRICT")] <- district
      v_output[which(attr(v_output,"Elements") == "CITY")] <- city
      v_output[which(attr(v_output,"Elements") == "postal_code")] <- postal_code
      
    }
  }else{
    print("Google can not find anything according to your input!")
  }
  return(v_output)
}


###############################################API Search###############################################
API_Search <- function(DS_PreAPI,baidu_key,google_key){
  if(!any(colnames(DS_PreAPI) == gv_geocore_fields)) {
    print("Please check the fileds of your input.")
    return()
  }
  
  DS_PostAPI <- cbind(DS_PreAPI[1,],postal_code="",phone_number="",map_url="",website="")[0,]
  
  rownum <- nrow(DS_PreAPI)
  DS_PreAPI_Remarked <- uf_validityCheck_ALL(DS_PreAPI)
  
  
  for(i in 1:rownum){
    #print(as.character(DS_PreAPI[i,1]))
    #print(DS_PreAPI$PROPERTY_NAME[i])
    #define best output
    v_best <- rep("",14)
    attr(v_best, "Elements") <- c("PROPERTY_NAME","PROPERTY_NAME_LC","ADDRESS_1","ADDRESS_LC","DISTRICT_NAME","DISTRICT_NAME_LC", 
                                  "CITY_NAME","CITY_NAME_LC", "GEO_LAT", "GEO_LONG","postal_code","phone_number","map_url","website")
    
    p_country <- "中国"
    p_country_en <- "China"
    p_city <- ifelse(DS_PreAPI$CITY_NAME_LC[i] == "", DS_PreAPI$CITY_NAME[i],DS_PreAPI$CITY_NAME_LC[i])
    p_district <- ifelse(DS_PreAPI$DISTRICT_NAME_LC[i] == "", DS_PreAPI$DISTRICT_NAME[i],DS_PreAPI$DISTRICT_NAME_LC[i])
    
    #chinese original data information is avaliable
    if(DS_PreAPI_Remarked$Remark_PROPERTY_NAME_LC[i] %in% c("Partial-Valid","Valid") | DS_PreAPI_Remarked$Remark_ADDRESS_LC[i] %in% c("Partial-Valid","Valid")){
      p_pty <- DS_PreAPI$PROPERTY_NAME_LC[i]
      p_addr <- DS_PreAPI$ADDRESS_LC[i]
      op.baidu.pty_lc <- baidu.api.detailSearch(baidu.api.poiSearch(p_pty,p_city,baidu_key),p_city,baidu_key)
      op.google.pty_lc <- google.api.detailSearch(google.api.textSearch(c(p_pty,p_district,p_city,p_country),google_key,"zh-CN"), google_key, "zh-CN",DS_PreAPI$PROPERTY_NAME_LC[i]) 
      op.baidu.addr_lc <- baidu.api.nearbySearch(baidu.api.geocoder(p_addr,p_city,baidu_key),p_city,baidu_key)
      op.google.addr_lc <- google.api.detailSearch(google.api.textSearch(c(p_addr,p_district,p_city,p_country),google_key,"zh-CN"),google_key,"zh-CN",DS_PreAPI$PROPERTY_NAME_LC[i]) 
      op.baidu.addr_lc[2] <- op.google.addr_lc[2]
      
      op.baidu.pty_lc[1:6]
      op.google.pty_lc[1:6]
      op.baidu.addr_lc[1:6]
      op.google.addr_lc[1:6]
      
      pty_org <- DS_PreAPI$PROPERTY_NAME_LC[i]
      addr_org <- DS_PreAPI$ADDRESS_LC[i]
      
      pty_baidu_pty <- op.baidu.pty_lc[1]
      pty_google_pty <- op.google.pty_lc[1]
      str_dist_pty_pty <- stringdist(c(pty_baidu_pty,pty_google_pty),pty_org)/c(max(nchar(pty_baidu_pty),nchar(pty_org)),max(nchar(pty_google_pty),nchar(pty_org)))
      
      addr_baidu_pty <- op.baidu.pty_lc[2]
      addr_google_pty <- op.google.pty_lc[2]
      str_dist_addr_pty <- stringdist(c(addr_baidu_pty,addr_google_pty),addr_org)/c(max(nchar(addr_baidu_pty),nchar(addr_org)),max(nchar(addr_google_pty),nchar(addr_org)))
      
      pty_baidu_addr <- op.baidu.addr_lc[1]
      pty_google_addr <- op.google.addr_lc[1]
      str_dist_pty_addr <- stringdist(c(pty_baidu_addr,pty_google_addr),pty_org)/c(max(nchar(pty_baidu_addr),nchar(pty_org)),max(nchar(pty_google_addr),nchar(pty_org)))
      
      addr_baidu_addr <- op.baidu.addr_lc[2]
      addr_google_addr <- op.google.addr_lc[2]
      str_dist_addr_addr <- stringdist(c(addr_baidu_addr,addr_google_addr),addr_org)/c(max(nchar(addr_baidu_addr),nchar(addr_org)),max(nchar(addr_google_addr),nchar(addr_org)))
      
      #if geocode of api output by Property is not null
      if((op.baidu.pty_lc[5] != "" | op.google.pty_lc[5] != "") & min(str_dist_pty_pty) <= 0.8){
        #core logic of selection
        str_dist_pty <- str_dist_pty_pty * 0.65 + str_dist_addr_pty * 0.35
        if(which(str_dist_pty == min(str_dist_pty))[1] == 1){
          v_best[c(2,4,6,8,9:14)] <- op.baidu.pty_lc[-9]
        }else{
          v_best[c(2,4,6,8,9:14)] <- op.google.pty_lc[-9]
        }
        v_best[6] <- ifelse(v_best[6] == "",op.google.pty_lc[3],v_best[6])
      }else{
        if(which(str_dist_pty_addr == min(str_dist_pty_addr))[1] == 1){
          v_best[c(2,4,6,8,9:14)] <- op.baidu.addr_lc[-9]
        }else{
          v_best[c(2,4,6,8,9:14)] <- op.google.addr_lc[-9]
        }
        v_best[6] <- ifelse(v_best[6] == "",op.google.addr_lc[3],v_best[6])
      }
      
      #get english api output
      op.google.pty_eng <- google.api.detailSearch(google.api.textSearch(c(v_best[2],v_best[6],v_best[8],p_country),google_key,"en"), google_key, "en",DS_PreAPI$PROPERTY_NAME[i]) 
      op.google.addr_eng <- google.api.detailSearch(google.api.textSearch(c(v_best[4],v_best[6],v_best[8],p_country),google_key,"en"),google_key,"en",DS_PreAPI$PROPERTY_NAME[i]) 
      
      if(op.google.pty_eng[1] != "" & op.google.pty_eng[2] != ""){
        v_best[c(1,3,5,7)] <- op.google.pty_eng[1:4]
      }else{
        v_best[c(1,3,5,7)] <- op.google.addr_eng[1:4]
      }
      
      
    }else if(DS_PreAPI_Remarked$Remark_PROPERTY_NAME[i] %in% c("Partial-Valid","Valid") | DS_PreAPI_Remarked$Remark_ADDRESS_1[i] %in% c("Partial-Valid","Valid")){
      #only english information is available
      p_pty_en <- DS_PreAPI$PROPERTY_NAME[i]
      p_addr_en <- DS_PreAPI$ADDRESS_1[i]
      #p_addr_en <- "6 Wenchanglu"
      
      op.google.pty_cn <- google.api.detailSearch(google.api.textSearch(c(p_pty_en,p_district,p_city,p_country_en),google_key,"zh-CN"), google_key, "zh-CN",DS_PreAPI$PROPERTY_NAME[i]) 
      op.google.addr_cn <- google.api.detailSearch(google.api.textSearch(c(p_addr_en,p_district,p_city,p_country_en),google_key,"zh-CN"),google_key,"zh-CN",DS_PreAPI$PROPERTY_NAME[i]) 
      op.google.pty_en <- google.api.detailSearch(google.api.textSearch(c(p_pty_en,p_district,p_city,p_country_en),google_key,"en"), google_key, "en",DS_PreAPI$PROPERTY_NAME[i]) 
      op.google.addr_en <- google.api.detailSearch(google.api.textSearch(c(p_addr_en,p_district,p_city,p_country_en),google_key,"en"),google_key,"en",DS_PreAPI$PROPERTY_NAME[i]) 
      
      #op.google.pty_cn[1:6]
      #op.google.addr_cn[1:6]
      #op.google.pty_en[1:6]
      #op.google.addr_en[1:6]
      
      
      pty_org_en <- DS_PreAPI$PROPERTY_NAME[i]
      addr_org_en <- DS_PreAPI$ADDRESS_1[i]
      
      pty_pty_cn <- op.google.pty_cn[1]
      addr_pty_cn <- op.google.pty_cn[2]
      
      pty_addr_cn <- op.google.addr_cn[1]
      addr_addr_cn <- op.google.addr_cn[2]
      
      pty_pty_en <- op.google.pty_en[1]
      addr_pty_en <- op.google.pty_en[2]
      
      pty_addr_en <- op.google.addr_en[1]
      addr_addr_en <- op.google.addr_en[2]
      
      op.google.pty_cn[5] <- ifelse(op.google.pty_cn[5] == "", "0.0",op.google.pty_cn[5])
      op.google.pty_cn[6] <- ifelse(op.google.pty_cn[6] == "", "0.0",op.google.pty_cn[6])
      op.google.addr_cn[5] <- ifelse(op.google.addr_cn[5]=="",op.google.pty_cn[5],op.google.addr_cn[5])
      op.google.addr_cn[6] <- ifelse(op.google.addr_cn[6]=="",op.google.pty_cn[6],op.google.addr_cn[6])
      diff_lat <- abs(as.numeric(op.google.pty_cn[5])  - as.numeric(op.google.addr_cn[5]))
      diff_long <- abs(as.numeric(op.google.pty_cn[6])  - as.numeric(op.google.addr_cn[6]))
      if(pty_pty_cn != "" & addr_pty_cn != "" & diff_lat < 0.002 & diff_long < 0.004){
        v_best[c(2,4,6,8,9:14)] <- op.google.pty_cn[-9]
        #get english result
        if(pty_pty_en != "" & addr_pty_en != ""){
          v_best[c(1,3,5,7)] <- op.google.pty_en[1:4]
        }else{
          op.google.pty_en_1 <- google.api.detailSearch(google.api.textSearch(c(pty_pty_cn,op.google.pty_cn[3],op.google.pty_cn[4],p_country_en),google_key,"en"), google_key, "en",pty_org_en) 
          op.google.addr_en_1 <- google.api.detailSearch(google.api.textSearch(c(addr_pty_cn,op.google.pty_cn[3],op.google.pty_cn[4],p_country_en),google_key,"en"),google_key,"en",pty_org_en) 
          if(op.google.pty_en_1[1] != "" & op.google.pty_en_1[2] != ""){
            v_best[c(1,3,5,7)] <- op.google.pty_en_1[1:4]
          }else{
            v_best[c(1,3,5,7)] <- op.google.addr_en_1[1:4]
          }
        }
        
      }else if(pty_addr_cn != "" & addr_addr_cn != ""){
        v_best[c(2,4,6,8,9:14)] <- op.google.addr_cn[-9]
        #get english result
        if(pty_addr_en != "" & addr_addr_en != ""){
          v_best[c(1,3,5,7)] <- op.google.addr_en[1:4]
        }else{
          op.google.pty_en_1 <- google.api.detailSearch(google.api.textSearch(c(pty_addr_cn,op.google.addr_cn[3],op.google.addr_cn[4],p_country_en),google_key,"en"), google_key, "en",pty_org_en) 
          op.google.addr_en_1 <- google.api.detailSearch(google.api.textSearch(c(addr_addr_cn,op.google.addr_cn[3],op.google.addr_cn[4],p_country_en),google_key,"en"),google_key,"en",pty_org_en) 
          if(op.google.pty_en_1[1] != "" & op.google.pty_en_1[2] != ""){
            v_best[c(1,3,5,7)] <- op.google.pty_en_1[1:4]
          }else{
            v_best[c(1,3,5,7)] <- op.google.addr_en_1[1:4]
          }
        }
        
      }else if(pty_pty_en != "" & addr_pty_en != ""){
        print("b")
      }else if(pty_addr_en != "" & addr_addr_en != ""){
        print("c")
      }else{
        v_best[c(2,4,6,8,9:14)] <- op.google.addr_cn[-9]
        v_best[c(1,3,5,7)] <- op.google.addr_en[1:4]
      }
      
      
      
    }
    
    DS_PostAPI[i,1] <- DS_PreAPI$SOURCE_PROPERTY_ID[i]
    DS_PostAPI[i,2] <- v_best[1]
    DS_PostAPI[i,3] <- v_best[2]
    DS_PostAPI[i,4] <- v_best[3]
    DS_PostAPI[i,5] <- v_best[4]
    DS_PostAPI[i,6] <- v_best[5]
    DS_PostAPI[i,7] <- v_best[6]
    DS_PostAPI[i,8] <- v_best[7]
    DS_PostAPI[i,9] <- v_best[8]
    op_city <- ifelse(v_best[8] == "", v_best[7], v_best[8])
    DS_PostAPI[i,10] <- gv_china_division_list[which(gv_china_division_list$CITY == op_city | gv_china_division_list$CITY_LC == op_city)[1],5]
    DS_PostAPI[i,11] <- gv_china_division_list[which(gv_china_division_list$CITY == op_city | gv_china_division_list$CITY_LC == op_city)[1],6]
    DS_PostAPI[i,12] <- "China"
    DS_PostAPI[i,13] <- "中国"
    DS_PostAPI[i,14] <- "UTC/GMT+08:00"
    DS_PostAPI[i,15] <- v_best[9]
    DS_PostAPI[i,16] <- v_best[10]
    DS_PostAPI[i,17] <- v_best[11]
    DS_PostAPI[i,18] <- v_best[12]
    DS_PostAPI[i,19] <- v_best[13]
    DS_PostAPI[i,20] <- v_best[14]
  }
  
  return(DS_PostAPI)
}














































