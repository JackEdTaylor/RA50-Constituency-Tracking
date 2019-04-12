library(tidyverse)
library(parlitools)
library(leaflet)
library(webshot)
library(htmlwidgets)
library(htmltools)
library(magick)

# GET DATA

petitionQuery <- function(constituencyID = "E14000530", petitionID = "1075160", track = TRUE) {
  
  petitionID <- as.character(petitionID)
  constituencyID <- as.character(constituencyID)
  
  get_all_dat <- function(csv_url, max_requests = 500) {
    # max page size of 500, so download from following pages until number of entries doesn't increase, or <500 entries added
    csv_url_pageN <- sprintf("%s?_pageSize=%i&_page=0", csv_url, max_requests)
    cat(sprintf("\t%s\n", csv_url_pageN))
    out <- NULL
    tryNr <- 0
    while (is.null(out) && tryNr < 50) {
      tryNr <- tryNr + 1
      try(
        out <- suppressMessages(read_csv(csv_url_pageN))
      )
    }
    
    
    page_nr <- 1
    while(nrow(out)%%max_requests == 0) {
      csv_url_pageN <- sprintf("%s?_pageSize=%i&_page=%i", csv_url, max_requests, page_nr)
      cat(sprintf("\t%s\n", csv_url_pageN))
      csv_url_dat <- suppressMessages(read_csv(csv_url_pageN))
      out <- rbind(out, csv_url_dat)
      page_nr <- page_nr + 1
    }
    
    out
  }
  
  if (constituencyID == "all") {
    if (track) {
      get_all_dat(sprintf("http://eldaddp.azurewebsites.net/epetitions/%s/track.csv", petitionID)) %>%
        rename("number_of_signatures" = "number of signatures")
    } else {
      get_all_dat(sprintf("http://eldaddp.azurewebsites.net/epetitions/%s/signaturesbyconstituency.csv", petitionID)) %>%
        rename("number_of_signatures" = "number of signatures")
    }
  } else {
    if (track) {
      # get the dates column
      dates <- get_all_dat(sprintf("http://eldaddp.azurewebsites.net/epetitions/%s/track.csv", petitionID)) %>%
        rowwise() %>%
        mutate(dateID = tail(strsplit(uri, "/")[[1]], n=1)) %>%
        select(date, dateID)
      
      get_all_dat(sprintf("http://eldaddp.azurewebsites.net/epetitions/%s/signaturesbyconstituency/%s/track.csv", petitionID, constituencyID)) %>%
        select(uri, `number of signatures`, `voting by constituency > gss code`, `voting by constituency > location`, `voting by constituency > member printed`) %>%
        rowwise() %>%
        mutate(dateID = strsplit(uri, "/")[[1]][5]) %>%
        left_join(dates, by = "dateID") %>%
        ungroup() %>%
        rename(
          "gss_code" = "voting by constituency > gss code",
          "location" = "voting by constituency > location",
          "MP" = "voting by constituency > member printed",
          "number_of_signatures" = "number of signatures"
        )
    } else {
      return(NA)
    }
  }
  
}

datNow <- petitionQuery(constituencyID = "all", track=FALSE)

datOverTime <- petitionQuery(constituencyID = "all")

# track changes in all constituencies

allConstituencies <- unique(datNow$`gss code`)

allConstituenciesDataList <- lapply(1:length(allConstituencies), function(const_nr) {
  cat(sprintf("%i/%i\n", const_nr, length(allConstituencies)))
  if (const_nr%%10==0) Sys.sleep(2)  # sleep every 10 requests for 2 seconds to avoid being detected by ddos protection
  const <- allConstituencies[[const_nr]]
  petitionQuery(const)
})

RA50 <- do.call(rbind, allConstituenciesDataList)

# Get proportion of registered voters
RA50 <- left_join(RA50, read_csv("electoral_numbers.csv"), by="gss_code") %>%
  mutate(proportional_signatures = number_of_signatures/electorate2018)

# Got total number of votes from each constituency yesterday

west_hex_map <- parlitools::west_hex_map
RA50_hex_map <- left_join(west_hex_map, RA50, by = "gss_code") %>%
  filter(dateID == max(dateID))

pal <- colorNumeric("Blues", RA50_hex_map$number_of_signatures)

leaflet(options=leafletOptions(
  dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75), list(58.25,50.0)),
  attributionControl = FALSE),
  RA50_hex_map
  ) %>%
  addPolygons(
    color = "grey",
    weight=0.75,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~pal(number_of_signatures)
  ) %>%
  addLegend("topright", pal = pal, values = ~number_of_signatures,
            title = "Number of Signatures",
            opacity = 1)  %>%
  htmlwidgets::onRender(
    "function(x, y) {
    var myMap = this;
    myMap._container.style['background'] = '#fff';
    }"
    ) %>%
  mapOptions(zoomToLimits = "first") %>%
  saveWidget("tmp.html",selfcontained = F)
  
  webshot("tmp.html", "yesterday.png", vwidth = 625, vheight=680)

  file.remove("tmp.html")
  unlink("tmp_files", recursive=TRUE)

##############################################  

# Iterate over all days
  
days <- sort(unique(RA50$dateID))

maxSign <- RA50 %>%
  group_by(dateID) %>%
  summarise(maxSign = max(number_of_signatures)) %>%
  pull(maxSign) %>%
  max() %>%
  plyr::round_any(1000, f = ceiling)

west_hex_map <- parlitools::west_hex_map

# get list of all possible proportions, for consistent colour scale
all_props <- c()
for (day in days) {
  RA50_hex_map <- left_join(west_hex_map, RA50, by = "gss_code") %>%
    filter(dateID == day) %>%
    mutate(proportionSign = number_of_signatures/sum(number_of_signatures))
  all_props <- c(all_props, unique(RA50_hex_map$proportionSign))
}

all_file_locs <- c()
total_yesterday <- 0
for (day in days) {
  
  RA50_hex_map <- left_join(west_hex_map, RA50, by = "gss_code") %>%
    filter(dateID == day) %>%
    mutate(proportionSign = number_of_signatures/sum(number_of_signatures))
  
  day_date <- RA50$date[match(day, RA50$dateID)]
  cat(sprintf("%s\n", day_date))
  
  day_ymd <- format(day_date, "%y_%m_%d")
  
  pal <- colorNumeric("YlGnBu", all_props)
  
  summarytext <- sprintf("Day: %s<br /><br />Signatures Today: +%s<br /><br />Total Signatures so far: %s",
                         format(day_date, "%d/%m/%y"),
                         prettyNum(sum(RA50_hex_map$number_of_signatures)-total_yesterday, big.mark=",", scientific=FALSE),
                         prettyNum(sum(RA50_hex_map$number_of_signatures), big.mark=",", scientific=FALSE))
  title <- tags$div(
    HTML(sprintf('<p><font size="+1.5">%s</font></p>', summarytext))
  )
  
  leaflet(options=leafletOptions(
    dragging = FALSE, zoomControl = FALSE, tap = FALSE,
    minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75), list(58.25,50.0)),
    attributionControl = FALSE),
    RA50_hex_map
  ) %>%
    addPolygons(
      color = "grey",
      weight=0.75,
      opacity = 0.5,
      fillOpacity = 1,
      fillColor = ~pal(proportionSign)
    ) %>%
    addLegend("topright", pal = pal, values = ~proportionSign,
              title = "Proportion of Signatures",
              opacity = 1)  %>%
    addControl(title, position = "bottomright") %>%
    htmlwidgets::onRender(
      "function(x, y) {
    var myMap = this;
    myMap._container.style['background'] = '#fff';
    }"
    ) %>%
    mapOptions(zoomToLimits = "first") %>%
    saveWidget("tmp.html", selfcontained = F)
  
  file_loc <- sprintf("img/%s.png", day_ymd)
  all_file_locs <- c(all_file_locs, file_loc)
  if (day == tail(days, n=1)) {
    # pause on last day
    all_file_locs <- c(all_file_locs, file_loc, file_loc, file_loc, file_loc)
  }
  webshot("tmp.html", file_loc, vwidth = 625, vheight=800)
  
  file.remove("tmp.html")
  unlink("tmp_files", recursive=TRUE)
  
  total_yesterday <- sum(RA50_hex_map$number_of_signatures)
}

# join into animation

images <- map(all_file_locs, image_read)
images <- image_join(images)
animation <- image_animate(images, fps = 1)
image_write(animation, "animation.gif")


