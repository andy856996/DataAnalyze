# ---------
#  28-5
# ---------
# ---------
#  28-5-1
# ---------
# ---------------------------------------------------------- #

knitr::kable(head(iris), caption='Tabular data printed using kable.')

# ---------------------------------------------------------- #

library(DT)
data(diamonds, package='ggplot2')
datatable(head(diamonds, 100))

# ---------------------------------------------------------- #

datatable(head(diamonds, 100),
            rownames=FALSE, extensions='Scroller', filter='top',
            options = list(
              dom = "tiS", scrollX=TRUE,
              scrollY = 400,
              scrollCollapse = TRUE
              ))

# ---------------------------------------------------------- #

datatable(head(diamonds, 100),
            rownames=FALSE, extensions='Scroller', filter='top',
            options = list(
              dom = "tiS", scrollX=TRUE,
              scrollY = 400,
              scrollCollapse = TRUE
              )) %>%
              formatCurrency('price', digits=0) %>%
                  formatStyle(columns='cut', valueColumns='cut', target='row',
                              backgroundColor=styleEqual(levels=c('Good', 'Ideal'),
                                                          values=c('red', 'green')
                                                        )
                              )


# ---------------------------------------------------------- #
# ---------
#  28-5-2
# ---------
# ---------------------------------------------------------- #

library(jsonlite)
pizza <- fromJSON('http://www.jaredlander.com/data/PizzaFavorites.json')
pizza

class(pizza$Details)
class(pizza$Details[[1]])
dim(pizza$Details[[1]])


# ---------------------------------------------------------- #

library(dplyr)
library(tidyr)
pizza <- pizza %>%
  # 解開巢狀data.frame
  unnest() %>%
  # 重新命名Address直行為Street
  rename(Street=Address) %>%
  # 建立一個新的直行以儲存整個地址
  unite(col=Address,
          Street, City, State, Zip,
          sep=', ', remove=FALSE)

pizza


# ---------------------------------------------------------- #

getCoords <- function(address)
  {
      RDSTK::street2coordinates(address) %>%
      dplyr::select_('latitude', 'longitude')
  }

# ---------------------------------------------------------- #

library(dplyr)
library(purrr)
pizza <- bind_cols(pizza, pizza$Address %>% map_df(getCoords))
pizza


# ---------------------------------------------------------- #

library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=~longitude, lat=~latitude,
               popup=~sprintf('%s<br/>%s', Name, Street),
               data=pizza
               )

# ---------------------------------------------------------- #
# ---------
#  28-5-3
# ---------
# ---------------------------------------------------------- #

library(WDI)
gdp <- WDI(country=c("US", "CA", "SG", "IL"),
             indicator=c("NY.GDP.PCAP.CD"),
             start=1970, end=2011)

# 對資料中的變數命名
names(gdp) <- c("iso2c", "Country", "PerCapGDP", "Year")

# ---------------------------------------------------------- #

head(gdp, 15)
gdpWide <- gdp %>%
  dplyr::select(Country, Year, PerCapGDP) %>%
  tidyr::spread(key=Country, value=PerCapGDP)


head(gdpWide)


# ---------------------------------------------------------- #

library(dygraphs)
dygraph(gdpWide, main='Yearly Per Capita GDP',
          xlab='Year', ylab='Per Capita GDP') %>%
  dyOptions(drawPoints = TRUE, pointSize = 1) %>%
  dyLegend(width=400)

# ---------------------------------------------------------- #

dygraph(gdpWide, main='Yearly Per Capita GDP',
          xlab='Year', ylab='Per Capita GDP') %>%
  dyOptions(drawPoints = TRUE, pointSize = 1) %>%
  dyLegend(width=400) %>%
  dyRangeSelector(dateWindow=c("1990", "2000"))

# ---------------------------------------------------------- #
# ---------
#  28-5-4
# ---------
# ---------------------------------------------------------- #

library(readr)
flights <- read_tsv('http://www.jaredlander.com/data/Flights_Jan_2.tsv')

# ---------------------------------------------------------- #

flights

# ---------------------------------------------------------- #

airports <- flights %>%
  count(From_Lat, From_Long) %>%
  arrange(desc(n))

airports

# ---------------------------------------------------------- #

earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"

# ---------------------------------------------------------- #

library(threejs)
globejs(img=earth, lat=airports$From_Lat, long=airports$From_Long,
          value=airports$n*5, color='red',
          arcs=flights %>%
            dplyr::select(From_Lat, From_Long, To_Lat, To_Long),
            arcsHeight=.4, arcsLwd=4, arcsColor="#3e4ca2", arcsOpacity=.85,
            atmosphere=TRUE, fov=30, rotationlat=.5, rotationlong=-.05)

# ---------------------------------------------------------- #
# ---------
#  28-5-5
# ---------
# ---------------------------------------------------------- #

library(d3heatmap)
data(economics, package='ggplot2')
econCor <- economics %>% select_if(is.numeric) %>% cor
d3heatmap(econCor, xaxis_font_size='12pt', yaxis_font_size='12pt',
            width=600, height=600)

# ---------------------------------------------------------- #