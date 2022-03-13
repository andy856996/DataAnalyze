# ---------
#  12-11
# ---------
# ---------------------------------------------------------- #

download.file("http://www.jaredlander.com/data/diamonds.db",
              destfile="data/diamonds.db", mode='wb')


# ---------------------------------------------------------- #

diaDBSource <- src_sqlite("data/diamonds.db")
diaDBSource

# ---------------------------------------------------------- #

diaDBSource2 <- DBI::dbConnect(RSQLite::SQLite(), "data/diamonds.db")
diaDBSource2

# ---------------------------------------------------------- #

diaTab <- tbl(diaDBSource, "diamonds")
diaTab

# ---------------------------------------------------------- #

diaTab %>% group_by(cut) %>% dplyr::summarize(Price=mean(price))

diaTab %>% group_by(cut) %>%
            dplyr::summarize(Price=mean(price), Carat=mean(Carat))

# ---------------------------------------------------------- #
