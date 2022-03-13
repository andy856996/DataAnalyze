# ---------
#  14-3
# ---------
# ---------
#  14-3-1
# ---------
# ---------------------------------------------------------- #

head(Aid_00s)

# ---------------------------------------------------------- #

library(reshape2)
melt00 <- melt(Aid_00s, id.vars=c("Country.Name", "Program.Name"),
               variable.name="Year", value.name="Dollars")
tail(melt00, 10)

# ---------------------------------------------------------- #

library(scales)
# 把Year直排名稱的"FY"消除掉,並把它轉換成numeric
melt00$Year <- as.numeric(str_sub(melt00$Year, start=3, 6))

# 依據年份和援助計畫進行分群
meltAgg <- aggregate(Dollars ~ Program.Name + Year, data=melt00,
                     sum, na.rm=TRUE)

# 只保留援助計畫名稱的首十個字元
# 這樣名字才能恰到好處地被放入圖中
meltAgg$Program.Name <- str_sub(meltAgg$Program.Name, start=1,
                                end=10)

ggplot(meltAgg, aes(x=Year, y=Dollars)) +
       geom_line(aes(group=Program.Name)) +
       facet_wrap(~ Program.Name) +
       scale_x_continuous(breaks=seq(from=2000, to=2009, by=2)) +
       theme(axis.text.x=element_text(angle=90, vjust=1, hjust=0)) +
       scale_y_continuous(labels=multiple_format(extra=dollar,
                                                 multiple="B"))

# ---------------------------------------------------------- #
# ---------
#  14-3-2
# ---------
# ---------------------------------------------------------- #

cast00 <- dcast(melt00, Country.Name + Program.Name ~ Year,
                value.var = "Dollars")
head(cast00)

# ---------------------------------------------------------- #