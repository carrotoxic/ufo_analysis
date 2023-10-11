library(tidyverse)
library(lubridate)
library(scales)

#データ読み込み
dfu = read_csv("nuforc_reports.csv")
dfu

#年ごとのカウント
tab <- table(cut(dfu$date_time, 'year'))
dfu_y = data.frame(Date=as.Date(names(tab)),
                   Frequency=as.vector(tab)) %>%
  as.tibble() %>%
  filter(Date>="2000/01/01")
dfu_y

ggplot(dfu_y, aes(Date, Frequency)) +  
  geom_line(color="#291b68", size=1.3) +
  geom_point(color="#d9f812", size=2.5) +
  theme(panel.grid.major = element_line(color = "#888888"),  panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#717171", color = "white", linetype = 1, size = 1),
        plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2")) +
  scale_x_date(date_breaks = "2 year",date_labels = "%Y", name = "日付(2000~2021)") +
  scale_y_continuous(name = "目撃数") +
  ggtitle("UFO目撃数(2000~2021年)/年別")

#月ごとのカウント
tab2 <- table(cut(dfu$date_time, 'month'))
dfu_m = data.frame(Date=as.Date(names(tab2)),
                   Frequency=as.vector(tab2)) %>%
  as.tibble() %>%
  filter(Date>="2019/01/01")
dfu_m

ggplot(dfu_m, aes(Date, Frequency)) +  
  geom_line(color="#291b68", size=1.3) +
  geom_point(color="#d9f812", size=2.5) +
  theme( panel.grid.major = element_line(color = "#888888"),  panel.grid.minor = element_line(color = "#888888"),
         panel.background = element_rect(fill = "#717171", color = "white", linetype = 1, size = 1),
         plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%B", minor_breaks = "1 month", name = "日付(2019~2021)", limits = as.Date(c("2019/01/01","2021/12/01"))) +
  scale_y_continuous(name = "目撃数")+
  ggtitle("UFO目撃数(2019~2021年)/月別")

#日ごとのカウント
tab3 <- table(cut(dfu$date_time, 'day'))
dfu_d = data.frame(Date=as.Date(names(tab3)),
                   Frequency=as.vector(tab3)) %>%
  as.tibble() %>%
  filter(Date>="2020/04/01",Date<"2020/05/01")
dfu_d

ggplot(dfu_d, aes(Date, Frequency)) +  
  geom_line(color="#291b68", size=1.3) +
  geom_point(color="#d9f812", size=2.5) +
  theme( panel.grid.major = element_line(color = "#888888"),  panel.grid.minor = element_line(color = "#888888"),
         panel.background = element_rect(fill= "#717171", color = "white", linetype = 1, size = 1),
         plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2")) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d", minor_breaks = "1 day", name = "日付(2020/4)") +
  scale_y_continuous(name = "目撃数") +
  ggtitle("UFO目撃数(2020年4月)/日別")

#時間ごとのカウント
tab4 <- table(cut(dfu$date_time, 'hour'))
tab4
dfu_h = data.frame(Date=as_datetime(names(tab4)),
                   Frequency=as.vector(tab4)) %>%
  as_tibble() %>%
  filter(Date>="2020-04-17 00:00:00", Date<="2020-04-17 24:00:00")
dfu_h

ggplot(dfu_h, aes(Date, Frequency)) +  
  geom_line(color="#291b68", size=1.3) +
  geom_point(color="#d9f812", size=2.5) +
  theme( panel.grid.major = element_line(color = "#888888"),  panel.grid.minor = element_line(color = "#888888"),
         panel.background = element_rect(fill = "#717171", color = "white", linetype = 1, size = 1),
         plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2")) +
  scale_x_datetime(breaks = date_breaks("hour"), labels = date_format("%H")) +
  scale_y_continuous(name = "目撃数") +
  ggtitle("UFO目撃数(2020年4月16~17日)/時間別")

#形態素解析
library(wordcloud)
library(tidytext)

dfu.t = dfu %>%
  select(date_time,text) %>%
  mutate(date_time = as.Date(date_time)) %>%
  filter(date_time>="2020/01/01", date_time<"2021/01/01")
dfu.t
write.csv(dfu.t, "ufotext.csv")
text <- scan("ufo.txt", what = character(), sep = "\n", blank.lines.skip = F)
ufo_posts <- tibble(post_no = 1:length(text), text = text)
ufo_words <- ufo_posts %>% unnest_tokens(output = "word", input = "text", token = "words")
rank = ufo_words %>% count(word)
ja_stop_words <- tibble(word = c("the","and","to","it","i","from","as","o?","were","be","with"))
mycol = brewer.pal(8, "BuPu")
ufo_words %>% count(word) %>% anti_join(ja_stop_words) %>% filter(n>=3000, n<=8000) %>% with(wordcloud(word, n, mini.freq = 10,max.words = 100,random.color = FALSE,colors = mycol, family="mono"))

#形状棒グラフ
tab5 <- table(dfu$shape)
tab5
dfu_s = data.frame((Shape=names(tab5)),
                   Frequency=as.vector(tab5)) %>%
  as_tibble() %>%
  filter(Frequency>=5000)
dfu_s

bar1 <- dfu_s %>%
  ggplot(aes(X.Shape...names.tab5.., Frequency)) +
  geom_bar(stat = "identity", fill="#47DB3F") +
  ggtitle("UFOの形状ランキング") +
  theme( panel.grid.major = element_line(color = "#e4e4e4"),  panel.grid.minor = element_line(color = "#e4e4e4"),
         panel.background = element_rect(fill = "white", color = "black", linetype = 1, size = 1),
         plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#5957a8")) +
  xlab("形状") +
  ylab("目撃数")
bar1

tab6 = table(dfu$state)
dfu_state = data.frame((Shape=names(tab6)),
                       Frequency=as.vector(tab6))

#UFO目撃マップ
library(leaflet)

dfupoint = dfu %>%
  filter(date_time>="2020-04-17 00:00:00", date_time<="2020-04-17 24:00:00") %>%
  select(date_time, city, lat=city_latitude, lng=city_longitude)
dfupoint  

ufoicon = makeIcon(iconUrl = "ufo2.png",
                   iconWidth =50, iconHeight = 50)
ufomap <- dfupoint %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng=~lng, lat=~lat, popup=~city, icon=ufoicon) %>%
  addProviderTiles(providers$Stamen.Toner)
ufomap
htmlwidgets::saveWidget(ufomap,"ufomap.html")

#層別棒グラフ

tab7 = table(dfu$state, dfu$shape)
dfu_state2 = as.data.frame(tab7)

dfu_state2 = dfu_state2 %>%
  filter(Var1 %in% c("TX","CA","FL","WA","NY"), Var2 %in% c("light","circle","triangle","fireball","sphere"))

library(RColorBrewer)
display.brewer.all()
ggplot(dfu_state2, aes(x = Var1, y = Freq))+
  geom_bar(stat="identity",position="stack", aes(fill=Var2), alpha=0.6)+
  ggtitle("目撃数棒グラフ(形状による層別)")+
  xlab("州")+
  ylab("目撃数")+
  labs(fill="形状")+
  scale_fill_brewer(palette="Set1")+
  theme(panel.grid.major = element_line(color = "#e4e4e4"),  panel.grid.minor = element_line(color = "#e4e4e4"),
        panel.background = element_rect(fill = "white", color = "black", linetype = 1, size = 1),
        plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#5957a8"))+
  theme_gray()  

#コロプレス図

library(sf)

sfile <- "gadm41_USA_shp/gadm41_USA_1.shp" 
ufo_sp <- st_read(sfile)
head(ufo_sp)

ufo_sp %>%
  select(geometry) %>%
  plot()

tab8 <- table(dfu$state)
tab8
dfu_state = data.frame(State=(names(tab8)),
                       Frequency=as.vector(tab8)) %>%
  as_tibble()
dfu_state

ufo_sp$ISO_1
dfu_state$State

ufo_sp = ufo_sp %>%
  mutate(ISO_1 = as.factor(gsub("US-","",ufo_sp$ISO_1)))
setequal(ufo_sp$ISO_1, dfu_state$State)

usufo <- inner_join(ufo_sp, dfu_state, by = c("ISO_1"="State"))
usufo
usufo %>%
  arrange(Frequency)
usufo = usufo %>%
  filter(NAME_1 != "Alaska") %>%
  filter(NAME_1 != "Hawaii")
usufo$NAME_1

ggplot(usufo, aes(fill=Frequency))+
  geom_sf()+
  scale_fill_continuous(low="#e6f296",high = "red")+
  ggtitle("UFO目撃数コロプレス図(全年代)")+
  labs(fill="目撃数")+
  theme_bw()+
  theme(plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2"))

dfu2010 = dfu %>%
  filter(date_time>="2010/01/01")
tab10 = table(dfu2010$state)
dfu_y2 = data.frame(State=names(tab10),
                   Frequency=as.vector(tab10)) %>%
  as_tibble()
dfu_y2

usufo2 = inner_join(ufo_sp, dfu_y2, by = c("ISO_1"="State")) %>%
  filter(NAME_1 != "Alaska") %>%
  filter(NAME_1 != "Hawaii")
usufo2$NAME_1

ggplot(usufo2, aes(fill=Frequency))+
  geom_sf()+
  scale_fill_continuous(low="#e6f296",high = "red")+
  ggtitle("UFO目撃数コロプレス図(2010以降)")+
  labs(fill="目撃数")+
  theme_bw()+
  theme(plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2"))

dfu2020 = dfu %>%
  filter(date_time>="2020/01/01")
tab11 = table(dfu2020$state)
dfu_y3 = data.frame(State=names(tab11),
                    Frequency=as.vector(tab11)) %>%
  as_tibble()
dfu_y3

usufo3 = inner_join(ufo_sp, dfu_y3, by = c("ISO_1"="State")) %>%
  filter(NAME_1 != "Alaska") %>%
  filter(NAME_1 != "Hawaii")
usufo3$NAME_1

ggplot(usufo3, aes(fill=Frequency))+
  geom_sf()+
  scale_fill_continuous(low="#e6f296",high = "red")+
  ggtitle("UFO目撃数コロプレス図(2020以降)")+
  labs(fill="目撃数")+
  theme_bw()+
  theme(plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2"))

tablas = table(dfu$state, cut(dfu$date_time, 'year'))
dfu_bar = data.frame(Date=as.Date(colnames(tablas)),
                     State=rownames(tablas),
                     Frequency=as.vector(tablas))
dfu_bar = dfu_bar %>%
  filter(State %in% c("TX","CA","FL","WA","NY", "NY")) %>%
  filter(Date>="2010/01/01")

ggplot(dfu_bar, aes(Date, Frequency, colour=State)) +
  geom_line(size=2) +
  ggtitle("PCR Positive daily (Hokkaido/Tokyo/Okinawa 2022)") +
  theme( panel.grid.major = element_line(color = "#888888"),  panel.grid.minor = element_line(color = "#888888"),
         panel.background = element_rect(fill = "#717171", color = "white", linetype = 1, size = 1),
         plot.title=element_text(size=rel(2), lineheight=.9, family="mono", face="bold", colour="#9A2BE2")) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y", name = "年") +
  scale_y_continuous(name = "目撃数") +
  ggtitle("UFO目撃数推移(2010年以降)/州別")+
  guides(color = guide_legend(title = "州"))
