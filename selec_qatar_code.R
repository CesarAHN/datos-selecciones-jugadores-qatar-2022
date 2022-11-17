library(rvest)
library(dplyr)
library(naniar)
#devtools::install_github("CesarAHN/datametria")
library(datametria)
library(gt)
#devtools::install_github("jthomasmock/gtExtras")
library(gtExtras)
library(ggplot2)
library(viridis)

options(scipen = 999)

rr<-data.frame(pais=c("QATAR","ECUADOR","SENEGAL","PAISES BAJOS","INGLATERRA","IR?N","EEUU","GALES",
                      "ARGENTINA","ARABIA SAUD?","M?XICO","POLONIA","FRANCIA","AUSTRALIA","DINAMARCA","T?NEZ",
                      "ESPA?A","COSTA RICA","ALEMANIA","JAP?N","B?LGICA","CANAD?","MARRUECOS","CROACIA",
                      "BRASIL","SERBIA","SUIZA","CAMER?N","PORTUGAL","GHANA","URUGUAY","KOREA DEL SUR"),
               grupo=rep(LETTERS[1:8], each=4),
               pp=c("https://www.transfermarkt.es/catar/kader/verein/14162/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/ecuador/kader/verein/5750/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/senegal/kader/verein/3499/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/paises-bajos/kader/verein/3379/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/inglaterra/kader/verein/3299/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/iran/kader/verein/3582/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/estados-unidos/kader/verein/3505/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/gales/kader/verein/3864/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/argentina/kader/verein/3437/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/arabia-saudita/kader/verein/3807/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/mexico/kader/verein/6303/saison_id/2021/plus/1", 
                    "https://www.transfermarkt.es/polonia/kader/verein/3442/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/francia/kader/verein/3377/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/australia/kader/verein/3433/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/dinamarca/kader/verein/3436/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/tunez/kader/verein/3670/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/espana/kader/verein/3375/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/costa-rica/kader/verein/8497/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/alemania/kader/verein/3262/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/japon/kader/verein/3435/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/belgica/kader/verein/3382/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/canada/kader/verein/3510/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/marruecos/kader/verein/3575/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/croacia/kader/verein/3556/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/brasil/kader/verein/3439/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/serbia/kader/verein/3438/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/suiza/kader/verein/3384/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/camerun/kader/verein/3434/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/portugal/kader/verein/3300/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/ghana/kader/verein/3441/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/uruguay/kader/verein/3449/saison_id/2021/plus/1",
                    "https://www.transfermarkt.es/corea-del-sur/kader/verein/3589/saison_id/2021/plus/1"),
               bandera=c("https://cdn-icons-png.flaticon.com/512/323/323297.png", # Qatar.
                         "https://cdn-icons-png.flaticon.com/512/4854/4854969.png", # Ecuador.
                         "https://cdn-icons-png.flaticon.com/512/4855/4855849.png", #Senegal.
                         "https://cdn-icons-png.flaticon.com/512/5315/5315534.png", # Paises bajos.
                         "https://cdn-icons-png.flaticon.com/512/197/197485.png", # Inglaterra.
                         "https://cdn-icons-png.flaticon.com/512/6211/6211416.png", # Ir?n.
                         "https://cdn-icons-png.flaticon.com/512/197/197484.png", # Estados unidos.
                         "https://cdn-icons-png.flaticon.com/512/8603/8603711.png", # Gales.
                         "https://cdn-icons-png.flaticon.com/512/197/197573.png", # Argentina
                         "https://cdn-icons-png.flaticon.com/512/5111/5111777.png", # Arabia saudi.
                         "https://cdn-icons-png.flaticon.com/512/5372/5372848.png", # M?xico.
                         "https://cdn-icons-png.flaticon.com/512/4628/4628690.png", # Polonia.
                         "https://cdn-icons-png.flaticon.com/512/5315/5315693.png", # Francia.
                         "https://cdn-icons-png.flaticon.com/512/323/323367.png", # Australia.
                         "https://cdn-icons-png.flaticon.com/512/4854/4854950.png", # Dinamarca.
                         "https://cdn-icons-png.flaticon.com/512/197/197624.png", # T?nez.
                         "https://cdn-icons-png.flaticon.com/512/4855/4855742.png", # Espa?a.
                         "https://cdn-icons-png.flaticon.com/512/4854/4854928.png", # Costa Rica.
                         "https://cdn-icons-png.flaticon.com/512/197/197571.png", # Alemania.
                         "https://cdn-icons-png.flaticon.com/512/197/197604.png", # Jap?n.
                         "https://cdn-icons-png.flaticon.com/512/197/197583.png", # B?lgica.
                         "https://cdn-icons-png.flaticon.com/512/197/197430.png", # Canad?.
                         "https://cdn-icons-png.flaticon.com/512/197/197551.png", # Marruecos.
                         "https://cdn-icons-png.flaticon.com/512/197/197503.png", # Croacia.
                         "https://cdn-icons-png.flaticon.com/512/3909/3909370.png", # Brasil.
                         "https://cdn-icons-png.flaticon.com/512/197/197602.png", # Serbia.
                         "https://cdn-icons-png.flaticon.com/512/197/197540.png", # Suiza.
                         "https://cdn-icons-png.flaticon.com/512/7826/7826398.png", # Camer?n.
                         "https://cdn-icons-png.flaticon.com/512/3909/3909361.png", # Portugal.
                         "https://cdn-icons-png.flaticon.com/512/5111/5111821.png", # Ghana.
                         "https://cdn-icons-png.flaticon.com/512/197/197599.png", # Uruguay.
                         "https://cdn-icons-png.flaticon.com/512/197/197582.png" # Corea del sur.
                         ))

for (i in 1:nrow(rr)) {
  pw<-read_html(rr$pp[i])
  bb<-pw %>% html_elements("table.items") %>% html_table() %>% as.data.frame()
  cc<-pw %>% html_elements("td.zentriert > a") %>% html_attr("title") %>% as.data.frame()
  bb<-bb %>% filter(!is.na(X.) & X.!="") %>% select(4:ncol(bb)) %>% cbind(cc)
  bb<-bb %>% select(miss_var_summary(bb) %>% filter(pct_miss!=100) %>% select(variable) %>% pull())
  names(bb)<-c("JUGADOR","PUESTO","NACIMIENTO","TALLA","PIE","P_JUGADOS","GOLES","DEBUT","VALOR","CLUB")
  bb$PAIS<-rr$pais[i]
  bb$GRUPO<-rr$grupo[i]
  bb$BANDERA<-rr$bandera[i]
  assign(paste0("a",i),bb)
  rm(bb,cc)
}

df<-do.call(rbind, lapply(ls(pattern = "^a"), get))

#-----------------------------------------------------------------------------------------------------------------------

dtb<-sapply(df, limpiecito) %>% as_tibble()

dtb<-dtb %>% mutate(FECHA_NACIMIENTO=gsub("(.*)( .*)","\\1",NACIMIENTO),
              EDAD=as.numeric(gsub("(.*\\()(.*)(\\))","\\2",NACIMIENTO)),
              TALLA=gsub(",",".",TALLA),
              TALLA=as.numeric(gsub("M|m","",TALLA)),
              PIE=ifelse(PIE=="","NO DEFINIDO",PIE),
              P_JUGADOS=as.numeric(gsub("-","0",P_JUGADOS)),
              GOLES=as.numeric(gsub("-","0",GOLES)),
              DEBUT=as.Date(DEBUT, tryFormats = c("%d-%m-%Y", "%d/%m/%Y")),
              VALOR=gsub(",",".",VALOR),
              VALOR2=as.numeric(gsub("(.* )(M.*)","\\1",VALOR)), # Hay jugadores que no tienen valor. 
              VALOR_TRFM=case_when(grepl("MILL",VALOR)~VALOR2*10e5,
                               grepl("MIL",VALOR)~VALOR2*10e2,
                               TRUE~NA_real_),
              PUESTO=case_when(grepl("ARQUERO|PORTERO",PUESTO)~"PORTEROS",
                               grepl("DEFENSA|LATERAL",PUESTO)~"DEFENSAS",
                               grepl("PIVOTE|MEDI|INTERIOR",PUESTO)~"MEDIOCAMPISTAS",
                               grepl("EXTREMO|DELANTERO",PUESTO)~"DELANTEROS")) %>% select(-NACIMIENTO,-VALOR,-VALOR2)




ligas<-data.frame(liga=c("PREMIER LEAGUE","BUNDESLIGA","SERIE A","LIGUE 1","LIGA DE PORTUGAL","LIGA 1","EREDIVISIE",
                         "JUPILER PRO LEAGUE","PREMIER LIGA","SUPERLIGAEN","SUPER LEAGUE","EKSTRAKLASA","SUPER LIGA SRBIJE",
                         "CHAMPIONSHIP","A LEAGUE MEN","SAUDI PRO LEAGUE","STARS LEAGUE","PREMIER LEAGUE",
                         "BOTOLA PRO INWI","LIGUE PROFESSIONNELLE","K LEAGUE 1","J1 LEAGUE"),
                  pais_liga=c("INGLATERRA","ALEMANIA","ITALIA","FRANCIA","PORTUGAL","ESPA?A","PAISES BAJOS",
                              "BELGICA","RUSIA","DINAMARCA","SUIZA","POLONIA","SERBIA","INGLATERRA","AUSTRALIA",
                              "ARABIA SAUDITA","QATAR","GHANA","MARRUECOS","TUNEZ", "KOREA DEL SUR","JAPON"),
                  url=c("https://www.transfermarkt.es/premier-league/startseite/wettbewerb/GB1",
                        "https://www.transfermarkt.es/bundesliga/startseite/wettbewerb/L1",
                        "https://www.transfermarkt.es/serie-a/startseite/wettbewerb/IT1",
                        "https://www.transfermarkt.es/ligue-1/startseite/wettbewerb/FR1",
                        "https://www.transfermarkt.es/liga-nos/startseite/wettbewerb/PO1",
                        "https://www.transfermarkt.es/laliga/startseite/wettbewerb/ES1",
                        "https://www.transfermarkt.es/eredivisie/startseite/wettbewerb/NL1",
                        "https://www.transfermarkt.es/jupiler-pro-league/startseite/wettbewerb/BE1",
                        "https://www.transfermarkt.es/premier-liga/startseite/wettbewerb/RU1",
                        "https://www.transfermarkt.es/superligaen/startseite/wettbewerb/DK1",
                        "https://www.transfermarkt.es/super-league/startseite/wettbewerb/C1",
                        "https://www.transfermarkt.es/pko-ekstraklasa/startseite/wettbewerb/PL1",
                        "https://www.transfermarkt.es/super-liga-srbije/startseite/wettbewerb/SER1",
                        "https://www.transfermarkt.es/championship/startseite/wettbewerb/GB2",
                        "https://www.transfermarkt.es/a-league-men/startseite/wettbewerb/AUS1",
                        "https://www.transfermarkt.es/saudi-pro-league/startseite/wettbewerb/SA1",
                        "https://www.transfermarkt.es/qatar-stars-league/startseite/wettbewerb/QSL",
                        "https://www.transfermarkt.es/ghana-premier-league/startseite/wettbewerb/GHPL",
                        "https://www.transfermarkt.es/botola-pro-inwi/startseite/wettbewerb/MAR1",
                        "https://www.transfermarkt.es/ligue-professionnelle-1/startseite/wettbewerb/TUN1",
                        "https://www.transfermarkt.es/k-league-1/startseite/wettbewerb/RSK1",
                        "https://www.transfermarkt.es/j1-league/startseite/wettbewerb/JAP1"),
                  band=c("https://cdn-icons-png.flaticon.com/512/330/330469.png",
                         "https://cdn-icons-png.flaticon.com/512/555/555613.png",
                         "https://cdn-icons-png.flaticon.com/512/3373/3373278.png",
                         "https://cdn-icons-png.flaticon.com/512/4060/4060248.png",
                         "https://cdn-icons-png.flaticon.com/512/206/206628.png",
                         "https://cdn-icons-png.flaticon.com/512/206/206724.png",
                         "https://cdn-icons-png.flaticon.com/512/3371/3371849.png",
                         "https://cdn-icons-png.flaticon.com/512/555/555625.png",
                         "https://cdn-icons-png.flaticon.com/512/555/555451.png",
                         "https://cdn-icons-png.flaticon.com/512/3371/3371998.png",
                         "https://cdn-icons-png.flaticon.com/512/3373/3373309.png",
                         "https://cdn-icons-png.flaticon.com/512/2151/2151365.png",
                         "https://cdn-icons-png.flaticon.com/512/206/206781.png",
                         "https://cdn-icons-png.flaticon.com/512/330/330469.png",
                         "https://cdn-icons-png.flaticon.com/512/206/206618.png",
                         "https://cdn-icons-png.flaticon.com/512/6211/6211614.png",
                         "https://cdn-icons-png.flaticon.com/512/555/555660.png",
                         "https://cdn-icons-png.flaticon.com/512/555/555424.png",
                         "https://cdn-icons-png.flaticon.com/512/2151/2151353.png",
                         "https://cdn-icons-png.flaticon.com/512/555/555666.png",
                         "https://cdn-icons-png.flaticon.com/512/330/330591.png",
                         "https://cdn-icons-png.flaticon.com/512/940/940177.png"))

for (i in 1:nrow(ligas)) {
  pw<-read_html(ligas$url[i])
  aa<-pw %>% html_elements("td.hauptlink.no-border-links") %>% html_text2()
  bb<-data.frame(CLUB=aa,PAIS_LIGA=ligas$pais_liga[i],LIGA=ligas$liga[i], BANDERA2=ligas$band[i])
  assign(paste0("lig",i),bb)
  rm(aa,bb,pw)
}

ligas<-do.call(rbind, lapply(ls(pattern = "^lig\\d"), get))

ligas<-sapply(ligas,limpiecito) %>% as_tibble() %>% distinct(.keep_all = T)

#-----------------------------------------------------------------------------------------------------------------------

dtb<-dtb %>% left_join(ligas) %>% mutate(PAIS_LIGA=ifelse(is.na(PAIS_LIGA),"OTROS PAISES",PAIS_LIGA),
                                         LIGA=ifelse(is.na(LIGA),"OTRAS LIGAS",LIGA))

dtb$BANDERA<-tolower(dtb$BANDERA)
dtb$BANDERA2<-tolower(dtb$BANDERA2)

saveRDS(dtb, "dtb_qatar2022.rds")
# dtb<-readRDS("dtb_qatar2022.rds")

#-----------------------------------------------------------------------------------------------------------------------

#--------------------
# Tablas y gr?ficos.
#--------------------

#----------------------------------------------------------------------------------------------------------
# Distrbuci?n de puestos. 
dtb %>% count(PUESTO, sort = T) %>% mutate(p=n/sum(n),ymax=cumsum(p),ymin=c(0, head(ymax, n=-1)),
                                           lp=(ymax + ymin)/2, etiq=paste0(round(p*100,1),"%")) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=PUESTO)) +
  geom_rect() +
  geom_label(x=3.5, aes(y=lp, label=etiq), size=5, bg=NA, fontface="bold", alpha=.7, color="black") +
  scale_fill_viridis(discrete = T,name="")+
  labs(title = "DISTRIBUCI?N DE JUGADORES\nPOR PUESTOS.")+
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "bottom",legend.title = element_text(face = "bold"))+
  guides(fill=guide_legend(ncol=2))

# Selecciones con mayor cantidad de jugadores por puestos.
dtb %>% group_by(PUESTO,BANDERA, PAIS) %>% count() %>% group_by(PAIS) %>% mutate(p=n/sum(n)) %>% 
  inner_join(dtb %>% group_by(PUESTO, PAIS) %>% count() %>% group_by(PUESTO) %>% summarise(n=max(n))) %>% 
  arrange(PUESTO,-n,-p) %>% rename(N=BANDERA, JUGADORES=n, `%`=p) %>% ungroup() %>% gt() %>% 
  fmt_percent(columns = matches("%")) %>% 
  tab_header(title = "SELECCIONES QUE LLEVAN M?S JUGADORES POR PUESTO.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 10) %>% 
  tab_style(style = list(cell_text(align = "center", size="xx-small")),
            locations = list(cells_column_labels(columns = c(PUESTO,PAIS,JUGADORES,`%`,N)))) %>%
  tab_style(style = list(cell_text(align = "center", size="small")),
            locations = list(cells_title("title"))) %>% 
  tab_style(style = list(cell_text(size="xx-small")),
            locations = list(cells_source_notes())) %>% 
  tab_style(style = list(cell_text(align = "center", size="xx-small")),
            locations = list(cells_body(columns = c(PUESTO,PAIS,JUGADORES,`%`,N))))
#----------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
# Distribuci?n de jugadores por pie.
dtb %>% count(PIE, sort = T) %>% mutate(p=n/sum(n),ymax=cumsum(p),ymin=c(0, head(ymax, n=-1)),
                                            lp=(ymax + ymin)/2, etiq=paste0(round(p*100,1),"%")) %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=PIE)) +
  geom_rect() +
  geom_label_repel(x=3.5, aes(y=lp, label=etiq), size=5, bg=NA, fontface="bold", alpha=.7, color="black") +
  scale_fill_viridis(discrete = T,name="")+
  labs(title = "DISTRIBUCI?N DE JUGADORES\nPOR PIE DE DOMINIO")+
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "bottom",legend.title = element_text(face = "bold"))+
  guides(fill=guide_legend(ncol=2))

# Pa?ses con la mayor cantidad de jugadores izquierdos. 
dtb %>% group_by(BANDERA,PAIS) %>% count(PIE) %>% mutate(p=n/sum(n)) %>% 
  filter(PIE=="IZQUIERDO") %>% arrange(-n) %>% head(4) %>% select(-PIE) %>% 
  rename(N=BANDERA, JUGADORES=n, `%`=p) %>% ungroup() %>% gt() %>% 
  fmt_percent(columns = matches("%")) %>% 
  tab_header(title = "TOP 4 DE SELECCIONES QUE LLEVAN M?S JUGADORES CON DOMINIO DE PIE IZQUIERDO.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,JUGADORES,`%`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,JUGADORES,`%`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

# Pa?ses con la mayor cantidad de jugadores ambidiestros. 
dtb %>% group_by(PAIS) %>% count(PIE) %>% mutate(p=n/sum(n)) %>% filter(PIE=="AMBIDIESTRO") %>% arrange(-n) %>% head(5)
#---------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------
# El pa?s con la talla m?s alta en promedio.
dtb %>% group_by(BANDERA,PAIS) %>% summarise(`TALLA PROMEDIO`=mean(TALLA, na.rm=T)) %>% arrange(-`TALLA PROMEDIO`) %>%
  head(5) %>% mutate(`TALLA PROMEDIO`=paste0(round(`TALLA PROMEDIO`,2)," m")) %>% rename(N=BANDERA) %>% ungroup() %>% gt() %>% 
  tab_header(title = "TOP 5 DE SELECCIONES CON LA TALLA PROMEDIO M?S ALTA.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,`TALLA PROMEDIO`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,`TALLA PROMEDIO`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

# El pa?s con la talla m?s baja en promedio.
dtb %>% group_by(BANDERA,PAIS) %>% summarise(`TALLA PROMEDIO`=mean(TALLA, na.rm=T)) %>% arrange(`TALLA PROMEDIO`) %>%
  head(5) %>% mutate(`TALLA PROMEDIO`=paste0(round(`TALLA PROMEDIO`,2)," m")) %>% rename(N=BANDERA) %>% ungroup() %>% gt() %>% 
  tab_header(title = "TOP 5 DE SELECCIONES CON LA TALLA PROMEDIO M?S BAJA.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,`TALLA PROMEDIO`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,`TALLA PROMEDIO`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")
#---------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------
# El jugador m?s alto. 
dtb %>% arrange(-TALLA) %>% head(4) %>% select(BANDERA,PAIS,JUGADOR,PUESTO,TALLA) %>%
  mutate(TALLA=paste0(round(TALLA,2)," m")) %>% rename(N=BANDERA) %>% gt() %>% 
  tab_header(title = "TOP 4 DE JUGADORES M?S ALTOS.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,JUGADOR,PUESTO,TALLA,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,JUGADOR,PUESTO,TALLA,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

# El jugador m?s bajo 
dtb %>% arrange(TALLA) %>% head(4) %>% select(BANDERA,PAIS,JUGADOR,PUESTO,TALLA) %>%
  mutate(TALLA=paste0(round(TALLA,2)," m")) %>% rename(N=BANDERA) %>% gt() %>% 
  tab_header(title = "TOP 4 DE JUGADORES M?S BAJOS.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,JUGADOR,PUESTO,TALLA,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,JUGADOR,PUESTO,TALLA,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

#----------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------
# El jugador con m?s partidos jugados.
dtb %>% arrange(-P_JUGADOS) %>% head(5) %>% select(BANDERA,JUGADOR,PAIS,P_JUGADOS) %>% 
  rename(N=BANDERA, `PARTIDOS JUGADOS`=P_JUGADOS) %>% gt() %>% 
  tab_header(title = "TOP 5 DE JUGADORES CON M?S PARTIDOS JUGADOS CON SU SELECCI?N") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,JUGADOR,`PARTIDOS JUGADOS`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,JUGADOR,`PARTIDOS JUGADOS`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

# El jugador con menos partidos jugados.
dtb %>% filter(P_JUGADOS==0)
dtb %>% filter(P_JUGADOS==0) %>% count(PAIS, sort = T)
#-----------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------------
# Las selecciones m?s experimentadas - partidos.
dtb %>% group_by(PAIS) %>% summarise(MEDIA_P_JUGADOS=mean(P_JUGADOS, na.rm=T)) %>% arrange(-MEDIA_P_JUGADOS) %>% head(5)

# Las selecciones menos experimentadas - partidos.
dtb %>% group_by(PAIS) %>% summarise(MEDIA_P_JUGADOS=mean(P_JUGADOS, na.rm=T)) %>% arrange(MEDIA_P_JUGADOS) %>% head(5)
#------------------------------------------------------------------------------------------------------------------

# Jugador com m?s goles anotados. 
dtb %>% arrange(-GOLES) %>% head(6) %>% mutate(p=GOLES/P_JUGADOS) %>% select(BANDERA,PAIS,JUGADOR,GOLES,p) %>% 
  rename(N=BANDERA, `%`=p) %>% gt() %>% 
  tab_header(title = "TOP 6 DE JUGADORES CON M?S GOLES ANOTADOS CON SU SELECCI?N.") %>% 
  fmt_percent(columns = matches("%")) %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,JUGADOR,GOLES,`%`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,JUGADOR,GOLES,`%`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT") %>% 
  tab_style(style = list(cell_text(size="xx-small")),
            locations = list(cells_source_notes()))

# Jugadores que a?n no marcan goles con su selecci?n.
dtb %>% filter(GOLES==0) %>% count()

# Jugadores que a?n no marcan goles con su selecci?n, por pa?ses.
dtb %>% filter(GOLES==0) %>% count(PAIS, sort = T)

# Jugadores por posici?n con mayores goles.
dtb %>% group_by(PUESTO) %>% summarise(GOLES=max(GOLES)) %>% filter(PUESTO!="PORTEROS") %>% left_join(dtb)

#--------------------------------------------------------------------------------------------------------------


dtb %>% group_by(BANDERA,PAIS) %>% summarise(`EDAD PROMEDIO`=round(mean(EDAD, na.rm=T),1)) %>% arrange(-`EDAD PROMEDIO`) %>% head(5) %>% 
  rename(N=BANDERA) %>% ungroup() %>% gt() %>% 
  tab_header(title = "TOP 5 DE SELECCIONES CON LA MAYOR EDAD PROMEDIO.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,`EDAD PROMEDIO`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,`EDAD PROMEDIO`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT") %>% 
  tab_style(style = list(cell_text(size="xx-small")),
            locations = list(cells_source_notes()))

dtb %>% group_by(BANDERA,PAIS) %>% summarise(`EDAD PROMEDIO`=round(mean(EDAD, na.rm=T),1)) %>% 
  arrange(`EDAD PROMEDIO`) %>% head(5) %>% rename(N=BANDERA) %>% ungroup() %>% gt() %>% 
  tab_header(title = "TOP 5 DE SELECCIONES CON LA MENOR EDAD PROMEDIO.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,`EDAD PROMEDIO`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,`EDAD PROMEDIO`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
# Los jugadores m?s j?venes y viejos.

# El jugador m?s viejo. 
dtb %>% arrange(-EDAD) %>% head(6) %>% select(BANDERA,PAIS,JUGADOR,EDAD, DEBUT) %>% 
  rename(N=BANDERA) %>% gt() %>% 
  tab_header(title = "TOP 6 JUGADORES CON LA MAYOR EDAD.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,JUGADOR,EDAD,DEBUT)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,JUGADOR,EDAD,DEBUT)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

# El jugador m?s joven.
dtb %>% arrange(EDAD) %>% head(6) %>% select(BANDERA,PAIS,JUGADOR,EDAD, DEBUT) %>% 
  rename(N=BANDERA) %>% gt() %>% 
  tab_header(title = "TOP 6 JUGADORES CON LA MENOR EDAD.") %>% 
  fmt_missing(columns = DEBUT, missing_text = "A?N NO DEBUTA") %>%
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PAIS,JUGADOR,EDAD,DEBUT)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PAIS,JUGADOR,EDAD,DEBUT)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

#------------------------------------------------------------------------------------------------------------

# El jugador m?s antiguo en debutar.
dtb %>% summarise(DEBUT=min(DEBUT, na.rm = T)) %>% left_join(dtb)

# Los jugadores que a?n no debutan.
dtb %>% filter(is.na(DEBUT))
dtb %>% filter(is.na(DEBUT)) %>% count()

# Pais que tiene m?s debutantes.
dtb %>% filter(is.na(DEBUT)) %>% count(PAIS, sort = T)

#-----------------------------------------------------------------------------------------------------

# Clubes de los cuales proceden la mayor?a de jugadores. 
dtb %>% group_by(BANDERA2,LIGA,PAIS_LIGA,CLUB) %>% count(PAIS, sort = T) %>% mutate(JUGADORES=sum(n)) %>%
  group_by(CLUB) %>% mutate(SELECCIONES=1:n(), SELECCIONES=max(SELECCIONES)) %>% arrange(-JUGADORES) %>% 
  ungroup() %>% distinct(CLUB,PAIS_LIGA, .keep_all = T) %>% select(-PAIS, -n) %>% head(8) %>% 
  rename(`PAIS DE LA LIGA`=PAIS_LIGA, N=BANDERA2) %>% gt() %>% 
  tab_header(title = "TOP 5 CLUBES QUE APORTAN M?S M?S JUGADORES.") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(CLUB,`PAIS DE LA LIGA`,LIGA,JUGADORES,SELECCIONES)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(CLUB,`PAIS DE LA LIGA`,LIGA,JUGADORES,SELECCIONES)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

#--------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------

# Ligas de donde provienen m?s jugadores.
dtb %>% count(BANDERA2,PAIS_LIGA,LIGA, sort = T) %>% mutate(`%`=n/sum(n)) %>% 
  filter(LIGA!="OTRAS LIGAS") %>% head(5) %>% 
  rename(N=BANDERA2, JUGADORES=n, `PAIS DE LA LIGA`=PAIS_LIGA) %>% gt() %>% 
  tab_header(title = "TOP 5 LIGAS QUE APORTAN M?S M?S JUGADORES.") %>% 
  fmt_percent(columns = `%`) %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(`PAIS DE LA LIGA`,LIGA,JUGADORES,`%`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(`PAIS DE LA LIGA`,LIGA,JUGADORES,`%`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")
  
# Por puestos. 
dtb %>% group_by(BANDERA2,PUESTO,PAIS_LIGA,LIGA) %>% count() %>% group_by(PUESTO) %>% mutate(`%`=n/sum(n)) %>%
  filter(LIGA!="OTRAS LIGAS") %>% arrange(PUESTO,-n) %>% mutate(cont=1:n()) %>% filter(cont<=2) %>% 
  select(-cont) %>% rename(N=BANDERA2, `PAIS DE LA LIGA`=PAIS_LIGA, JUGADORES=n) %>% ungroup() %>% gt() %>% 
  tab_header(title = "TOP 2 LIGAS QUE APORTAN M?S M?S JUGADORES POR PUESTOS") %>% 
  fmt_percent(columns = `%`) %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(PUESTO,`PAIS DE LA LIGA`,LIGA,JUGADORES,`%`,N)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(PUESTO,`PAIS DE LA LIGA`,LIGA,JUGADORES,`%`,N)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

#------------------------------------------------------------------------------------------------------

# El jugador con mayor valor. 
dtb %>% summarise(VALOR_TRFM=max(VALOR_TRFM, na.rm = T)) %>% left_join(dtb)

# EL top de 7 jugadores m?s valorados. 
dtb %>% arrange(-VALOR_TRFM) %>% head(7) %>% select(BANDERA,JUGADOR, PUESTO, BANDERA2, LIGA, VALOR_TRFM) %>% 
  rename(PAIS=BANDERA, `PAIS LIGA`=BANDERA2,VALOR=VALOR_TRFM) %>% 
  mutate(VALOR=gsub("0{6}$"," Millones",VALOR)) %>% gt() %>%
  tab_header(title = "TOP 7 JUGADORES M?S VALORADOS, SEG?N TRANSFERMARKT") %>% 
  gt_theme_538() %>% gt_img_rows(columns = PAIS, height = 20) %>% 
  gt_img_rows(columns = `PAIS LIGA`, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(JUGADOR, PUESTO, `PAIS LIGA`, LIGA, VALOR,PAIS)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(JUGADOR, PUESTO, `PAIS LIGA`, LIGA, VALOR,PAIS)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

# Jugadores por posici?n con mayor valor.
dtb %>% group_by(PUESTO) %>% summarise(VALOR_TRFM=max(VALOR_TRFM, na.rm = T)) %>% left_join(dtb) %>% 
  select(PUESTO,JUGADOR,BANDERA,LIGA,BANDERA2, VALOR_TRFM) %>% 
  rename(VALOR=VALOR_TRFM,`PAIS LIGA`=BANDERA2, PAIS=BANDERA) %>% mutate(VALOR=gsub("0{6}$","M",VALOR)) %>% gt() %>%
  tab_header(title = "JUGADOR M?S VALORADO POR PUESTO, SEG?N TRANSFERMARKT") %>% 
  gt_theme_538() %>% gt_img_rows(columns = PAIS, height = 20) %>% 
  gt_img_rows(columns = `PAIS LIGA`, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(JUGADOR, PUESTO, `PAIS LIGA`, LIGA, VALOR,PAIS)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(JUGADOR, PUESTO, `PAIS LIGA`, LIGA, VALOR,PAIS)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")

# Los pa?ses con m?s valor. 
dtb %>% group_by(BANDERA,PAIS) %>% summarise(`VALOR TOTAL`=sum(VALOR_TRFM, na.rm=T)) %>%
  arrange(-`VALOR TOTAL`) %>% head(5) %>% rename(N=BANDERA) %>% ungroup() %>% 
  mutate(`VALOR TOTAL`=paste0(`VALOR TOTAL`/1000000," Millones")) %>% gt() %>% 
  tab_header(title = "TOP 5 DE SELECCIONES CON MAYOR VALOR, SEG?N TRANSFERMARKT") %>% 
  gt_theme_538() %>% gt_img_rows(columns = N, height = 20) %>% 
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_column_labels(columns = c(N, PAIS, `VALOR TOTAL`)))) %>%
  tab_style(style = list(cell_text(align = "center")),
            locations = list(cells_body(columns = c(N, PAIS, `VALOR TOTAL`)))) %>% 
  tab_source_note("ELABORACI?N: https://github.com/CesarAHN") %>% 
  tab_source_note("FUENTE: TRANSFERMARKT")
  

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------
