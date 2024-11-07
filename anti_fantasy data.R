library(remotes)
library(rlang)
library(ffscrapr)
# library(textreadr)
library(tidyverse)
library(readr)
library(janitor)
library(xml2)
library(stringr)
library(DT)
library(berryFunctions)

# url <- "https://www.ourlads.com/nfldepthcharts/depthchartpos/"
# players <- c()
# positions <- c('wr','rb','qb','te')
# 
# for (i in 1:length(positions)){
#   popParse <- xml2::read_html(paste0(url,positions[i]))
#   popNodes <- rvest::html_nodes(popParse,'table')
#   pop <- rvest::html_table(popNodes, header = TRUE, fill = TRUE)[[1]]
#   players <- append(players,pop$`Player 1`)
# }
# 
# 
# new <- map(strsplit(players, "\\s.\\/|\\s\\d|\\s..\\d|\\s..\\/"), ~.x[1]) |> unlist() |> unique()
# temp <- sub("(.*),\\s(.*)","\\2 \\1", new)
# eligible <- tibble(player_name=str_to_title(sub("(.*)\\s(.*)\\s(.*)","\\1 \\2", temp)))
# eligible$player_name[eligible$player_name=="Equanimeous St."] <- "Equanimeous St. Brown"






################### get eligibility / projections #################################
week <- as.numeric(strftime(Sys.Date(), format = "%V"))-35
if(strftime(Sys.Date(), format = "%u")=="1"){week <- as.numeric(strftime(Sys.Date(), format = "%V"))-36}

url <- paste0("https://www.fantasypros.com/nfl/projections/")
# proj <- tibble(Player=c(),FPTS=c(),pos=c())
positions <- c('WR','RB','QB','TE')
nreturn <- c(3,1,1,1)
nreturn_more <- c(5,2,1,2)

proj <- c()
# proj_more <- c()
i <- 2
for (i in 1:length(positions)){
  url2 <- paste0(url,tolower(positions[i]),".php")
  if (i != 3){url2 <- paste0(url2,"?scoring=HALF")}
  popParse <- xml2::read_html(url2)
  popNodes <- rvest::html_nodes(popParse,'table')
  pop <- rvest::html_table(popNodes, header = TRUE, fill = TRUE)[[1]]|> row_to_names(row_number = 1) |> 
    select(Player, FPTS) |> mutate(pos=positions[i],team = word(Player,-1),FPTS=as.numeric(FPTS)) |> 
    arrange(team,pos,desc(FPTS)) |> group_by(team) |> slice(1:nreturn_more[i]) |>
    mutate(starter = case_when(row_number() <= nreturn[i] ~ 1,TRUE ~ 0)) |> ungroup(team)
  # data_DC <- pop |> slice(1:nreturn[i]) |> ungroup(team)
  # data <- pop |> slice(1:nreturn_more[i]) |>mutate(starter = case_when(row_number() <= nreturn[i] ~ 1,TRUE ~ 0)) |> ungroup(team)
  
  proj <- add_row(pop,proj)
  # proj_more <- add_row(data_more,proj_more)
}

temp_name <- sub("(.*)\\s(.*)\\s(.*)","\\1 \\2", proj$Player)
proj$Player <- sub("(.*)\\s(.*)\\s(.*)","\\1 \\2", temp_name)
proj$Player <- str_to_title(proj$Player)

proj <-  proj |> arrange(team,pos,desc(FPTS)) |> select(team,pos,Player,FPTS,starter)
proj$Player[proj$Player=="Equanimeous St."] <- "Equanimeous St. Brown"
proj$Player[proj$Player=="Amon-Ra St."] <- "Amon-Ra St. Brown"



################# sleeper rosters ##################
week <- as.numeric(strftime(Sys.Date(), format = "%V"))-35
if(strftime(Sys.Date(), format = "%u")=="1"){week <- as.numeric(strftime(Sys.Date(), format = "%V"))-36}

anti <- sleeper_connect(season=2024, league_id =1050833763489394688)
rostered <- ff_rosters(anti) |> mutate(Player=player_name) |> select(franchise_name,Player,pos)
rostered$Player <- str_to_title(rostered$Player)


FA <- setdiff(proj[,3],rostered['Player'])
# colnames(FA)[1] <- 'Player'


final <- inner_join(FA,proj) |> filter(starter==1) |> mutate(FPTS=as.numeric(FPTS)) |> arrange(FPTS) |> group_by(pos) |> slice(1:5) |> arrange(pos,FPTS) |> select(1:4)

left_join(FA,proj) |> mutate(FPTS=as.numeric(FPTS)) |> filter(is.na(pos)==T)

inelligibles <- rostered |> filter(pos %in% c('QB','RB','TE','WR'))
elligibles <- proj |> filter(starter==1)
inelligibles <- inner_join(rostered,setdiff(inelligibles['Player'],elligibles[,3])) |> arrange(franchise_name,Player)

color <- 'lightpink'

teams <- proj |> select(team) |> unique() |> mutate(id=row_number()%%2)
proj <- proj |> mutate(new_team = case_when(lead(team) == team ~ 0,TRUE ~ 1)) #outputs last record for each team
final <- final |> mutate(new_pos = case_when(lead(pos) == pos ~ 0,TRUE ~ 1)) #outputs last record for each pos
frans <- inelligibles |> select(franchise_name) |> unique() |> mutate(id=row_number()%%2)
inelligibles <- left_join(inelligibles,frans) |> 
  mutate(new_fran = case_when(lead(franchise_name) == franchise_name ~ 0,TRUE ~ 1)) #determine new franchise


starters_flag <- is.error(ff_starters(anti,week=week))

if(starters_flag==T){
  inelligibles_table <- datatable(inelligibles,rownames=FALSE, options = list(
    columnDefs = list(list(visible = FALSE, targets = c(3,4))),pageLength=50)) |> 
    formatStyle(
      0:ncol(inelligibles), valueColumns='new_fran',
      # target = 'row',
      `border-bottom` = styleEqual(1, "solid 3px")
    ) |> formatStyle(
      'id',
      target = 'row',
      backgroundColor = styleEqual( c(0,1), c('white','gainsboro'))
    )
}
if(starters_flag==F){
  starters <- ff_starters(anti,week=week) |> mutate(Player=player_name,Starter = starter_status) |> select(Player,Starter)
  starters$Starter <- as.integer(recode(starters$Starter,'starter'=1,'nonstarter'=0))
  starters$Starter <- as.integer(starters$Starter)
  starters$Player <- str_to_title(starters$Player)
  
  inelligibles <- left_join(inelligibles,starters)|> arrange(franchise_name,desc(Starter),Player) |> 
      mutate('Inelligible Starter'=Starter,new_fran = case_when(lead(franchise_name) == franchise_name ~ 0,TRUE ~ 1))
  inelligibles_table <- datatable(inelligibles,rownames=FALSE,options = list(
      columnDefs = list(list(visible = FALSE, targets = c(3,4,5))),pageLength=50)) |> 
    formatStyle(
      0:ncol(inelligibles), valueColumns='new_fran',
      # target = 'row',
      `border-bottom` = styleEqual(1, "solid 3px")
    )|> formatStyle(
      'id',
      target = 'row',
      backgroundColor = styleEqual( c(0,1), c('white','gainsboro'))
    ) |> formatStyle(
      'Starter',
      target = 'row',
      backgroundColor = styleEqual( 1, color)
  ) 
}



DC_table <- datatable(left_join(proj,teams),rownames=FALSE, options = list(columnDefs = list(
    list(visible = FALSE, targets = c(4,5,6))),pageLength = 300
  )) |> formatStyle(
    'id',
    target = 'row',
    backgroundColor = styleEqual( c(1,0), c('white','gainsboro')
  )) |> formatStyle(
      'starter',
      target = 'row',
      backgroundColor = styleEqual( 0, color)
  )|> formatStyle(
    0:ncol(proj), valueColumns='new_team',
    # target = 'row',
    `border-bottom` = styleEqual(1, "solid 3px")
  )

# DC_table <- datatable(left_join(proj,teams),rownames=FALSE, options = list(columnDefs = list(
#   ),pageLength = 300 
# )) |> formatStyle(
#   'id',
#   target = 'row',
#   backgroundColor = styleEqual( c(1,0), c('white','gainsboro')
#   )) |> formatStyle(
#     'starter',
#     target = 'row',
#     backgroundColor = styleEqual( 0, color)
#   )|> formatStyle(
#     0:ncol(proj), valueColumns='new_team',
#     # target = 'row',
#     `border-bottom` = styleEqual(1, "solid 3px")
#   )




final_table <- datatable(final,rownames=FALSE,options = list(columnDefs = list(
  list(visible = FALSE, targets = c(4))),pageLength = 150 )) |> 
  formatStyle(
  'pos',
  target = 'row',
  backgroundColor = styleEqual( c('QB','RB','TE','WR'), rep(c('white','gainsboro'),times=2))
)|> formatStyle(
  0:ncol(final), valueColumns='new_pos',
  # target = 'row',
  `border-bottom` = styleEqual(1, "solid 3px")
)


