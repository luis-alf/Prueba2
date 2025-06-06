library(data.table)
library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)

library(sf)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ents  <- st_read("00ent.shp")
ents$ent <- as.numeric(ents$CVE_ENT)
ents <- st_transform(ents, crs = 4326)


dt <- fread('ENOE_COE2T125.csv')

dt <- data.frame(dt)

num_vars <- c('EDA','P6B2','P7F_DIAS','P7F_HORAS','P7GCAN','P11_H1','P11_M1',
             'P11_H2','P11_M2','P11_H3','P11_M3','P11_H4','P11_M4','P11_H5','P11_M5',
             'P11_H6','P11_M6','P11_H7','P11_M7','P11_H8','P11_M8','FAC_TRI','FAC_MEN')  

num_vars <- tolower(num_vars)


colnames(dt) <- toupper(colnames(dt))
colnames(dt) <- tolower(colnames(dt))

colnames(dt) <- gsub('_',' ',colnames(dt))
colnames(dt) <- gsub(' ','.',colnames(dt))
colnames(dt) <- gsub('\\.','_',colnames(dt))

lapply(dt, class)

dt[] <- lapply(dt,as.factor)
dt[,num_vars] <- lapply(dt[,num_vars], function(x) as.numeric(as.character(x)))

colnames(dt)[!colnames(dt) %in% num_vars]

dt[,colnames(dt)[!colnames(dt) %in% num_vars]] <- lapply(dt[,colnames(dt)[!colnames(dt) %in% num_vars]],as.factor)

num_vars[grepl('p11_',num_vars)]

dt[,num_vars[grepl('p11_',num_vars)]] <- lapply(dt[,num_vars[grepl('p11_',num_vars)]], function(x) ifelse(x>97,NA,x))
dt$p7gcan[dt$p7gcan==999999] <- NA
dt$p7f_horas <- ifelse(dt$p7f_horas>97,NA,dt$p7f_horas)
dt$p7f_dias[dt$p7f_dias==9] <- NA
dt$p6b2[dt$p6b2==999999] <- NA
dt$eda[dt$eda>97] <- NA

dt[] <- lapply(dt,function(x){
  
  if(length(table(x))==1){
    x <- as.character(x)
    x[is.na(x)] <- '0'
    x <- factor(x)
    
  }
  
  x
  
})

summary(dt)

ggplot(dt)+
  aes(x=eda)+
  geom_histogram()


ggplot(dt)+
  aes(x=eda)+
  geom_histogram(color='black',fill='forestgreen',binwidth = 5)+
  theme_minimal()

ggplot(dt)+
  aes(x=p6b2)+
  geom_histogram(color='black',fill='forestgreen',binwidth = 5)+
  theme_minimal()


ggplot(dt)+
  aes(x=p6b2)+
  geom_histogram(color='black',fill='forestgreen',bins = 30)+
  scale_x_log10(labels=scales::label_number())+
  theme_minimal()


ggplot(dt)+
  aes(x=p6b2)+
  geom_density(color='black',linewidth=0.75)+
  theme_minimal()


ggplot(dt)+
  aes(x=log(p6b2))+
  geom_density(color='black',linewidth=1)+
  scale_x_continuous(labels = function(x) round(exp(x)), breaks = scales::pretty_breaks())+
  theme_minimal()

ggplot(dt)+
  aes(x=p6b2)+
  geom_density(color='black',linewidth=1)+
  scale_x_log10()+
  theme_minimal()




dt %>%
  group_by(ent) %>%
  summarise(n=n(),
            menos_mil=sum(p6b2<1000,na.rm=T)) %>%
  mutate(porcentaje=menos_mil/n*100)

dt %>%
  count(ent)

dt %>%
  group_by(ent) %>%
  summarise(n=n()) %>%
  arrange(-n) %>%
  mutate(ent=factor(ent,levels=ent)) %>%
  # mutate(ent=factor(ent, levels=.[['ent']][order(-.[['n']])])) %>%
  ggplot()+
  aes(x=ent,y=n)+
  geom_col(color='black',fill='forestgreen')+
  theme_minimal()



dt2 <- dt %>%
  group_by(ent) %>%
  summarise(n=n()) %>%
  arrange(-n)

dt2$ent <- factor(dt2$ent,levels = dt2$ent)

ggplot(dt2)+
  aes(x=ent,y=n)+
  geom_col(color='black',fill='forestgreen')+
  theme_minimal()


ggplot(dt)+
  aes(x=ent)+
  geom_bar(color='black',fill='forestgreen')+
  theme_minimal()


ggplot(dt[sample(1:nrow(dt),10000),])+
  aes(x=eda,y=p6b2)+
  geom_point()+
  theme_minimal()

ggplot(dt[sample(1:nrow(dt),10000),])+
  aes(x=eda,y=log(p6b2))+
  geom_point()+
  theme_minimal()


ggplot(dt)+
  aes(x=eda,y=p6b2)+
  geom_hex()+
  scale_y_log10(labels=scales::label_dollar())+
  theme_minimal()



ggplot(dt[sample(1:nrow(dt),10000),])+
  aes(x=eda,y=p6b2)+
  geom_point()+
  geom_smooth(linewidth=2)+
  theme_minimal()



ggplot(dt[sample(1:nrow(dt),10000),])+
  aes(x=eda,y=log(p6b2))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE,color='red')+
  theme_minimal()


ggplot(dt[sample(1:nrow(dt),10000),])+
  aes(x=p7f_horas,y=log(p6b2))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE,color='red')+
  theme_minimal()


ggplot(dt[!is.na(dt$p6b1) & !is.na(dt$p6d),])+
  aes(x=p6b1,y=p6d)+
  geom_count(color='forestgreen')+
  theme_minimal()

dt %>% 
  filter(!is.na(dt$p6b1) & !is.na(dt$p6d)) %>%
  group_by(p6b1,p6d) %>%
  summarise(n=n()) %>%
  ggplot()+
  aes(x=p6b1,y=p6d, size=n, alpha=n)+
  geom_point(color='forestgreen')+
  scale_size_continuous(range = c(5,25))+
  scale_alpha_continuous(range = c(3/4,1))+
  theme_minimal()

all <- unique(dt[!is.na(dt$p6b1) & !is.na(dt$p6d),c('p6b1','p6d')]) %>% expand(p6b1, p6d)
all


dt %>% 
  filter(!is.na(dt$p6b1) & !is.na(dt$p6d)) %>%
  group_by(p6b1,p6d) %>%
  summarise(n=n()) %>%
  as_tibble() %>%
  right_join(all, by=c('p6b1','p6d')) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  ggplot() +
  aes(x=p6b1,y=p6d,fill=n)+
  geom_tile(color='gray95')+
  scale_fill_gradient(low = 'white',high='forestgreen')+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


dt %>% 
  filter(!is.na(dt$p6b1) & !is.na(dt$p6d)) %>%
  group_by(p6b1,p6d) %>%
  summarise(n=n()) %>%
  as_tibble() %>%
  right_join(all, by=c('p6b1','p6d')) %>%
  mutate(n=ifelse(is.na(n),0,n)) %>%
  ggplot() +
  aes(x=p6b1,y=p6d,fill=n)+
  geom_tile(color='gray95')+
  geom_text(aes(label=scales::comma(n)),color='gray25')+
  scale_fill_gradient(low = 'white',high='forestgreen')+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  

dt %>% 
  group_by(ent) %>%
  summarise(n=n()) %>%
  mutate(ent = as.numeric(as.character(ent))) %>%
  left_join(ents,by='ent') %>%
  ggplot() +
  aes(fill=n, geometry=geometry)+
  geom_sf()+
  scale_fill_gradient(low = 'white',high='forestgreen')+
  theme_void()



plist <- lapply(colnames(dt), function(vr1){
  lapply(colnames(dt), function(vr2){
    
    if(vr1==vr2){
      
      if(class(dt[[vr1]])=='factor'){
        
        p <- ggplot()+
          aes(x=dt[[vr1]])+
          geom_bar(color='black',fill='forestgreen')+
          labs(x=vr1)+
          theme_minimal()
        
        # if(vr1=='cd_a'){
        #   
        #   p <- p + theme_void()
        #   
        # }
        
      }else{
        
        p <- ggplot()+
          aes(x=dt[[vr1]])+
          geom_histogram(color='black',bins=30)+
          labs(x=vr1)+
          theme_minimal()
        
      }
      
    }else{
      
      if(class(dt[[vr1]])=='factor' & class(dt[[vr2]])=='factor'){
        
        p <- ggplot()+
          aes(y=dt[[vr1]], x=dt[[vr2]])+
          geom_count(color='forestgreen')+
          labs(y=vr1,x=vr2)+
          theme_minimal()
        
      }else if(class(dt[[vr1]])=='factor'){
        
        p <- ggplot()+
          aes(y=dt[[vr1]], x=dt[[vr2]], group=dt[[vr1]])+
          geom_boxplot()+
          labs(y=vr1,x=vr2)+
          theme_minimal()
        
      }else if(class(dt[[vr2]])=='factor' ){
        
        p <- ggplot()+
          aes(y=dt[[vr1]], x=dt[[vr2]], group=dt[[vr2]])+
          geom_boxplot()+
          labs(y=vr1,x=vr2)+
          theme_minimal()
        
      }else{
        
        p <- ggplot()+
          aes(y=dt[[vr1]], x=dt[[vr2]])+
          #geom_point(color='forestgreen')+
          geom_hex()+
          scale_fill_gradient(low = 'white',high='forestgreen')+
          theme_minimal()
        
      }
      
    }
    
    p
    
  }) 
})


library(patchwork)

ggsave('distribuciones.svg',patchwork::wrap_plots(unlist(plist,recursive=F),ncol = ncol(dt)),bg='white',
       width=30,height=20,dpi=1200) 








