library(data.table)
library(tidyverse)

files <- list.files('D:/Downloads/Nacimientos/Procesados',full.names = T)

f <- files[1]

f <- files[16]

lapply(files, function(f) colnames(fread(f,nrows = 0)))

df <- lapply(files, function(f){
  
  df <- fread(f)
  
  if(as.numeric(gsub('.*/|_.*','',f))<2017){
    df <- df[,list(n=.N),by=c('edad_madre','estado_conyugal','escolaridad_madre','ocupacion_habitual_madre',
                              'entidad_residencia_madre','municipio_residencia_madre','localidad_residencia_madre','clues',
                              'fecha_nacimiento_nac_vivo',
                              'numero_embarazos')]
  }else if(as.numeric(gsub('.*/|_.*','',f))<2021){
    
    df <- df[,list(n=.N),by=c('EDADM','EDOCIVIL','NIV_ESCOL','CVEOCUPHAB',
                              'ENT_RES','MPO_RES','LOC_RES','CLUES',
                              'FECH_NACH',
                              'NUM_EMB')]
  }else{
    
    
    df <- df[,list(n=.N),by=c('EDAD','ESTADOCONYUGAL','ESCOLARIDAD','CLAVEOCUPACIONHABITUAL',
                              'ENTIDADRESIDENCIA','MUNICIPIORESIDENCIA','LOCALIDADRESIDENCIA','CLUES',
                              'FECHANACIMIENTO',
                              'NUMEROEMBARAZOS')]
  }
  
  colnames(df) <- c('edad_madre','estado_conyugal','escolaridad_madre','ocupacion_habitual_madre',
                    'entidad_residencia_madre','municipio_residencia_madre','localidad_residencia_madre','clues',
                    'fecha_nacimiento_nac_vivo',
                    'numero_embarazos','n')
  
  df[,list(n=sum(n)),by='fecha_nacimiento_nac_vivo']
  

  
}) %>% rbindlist()


