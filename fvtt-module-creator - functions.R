# module.creator
# v1.2

replace.in.file <- function(input, output, search, replace){
  x <- readLines(input, encoding="UTF-8", warn=FALSE)
  y <- gsub( search, replace, x )
  con <- file(output, encoding="UTF-8")
  writeLines(y, con)
  close(con)
}

clean.db <- function(i.db, i.system){
  file.t <- tempfile()
  json_data <- lapply(readLines(i.db), fromJSON)
  json_file <- character()  
  if (i.system=="rqg"){
    cat("Sistema RQG\n")
    for (i in 1:length(json_data)){
      if ("description" %in% names(json_data[[i]]$data)) json_data[[i]]$data$description <- ""
      if ("descriptionRqidLink" %in% names(json_data[[i]]$data)){
        json_data[[i]]$data$descriptionRqidLink$rqid <- ""
        json_data[[i]]$data$descriptionRqidLink$name <- ""
        json_data[[i]]$data$descriptionRqidLink$documentType <- ""
      }
      write_json(json_data[[i]], file.t, encoding="UTF-8", null="null", auto_unbox=T)
      json_file.t <- gsub( "[null]", "null", readLines(file.t, encoding="UTF-8"), fixed=T)
      json_file <- c(json_file, json_file.t)
    }
  }else if(i.system=="CoC7"){
    cat("Sistema CoC\n")
    for (i in 1:length(json_data)){
      # Descripciones generaes
      if ("value" %in% names(json_data[[i]]$data$description)) json_data[[i]]$data$description$value <- ""
      if ("value" %in% names(json_data[[i]]$data$skills$data$description)) json_data[[i]]$data$skills$data$description$value <- ""
      # Grupos de opciones
      if ("groups" %in% names(json_data[[i]]$data)){
        temp1 <- json_data[[i]]$data$groups$skills
        for (j in 1:length(temp1)) if ("value" %in% names(json_data[[i]]$data$groups$skills[[j]]$data$description)) json_data[[i]]$data$groups$skills[[j]]$data$description$value <- ""
      }
      if ("items" %in% names(json_data[[i]]$data)){
        if ("value" %in% names(json_data[[i]]$data$items$data$description)) json_data[[i]]$data$items$data$description$value <- ""
      }
      if ("items" %in% names(json_data[[i]])){
        if ("value" %in% names(json_data[[i]]$items$data$description)) json_data[[i]]$items$data$description$value <- ""
      }
      write_json(json_data[[i]], file.t, encoding="UTF-8", null="null", auto_unbox=T)
      json_file.t <- gsub( "[null]", "null", readLines(file.t, encoding="UTF-8"), fixed=T)
      json_file <- c(json_file, json_file.t)
    } 
    
  }else{
    cat("Sistema no soportado\n")
  }
  writeLines(json_file, i.db)
}

clean.all.db <- function(i.path, i.system){
  files <- list.files(i.path, pattern="*.db", full.names = T)
  for (filesi in files) clean.db(filesi, i.system)
}

replace.db.references <- function(i.db, i.dbindex){
  
  diro <- file.path(dirname(tools::file_path_as_absolute(i.db)),"modificados")
  if (!dir.exists(diro)) dir.create(diro)
  
  cat("Fichero ", i.db,"\n")
  json_data.index <- lapply(readLines(i.dbindex, encoding="UTF-8"), fromJSON, simplifyVector = F)
  json_data <- lapply(readLines(i.db, encoding="UTF-8"), fromJSON, simplifyVector = F)
  modificado <- F
  indices <- data.frame()
  for (i in 1:length(json_data.index)) indices <- indices %>%
    bind_rows(data.frame(name=json_data.index[[i]]$name, img=json_data.index[[i]]$img, stringsAsFactors = F))
  
  indices <- indices %>%
    mutate(ene=1:n()) %>%
    group_by(name) %>%
    arrange(name, desc(ene)) %>%
    slice(1) %>%
    ungroup()

  for (i in 1:length(json_data)){
    cat("\tClave: ", json_data[[i]]$name,"\t")
    # Formato creacion-pj, esta en items
    temp1 <- json_data[[i]]$data$items
    if (!is.null(temp1)){
      for (j in 1:length(temp1)){
        if (is.null(json_data[[i]]$data$items[[j]]$img)){
          cat("No existe clave para img\n")
        }else{
          temp2 <- indices %>%
            filter(name==json_data[[i]]$data$items[[j]]$name)
          if (NROW(temp2)>0){
            json_data[[i]]$data$items[[j]]$img <- temp2$img[1]
            modificado <- T
          }else{
            cat("\tClave no encontrada: ", json_data[[i]]$data$items[[j]]$name)
          }
          cat("Cadenas reemplazadas\n")
        }
      }
    }else{
      cat("Nada que reemplazar\n")
    }
    # Formato ocupaciones, esta en skills
    temp1 <- json_data[[i]]$data$skills
    if (!is.null(temp1)){
      for (j in 1:length(temp1)){
        if (is.null(json_data[[i]]$data$skills[[j]]$img)){
          cat("No existe clave para img\n")
        }else{
          temp2 <- indices %>%
            filter(name==json_data[[i]]$data$skills[[j]]$name)
          if (NROW(temp2)>0){
            json_data[[i]]$data$skills[[j]]$img <- temp2$img[1]
            modificado <- T
          }else{
            cat("\tClave no encontrada: ", json_data[[i]]$data$skills[[j]]$name)
          }
          cat("Cadenas reemplazadas\n")
        }
      }
    }else{
      cat("Nada que reemplazar\n")
    }
    # Formato ocupaciones, tambien esta en groups
    temp2 <- json_data[[i]]$data$groups
    if (!is.null(temp2)) if (length(temp2)>0) for (k in 1:length(temp2)){
      temp1 <- json_data[[i]]$data$groups[[k]]$skills
      if (!is.null(temp1)){
        for (j in 1:length(temp1)){
          if (is.null(json_data[[i]]$data$groups[[k]]$skills[[j]]$img)){
            cat("No existe clave para img\n")
          }else{
            temp2 <- indices %>%
              filter(name==json_data[[i]]$data$groups[[k]]$skills[[j]]$name)
            if (NROW(temp2)>0){
              json_data[[i]]$data$groups[[k]]$skills[[j]]$img <- temp2$img[1]
              modificado <- T
            }else{
              cat("\tClave no encontrada: ", json_data[[i]]$data$groups[[k]]$skills[[j]]$name)
            }
            cat("Cadenas reemplazadas\n")
          }
        }
      }else{
        cat("Nada que reemplazar\n")
      }  
    }    
  }
  if (modificado){
    i.db.out <- file.path(diro, basename(i.db))
    cat("\tEscribiendo ", i.db.out, "\n")
    json_file <- character()
    for (i in 1:length(json_data)){
      file.t <- tempfile()
      write_json(json_data[[i]], file.t, encoding="UTF-8", null="null", auto_unbox=T)
      json_file.t <- gsub( "[null]", "null", readLines(file.t, encoding="UTF-8"), fixed=T)
      json_file <- c(json_file, json_file.t)
    }
    writeLines(json_file, i.db.out, useBytes = TRUE)
    replace.in.file(i.db.out, i.db.out,"<\\\\/", "</")    
  }
  indices
}

replace.all.db.references <- function(i.db.dir, i.dbindex){
  temp1 <- data.frame(dbf=list.files(i.db.dir, "*.db", full.names = T), stringsAsFactors = F) %>%
    filter(dbf!=i.dbindex)
  for (i in 1:NROW(temp1)) replace.db.references(temp1$dbf[i], i.dbindex)
}

module.creator <- function(world, module, title, description="", author="", copy=NA,
                           foundrydata = NA, foundryversion = 9,
                           compendiumfolders=NA, clean.descriptions=F, remove.compendiums = NA
                           ){
  
	if (is.null(foundrydata)) stop("Directorio foundrydata incorrecto\n")
	if (is.na(foundrydata)) stop("Directorio foundrydata incorrecto\n")
	if (length(foundrydata)!=1) stop("Directorio foundrydata incorrecto\n")
	if (!dir.exists(foundrydata)) stop("Directorio foundrydata no encontrado\n")
		
  mdir <- file.path(foundrydata,"Data/modules",module)
  cat("Creando modulo en ", mdir, "\n")
  
  if (dir.exists(mdir)) unlink(mdir, recursive=T)
  dir.create(mdir)
  
  # additional directories
  
  if (!any(is.na(copy))) for (d in copy){
    dir.create(file.path(mdir, d), recursive = T)
    file.copy(file.path(foundrydata,"Data/worlds",world,d), dirname(file.path(foundrydata,"Data/modules",module, d)), recursive=T)
  }
  
  # paquetes
  
  dir.create(file.path(mdir, "packs"))
  paquetes <- data.frame(dbfile=list.files(file.path(foundrydata,"Data/worlds",world,"packs"), pattern="*.db", full.names = T)) %>%
    mutate(basename=basename(dbfile),
           filename=tools::file_path_sans_ext(basename),
           fileext=tools::file_ext(basename)) %>%
    filter(!(filename %in% remove.compendiums))
  
  
  for (i in 1:NROW(paquetes)){
    file.1 <- file.path(foundrydata,"Data/worlds",world,"packs", paquetes$basename[i])
    file.2 <- file.path(foundrydata,"Data/modules",module,"packs", paquetes$basename[i])
    replace.in.file(file.1, file.2, paste0("worlds/",world), paste0("modules/",module))
    replace.in.file(file.2, file.2, "data-pack=\\\\\"world\\.", paste0("data-pack=\\\\\"",module,"\\."))
    replace.in.file(file.2, file.2, "@Compendium\\[world\\.", paste0("@Compendium\\[",module,"\\."))
    replace.in.file(file.2, file.2, "Compendium\\.world\\.", paste0("Compendium\\.",module,"\\."))
    replace.in.file(file.2, file.2, "world\\.", paste0(module,"\\."))
  }
  
  # module.json
  
  file.1 <- file.path(foundrydata,"Data/worlds",world, "world.json")
  file.2 <- file.path(foundrydata,"Data/modules",module, "module.json")
  file.3 <- file.path(foundrydata,"Data/modules",module, paste0(module, ".js"))
  
  # json_data <- fromJSON(file=file.1)
  json_data <- fromJSON(file.1, simplifyVector = F)
  
  if (is.null(json_data$id)) d.core=9 else d.core=10
  
  cat("Core compatibility: ", d.core, "\n")

  if (d.core<10){
    json_data$name <- module
    json_data$author <- author
    json_data$minimumCoreVersion <- str_extract(json_data$coreVersion, "[^\\.]")
    json_data$compatibleCoreVersion <- json_data$coreVersion
  }else{
    json_data$id <- module
    json_data$authors[[1]] <- list(name=author, flags=NULL)
    json_data$relationships[[1]] <- list(id="compendium-folders", type="module", compatibility=NULL)
      json_data$relationships[[2]] <- list(id=json_data$system, type="system", compatibility=json_data$systemVersion)
    names(json_data$relationships) <- c("requires", "systems")
  }
  
  json_data$title <- title
  json_data$description <- description

  packsnames <- character()
  for (i in 1:length(json_data$packs)){
    json_data$packs[[i]]$package <- module
    json_data$packs[[i]]$absPath <- json_data$packs[[i]]$path
    packsnames <- c(packsnames, json_data$packs[[i]]$name)
  }
  
  json_data$packs[packsnames %in% remove.compendiums] <- NULL
  
  # if (!is.na(config.compendiumfolders)) if (file.exists(config.compendiumfolders)){
  #   json_data$esmodules <- list(paste0(module,".js"))
  #   json_data$dependencies <- list(list(name = "compendium-folders", 
  #                                       manifest="https://raw.githubusercontent.com/earlSt1/vtt-compendium-folders/master/module.json", 
  #                                       version="1.0.5"))
  #   json_data<-json_data[c("name","title","description","author","version","minimumCoreVersion","compatibleCoreVersion","esmodules","dependencies", "packs")]
  #   
  # }else{
  #   json_data<-json_data[c("name","title","description","author","version","minimumCoreVersion","compatibleCoreVersion", "packs")]
  #   
  # }
  
  if (is.na(compendiumfolders)){
    json_data$esmodules <- NULL
    json_data$dependencies <- NULL
  }else{
    config.compendiumfolders <- file.path(foundrydata,"Data/worlds",world, compendiumfolders)
    if (file.exists(config.compendiumfolders)){
      json_data$esmodules <- list(paste0(module,".js"))
      json_data$dependencies <- list(list(name = "compendium-folders", 
                                          manifest="https://raw.githubusercontent.com/earlSt1/vtt-compendium-folders/master/module.json", 
                                          version="1.0.5"))
    }else{
      json_data$esmodules <- NULL
      json_data$dependencies <- NULL
    }
  }
  
  # json_data<-json_data[names(json_data) %in% c("name","title","description","author","version","coreVersion","minimumCoreVersion","compatibleCoreVersion","esmodules","dependencies", "packs", "system", "systemVersion")]
  
  if (d.core<10){
    json_data<-json_data[names(json_data) %in% c("name","title","description","author","version","minimumCoreVersion","compatibleCoreVersion","esmodules",
                                                 "dependencies", "packs", "system", "systemVersion")]
  }else{
    json_data<-json_data[names(json_data) %in% c("id","title","description","authors","version","compatibility","esmodules",
                                                 "relationships", "packs")]
  }
  
  write_json(json_data, file.2, pretty=T, auto_unbox=T)
  
  # t.file <- tempfile()
  # replace.in.file(config.compendiumfolders, t.file,"world\\.", paste0(module,"\\."))
  # replace.in.file(t.file, t.file,paste0("worlds/",world), paste0("modules/",module))
  # codigocc <- readLines(t.file, warn=FALSE)[1]
  # 
  # if (sum(!is.na(remove.folders))>0){
  #   for (rf in remove.folders){
  #     codigocc <- gsub(paste0(",\"cfolder_[[:alnum:]]{11}\":\\{","\"titleText\":\"",rf,"\"[^\\}]*\\},"),",",codigocc)
  #     codigocc <- gsub(paste0("\"cfolder_[[:alnum:]]{11}\":\\{","\"titleText\":\"",rf,"\"[^\\}]*\\},"),"",codigocc)
  #     codigocc <- gsub(paste0(",\"cfolder_[[:alnum:]]{11}\":\\{","\"titleText\":\"",rf,"\"[^\\}]*\\}"),"",codigocc)
  #   }
  # }
  # 
  # replace.in.file("H:/R/fvtt-module-creator/fvtt-module-creator.js", file.3, "@~@" ,codigocc)
  
  # if (sum(!is.na(remove.compendiums))>0){
  #   for (rc in remove.compendiums){
  #     # cat(rc,"\n")
  #     replace.in.file(file.3, file.3,paste0(",\"","compendios-llc-es-basicos.",rc,"\","), ",")
  #     replace.in.file(file.3, file.3,paste0("\"","compendios-llc-es-basicos.",rc,"\","), "")
  #     replace.in.file(file.3, file.3,paste0(",\"","compendios-llc-es-basicos.",rc,"\""), "")
  #     replace.in.file(file.3, file.3,paste0("\"","compendios-llc-es-basicos.",rc,"\""), "")
  #   }
  # } 
  
  cf_data <- fromJSON(config.compendiumfolders, simplifyVector = F)
  
  # correccion iconos
  # cf_data[["default"]]$folderIcon <- NULL

  configs <- names(cf_data)[grepl("cfolder_.*",names(cf_data))]
  listado.directorios <- data.frame()
  for (cfs in configs){
    name = cfs
    titulo = cf_data[[cfs]]$titleText
    # quitar=ifelse(cf_data[[cfs]]$titleText %in% remove.folders,T,F)
    
    if (length(cf_data[[cfs]]$folderIcon)>0) cf_data[[cfs]]$folderIcon <- gsub(paste0("worlds/",world),paste0("modules/",module), cf_data[[cfs]]$folderIcon)
    if (length(cf_data[[cfs]]$compendiumList)>0) for (i in length(cf_data[[cfs]]$compendiumList):1){
      coname <- sub("[^\\.]*\\.(.*)","\\1",cf_data[[cfs]]$compendiumList[[i]])
      if (coname %in% remove.compendiums){
        cf_data[[cfs]]$compendiumList[[i]] <- NULL
      }else{
        cf_data[[cfs]]$compendiumList[[i]] <- gsub("world\\.",paste0(module,"\\."), cf_data[[cfs]]$compendiumList[[i]])
      }
    }
    if (length(cf_data[[cfs]]$compendiums)>0) for (i in length(cf_data[[cfs]]$compendiums):1){
      coname <- sub("[^\\.]*\\.(.*)","\\1",cf_data[[cfs]]$compendiums[[i]]$code)
      if (coname %in% remove.compendiums){
        cf_data[[cfs]]$compendiums[[i]] <- NULL
      }else{
        cf_data[[cfs]]$compendiums[[i]]$code <- gsub("world\\.",paste0(module,"\\."), cf_data[[cfs]]$compendiums[[i]]$code)
      }
    }
    comli <- length(cf_data[[cfs]]$compendiumList)
    com <- length(cf_data[[cfs]]$compendiums)
    if (!is.null(unlist(cf_data[[cfs]]$pathToFolder))) direc = unlist(cf_data[[cfs]]$pathToFolder) else direc=""
    listado.directorios <- listado.directorios %>%
      bind_rows(data.frame(name=name, 
                           titulo=titulo, 
                           #quitar=quitar,
                           comli=comli,
                           com=com,
                           parent=ifelse(is.null(unlist(cf_data[[cfs]]$parent)),"",cf_data[[cfs]]$parent),
                           pathToFolder=direc, stringsAsFactors = F))
  }

  # Eliminar carpetas vacias
  # directorios.contenedores <- listado.directorios %>%
  #   filter(pathToFolder!="") %>%
  #   distinct(pathToFolder) %>%
  #   left_join(listado.directorios, by=c("pathToFolder"="name"))
  # 
  # directorios.borrar <- listado.directorios %>%
  #   filter(!((comli>0 & com>0) | name %in% directorios.contenedores$pathToFolder))
  
  temp1 <- listado.directorios %>%
    filter(comli>0 | com>0)
  
  temp2 <- listado.directorios %>%
    filter(name %in% temp1$parent)
  
  directorios.mantener <- temp1 %>%
    bind_rows(temp2)
  
  directorios.borrar <- listado.directorios %>%
    filter(!(name %in% directorios.mantener$name))
  
  if (NROW(directorios.borrar)>0) for (i in 1:NROW(directorios.borrar)) cf_data[[directorios.borrar$name[i]]] <- NULL

  replace.in.file("H:/R/fvtt-module-creator/fvtt-module-creator.js", file.3, "@~@" ,toJSON(cf_data, null="null", auto_unbox = T))
  
  if (clean.descriptions){
    packs.path <- file.path(foundrydata,"Data/modules",module,"packs")
    cat("Removing descriptions from: ", packs.path,"\n")
    clean.all.db(packs.path, json_data$system)
  }
}
