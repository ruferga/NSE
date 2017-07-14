parent <- "C:/Users/Ruben/Documents/Datos/Scince/Estados"

dirs <- list.dirs(parent, full.names = F, recursive = F)

require(foreign)

lapply(dirs, 

        for(i in 1:length(dirs)){
    
          pob <- read.dbf(paste(parent, dirs[i], "ageb_urb.dbf", sep = "/"))
          edu <- read.dbf(paste(parent, dirs[i], "tablas", 
                                "cpv2010_ageb_urb_caracteristicas_educativas.dbf", sep = "/"))
          viv <- read.dbf(paste(parent, dirs[i], "tablas", 
                                "cpv2010_ageb_urb_viviendas.dbf", sep = "/"))
       
          #Unir las tablas requeridas para estimar los NSE con el algoritmo de ITDP a nivel ageb
         
          union1 <- merge(pob, edu, by=intersect(names(pob), names(edu)))
          merge <- merge(union1, viv, by=intersect(names(union1), names(viv)))
          
          #table(names(merge))
          
          merge_sub <- subset(merge, select = c("CVEGEO", "POB22", "VIV33","VIV27",	"VIV26", 
                                                "EDU46",	"EDU49_R", "VIV2", "POB1", "VIV34",
                                                "VIV24", "VIV36", "VIV32","VIV12", "VIV28"))
          
          #Reempalza -8 y -9 por 0 y -7 y -6 por NA
          
          sel <- grepl("^EDU[:alnum:]?|(_[:alpha:]+?)|^VIV[:alnum:]?|^POB[:alnum:]?", 
                       names(merge_sub))
          
          merge_sub[sel] <- lapply(merge_sub[sel], function (x) replace(x,x %in% -7:-6, NA))
          merge_sub[sel] <- lapply(merge_sub[sel], function (x) replace(x,x %in% -9:-8, 0))
          
          #Representación gráfica de los NA
          require(VIM)
          aggr_plot <- aggr(merge_sub, col=c('pink','coral'),
                            numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7,
                            gap=3, ylab=c("Histogram of missing data","Pattern"))
          
          #Prueba para determinar si los NA's son más del 5%
          pMiss <- function(x){sum(is.na(x)/length(x)*100)}
          apply(merge_sub, 2, pMiss)
          
          #División de la base merge_sub para que sea posible la imputación de valores faltantes
          #Mice no es capaz de procesar la variable cvegeo
          
          require(dplyr)
          
          #merge_sub; sort(merge_sub)
          
          merge_sub <- mutate(merge_sub, id = row.names(merge_sub))
          
          matriz <- subset(merge_sub, select= c("id", "POB22", "VIV33", "VIV27",
                                                "VIV26", "EDU46", "EDU49_R", "POB1", "VIV2",
                                                "VIV34", "VIV24",	"VIV36", "VIV32","VIV12", "VIV28"))
          
          cvegeo <- subset(merge_sub, select=c("CVEGEO", "id"))
          
          
          #Imputación de los valores faltantes
          
        
          require(mice)
          
          tempData <- mice(data = matriz, m=5, method = "norm.nob", maxit = 50, seed = 500)
          
          summary(tempData)
          
          # Completar las bases de datos derivadas de matriz
          completo1 <- complete(tempData, 1)
          completo2 <- complete(tempData, 2)
          completo3 <- complete(tempData, 3)
          completo4 <- complete(tempData, 4)
          completo5 <- complete(tempData, 5)
          
          completo1$cid <- "c1"
          completo2$cid <- "c2"
          completo3$cid <- "c3"
          completo4$cid <- "c4"
          completo5$cid <- "c5"
          
          #Se combinan las tablas completas derivadas de matriz
          
          require(sjmisc)
          
          merged_comp <- Reduce(function(x,y) merge(x, y, all = T), 
                    list(completo1, completo2, completo3, completo4, completo5))
          
          merged_comp[complete.cases(merged_comp), ]
          
          #View(merged_comp)
          
          merged_comp1 <- merged_comp
          
          merged_comp1$cid <- NULL
          
          #View(merged_comp1)
          
          #Se colapsan las tablas completas
          
          require(data.table)
          
          comp_means <- setDT(merged_comp1)[ , list(pob22 = mean(POB22), viv33 = mean(VIV33),
                                                    viv27 = mean(VIV27),viv26 = mean(VIV26), 
                                                    edu46 = mean(EDU46), edu49_r = mean(EDU49_R),
                                                    pob1 = mean(POB1), viv2 = mean(VIV2)), 
                                                    by = (id)]
          
          
          View(comp_means)
          
          #Cálculo de los índices en la tabla comp_means
            #Índice Computadora 
          
          comp_means$compu <- comp_means$viv33/comp_means$viv27
          
          comp_means$compu1 <- comp_means$viv33/comp_means$viv26
          
          
            #Índice Escolaridad
          
          comp_means$educa <- comp_means$edu49_r/max(comp_means$edu49_r)
          
          
            #Índice Educación Superior
          
          comp_means$edusup <- comp_means$edu46/comp_means$pob22
          
          
          #Visualización de NAs
          require(VIM)
          aggr_plot <- aggr(comp_means, col=c('pink','coral'),
                            numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7,
                            gap=3, ylab=c("Histogram of missing data","Pattern"))
          
          #PCA índices 
          
          require(mice)
          
          tempData1 <- mice(data = comp_means, m=5, method = "norm.nob", maxit = 50, seed = 500)
          
          summary(tempData1)
          
          # Completar las bases de datos
          edusup_comp1 <- complete(tempData1, 1)
          edusup_comp2 <- complete(tempData1, 2)
          edusup_comp3 <- complete(tempData1, 3)
          edusup_comp4 <- complete(tempData1, 4)
          edusup_comp5 <- complete(tempData1, 5)
          
          edusup_comp1$cid <- "c1"
          edusup_comp2$cid <- "c2"
          edusup_comp3$cid <- "c3"
          edusup_comp4$cid <- "c4"
          edusup_comp5$cid <- "c5"
          
          #Se combinan las tablas completas 
          
          require(sjmisc)
          
          comp_merged <- Reduce(function(x,y) merge(x, y, all = T), 
                                list(edusup_comp1, edusup_comp2, edusup_comp3, 
                                     edusup_comp4, edusup_comp5))
          
          comp_merged[complete.cases(comp_merged), ]
          
          #View(comp_merged)
          
          comp_merged1 <- comp_merged
          
          comp_merged1$cid <- NULL
          
          #Se colapsan las tablas completas
          
          require(data.table)
          
          comp_means2 <- setDT(comp_merged1)[ , list(pob22 = mean(pob22), 
                                              viv33 = mean(viv33), viv27 = mean(viv27), 
                                              viv26 = mean(viv26), edu46 = mean(edu46), 
                                              edu49_r = mean(edu49_r), pob1 = mean(pob1), 
                                              viv2 = mean(viv2),compu = mean(compu), 
                                              compu1 = mean(compu1), educa = mean(educa), 
                                              edusup = mean(edusup)), by = (id)]
          
          comp_means2 <- comp_means2[complete.cases(comp_means2)]
          
          comp_means2 <- comp_means2[comp_means2$compu < Inf]
          
          comp_means2 <- comp_means2[comp_means2$compu1 < Inf]
          
          comp_means2 <- comp_means2[comp_means2$educa < Inf]
          
          comp_means2 <- comp_means2[comp_means2$edusup < Inf]
          
          comp_means2 <- comp_means2[comp_means2$compu > -Inf]
          
          comp_means2 <- comp_means2[comp_means2$compu1 > -Inf]
          
          comp_means2 <- comp_means2[comp_means2$educa > -Inf]
          
          comp_means2 <- comp_means2[comp_means2$edusup > -Inf]
          
          
          pcaind <- prcomp(~compu+educa+edusup, data = comp_means2, tol = 1, scale = T)
          pcaind1 <- prcomp(~compu1+educa+edusup, data = comp_means2, tol = 1, scale = T)
          pcavar<- prcomp(~pob22+viv33+viv27+viv26+edu46+edu49_r, data = comp_means2,
                          tol = 1, scale = T)
          
          comp_means2$s_ind <- na.pass(predict(pcaind))
          comp_means2$s_ind1 <- na.pass(predict(pcaind1))
          comp_means2$s_var <- na.pass(predict(pcavar))
          
          #Se calculan los clusters para s_ind y s_var
          
          kmeans <- kmeans(subset(comp_means2,select = c("s_ind")), 7
                           , nstart = 25, algorithm = c("Lloyd"))
          kmeans1 <- kmeans(subset(comp_means2,select = c("s_ind1")), 7
                            , nstart = 25, algorithm = c("Lloyd"))
          kmeans2 <- kmeans(subset(comp_means2,select = c("s_var")), 7
                            , nstart = 25, algorithm = c("Lloyd"))
          
          comp_means2$clu_ind <- fitted(kmeans, method = c("classes"))
          comp_means2$clu_ind1 <- fitted(kmeans1, method = c("classes"))
          comp_means2$clu_var <- fitted(kmeans2, method = c("classes"))
          
          #View(comp_means2)
          
          require(Hmisc)
          
          #rcorr(comp_means2$s_var, comp_means2$s_ind)
          #rcorr(comp_means2$s_var, comp_means2$s_ind1)
          #rcorr(comp_means2$s_ind, comp_means2$edu46)
          #rcorr(comp_means2$s_ind1, comp_means2$edu46)
          #rcorr(comp_means2$s_var, comp_means2$edu46)
          
          # Con los scores de los índices del ITDP
          #Genera una tabla resumen de los scores y los clusters
          
          require(data.table)
          
          t_ind <- setDT(comp_means2)[ , list(mean = mean(s_ind)), by = (clu_ind)]
          t_ind <- t_ind[order(-mean)]
          t_ind$rank <- seq.int(nrow(t_ind))
          
          t_ind1 <- setDT(comp_means2)[ , list(mean1 = mean(s_ind1)), by = (clu_ind1)]
          t_ind1 <- t_ind1[order(-mean1)]
          t_ind1$rank1 <- seq.int(nrow(t_ind1))
          
          #Revisar t_ind y sustituir los valores de forma descendiente en el }siguiente código
          #Con s_ind hay un relación directa (revisar matriz de correlación arriba: + es +)
          
          t_ind$nse_ind <- "NA"
          
          t_ind$nse_ind[t_ind$rank== 7]	<- "1A/B"
          t_ind$nse_ind[t_ind$rank== 6]	<- "2C+"
          t_ind$nse_ind[t_ind$rank== 5]	<- "3C"
          t_ind$nse_ind[t_ind$rank== 4]	<- "4C-"
          t_ind$nse_ind[t_ind$rank== 3]	<- "5D+"
          t_ind$nse_ind[t_ind$rank== 2]	<- "6D"
          t_ind$nse_ind[t_ind$rank== 1]	<- "7E"
          t_ind   
          
          t_ind1$nse_ind1 <- "NA"
          
          t_ind1$nse_ind1[t_ind1$rank1== 7]	<- "1A/B"
          t_ind1$nse_ind1[t_ind1$rank1== 6]	<- "2C+"
          t_ind1$nse_ind1[t_ind1$rank1== 5]	<- "3C"
          t_ind1$nse_ind1[t_ind1$rank1== 4]	<- "4C-"
          t_ind1$nse_ind1[t_ind1$rank1== 3]	<- "5D+"
          t_ind1$nse_ind1[t_ind1$rank1== 2]	<- "6D"
          t_ind1$nse_ind1[t_ind1$rank1== 1]	<- "7E"
          t_ind1
          # Con los scores de las variables elegidas
          #Genera una tabla resumen de los scores y los clusters
          
          t_var <- setDT(comp_means2)[ , list(mean2 = mean(s_var)), by = (clu_var)]
          t_var <- t_var[order(mean2)]
          t_var$rank2 <- seq.int(nrow(t_var))
          t_var 
          
          #Revisar t_ind y sustituir los valores de forma descendiente en el siguiente código
          #Con s_ind hay un relación inversa (revisar matriz de correlación arriba: + es -)
          
          t_var$nse_var <- "NA"
          
          t_var$nse_var[t_var$rank2== 1]	<- "1A/B"
          t_var$nse_var[t_var$rank2== 2]	<- "2C+"
          t_var$nse_var[t_var$rank2== 3]	<- "3C"
          t_var$nse_var[t_var$rank2== 4]	<- "4C-"
          t_var$nse_var[t_var$rank2== 5]	<- "5D+"
          t_var$nse_var[t_var$rank2== 6]	<- "6D"
          t_var$nse_var[t_var$rank2== 7]	<- "7E"
          
          #Unir la base con los nse y los scores con la clave geográfica
          
          comp_means_nse <- merge(comp_means2, t_ind, by = c('clu_ind'))
          
          comp_means_nse1 <- merge(comp_means_nse, t_ind1, by = c('clu_ind1'))
          
          comp_means_nse2 <- merge(comp_means_nse1, t_var, by = c('clu_var'))
          
          ageb_urb_nse <- merge(cvegeo, comp_means_nse2, by= 'id')
          
          View(ageb_urb_nse)
          
          #Genera tablas resumen población, viviendas, nse
          
          tnse_ind <- setDT(ageb_urb_nse)[ , list(personas = sum(pob1), 
                                                  hogares = sum(viv2)), by = (nse_ind)]
          tnse_ind <- tnse_ind[order(nse_ind)]
          
          tnse_ind$cum_pob <- cumsum(tnse_ind$personas)
          tnse_ind$cum_hog <- cumsum(tnse_ind$hogares)
          tnse_ind$por_pob <- tnse_ind$personas/sum(tnse_ind$personas)*100
          tnse_ind$por_hog <- tnse_ind$hogares/sum(tnse_ind$hogares)*100
          
          
          tnse_ind1 <- setDT(ageb_urb_nse)[ , list(personas = sum(pob1), 
                                                  hogares = sum(viv2)), by = (nse_ind1)]
         
          tnse_ind1 <- tnse_ind1[order(nse_ind1)]
          
          tnse_ind1$cum_pob <- cumsum(tnse_ind1$personas)
          tnse_ind1$cum_hog <- cumsum(tnse_ind1$hogares)
          tnse_ind1$por_pob <- tnse_ind$personas/sum(tnse_ind1$personas)*100
          tnse_ind1$por_hog <- tnse_ind$hogares/sum(tnse_ind1$hogares)*100
          
          tnse_var <- setDT(ageb_urb_nse)[ , list(personas = sum(pob1), 
                                                  hogares = sum(viv2)), by = (nse_var)]
          
          tnse_var <- tnse_var[order(nse_var)]
          
          tnse_var$cum_pob <- cumsum(tnse_var$personas)
          tnse_var$cum_hog <- cumsum(tnse_var$hogares)
          tnse_var$por_pob <- tnse_var$personas/sum(tnse_var$personas)*100
          tnse_var$por_hog <- tnse_var$hogares/sum(tnse_var$hogares)*100
          
          write.csv(ageb_urb_nse, file = paste(parent, dirs[i],  "ageb_urb_nse.csv",
                                               sep = "/"), fileEncoding = "UTF8")
          write.csv(tnse_ind1, file =paste(parent, dirs[i], "tnse_ind1.csv", sep = "/")
                    , fileEncoding = "UTF8")
          
          cat("\014")
})
