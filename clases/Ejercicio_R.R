# dir <- "C:\\Users\\carli\\OneDrive\\Documents\\AS10\\AS10 Material de apoyo\\R\\Paquete estadistico R\\Ficheros de datos"
# setwd(dir)

numSummary <- function(data, 
                       statistics=c("mean", "sd", "se(mean)", "var", "CV", "IQR", "quantiles", "skewness", "kurtosis"),
                       type=c("2", "1", "3"),
                       quantiles=c(0, .25, .5, .75, 1), groups){
    sd <- function(x, type, ...){
        apply(as.matrix(x), 2, stats::sd, na.rm=TRUE)
    }
    IQR <- function(x, type, ...){
        apply(as.matrix(x), 2, stats::IQR, na.rm=TRUE)
    }
    std.err.mean <- function(x, ...){
        x <- as.matrix(x)
        sd <- sd(x)
        n <- colSums(!is.na(x))
        sd/sqrt(n)
    }
    var <- function(x, type, ...){
        apply(as.matrix(x), 2, stats::var, na.rm=TRUE)
    }
    skewness <- function(x, type, ...){
        if (is.vector(x)) return(e1071::skewness(x, type=type, na.rm=TRUE))
        apply(x, 2, skewness, type=type)
    }
    kurtosis <- function(x, type, ...){
        if (is.vector(x)) return(e1071::kurtosis(x, type=type, na.rm=TRUE))
        apply(x, 2, kurtosis, type=type)
    }
    data <- as.data.frame(data)
    if (!missing(groups)) {
        groups <- as.factor(groups)
        counts <- table(groups)
        if (any(counts == 0)){
            levels <- levels(groups)
            warning("the following groups are empty: ", paste(levels[counts == 0], collapse=", "))
            groups <- factor(groups, levels=levels[counts != 0])
        }
    }
    variables <- names(data)
    if (missing(statistics)) statistics <- c("mean", "sd", "quantiles", "IQR")
    statistics <- match.arg(statistics, c("mean", "sd", "se(mean)", "var", "CV", "IQR", "quantiles", "skewness", "kurtosis"),
                            several.ok=TRUE)
    type <- match.arg(type)
    type <- as.numeric(type)
    ngroups <- if(missing(groups)) 1 else length(grps <- levels(groups))
    quantiles <- if ("quantiles" %in% statistics) quantiles else NULL
    if (anyDuplicated(quantiles)){
        warning("there are duplicated quantiles, which are ignored")
        quantiles <- sort(unique(quantiles))
    }
    quants <- if (length(quantiles) >= 1) paste(100*quantiles, "%", sep="") else NULL
    nquants <- length(quants)
    stats <- c(c("mean", "sd", "se(mean)", "var", "IQR", "CV", "skewness", "kurtosis")[c("mean", "sd", "se(mean)", "var", "IQR", "CV", "skewness", "kurtosis") %in% statistics], quants)
    nstats <- length(stats)
    nvars <- length(variables)
    result <- list()
    if ((ngroups == 1) && (nvars == 1) && (length(statistics) == 1)){
        if (statistics == "quantiles")
            table <- quantile(data[,variables], probs=quantiles, na.rm=TRUE)
        else {
            stats <- statistics
            stats[stats == "se(mean)"] <- "std.err.mean"
            table <- do.call(stats, list(x=data[,variables], na.rm=TRUE, type=type))
            names(table) <- statistics
        }
        NAs <- sum(is.na(data[,variables]))
        n <- nrow(data) - NAs
        result$type <- 1
    }
    else if ((ngroups > 1)  && (nvars == 1) && (length(statistics) == 1)){
        if (statistics == "quantiles"){
            table <- matrix(unlist(tapply(data[, variables], groups,
                                          quantile, probs=quantiles, na.rm=TRUE)), ngroups, nquants,
                            byrow=TRUE)
            rownames(table) <- grps
            colnames(table) <- quants
        }
        else table <- tapply(data[,variables], groups, statistics,
                             na.rm=TRUE, type=type)
        NAs <- tapply(data[, variables], groups, function(x)
            sum(is.na(x)))
        n <- table(groups) - NAs
        result$type <- 2
    }
    else if ((ngroups == 1) ){
        X <- as.matrix(data[, variables])
        table <- matrix(0, nvars, nstats)
        rownames(table) <- if (length(variables) > 1) variables else ""
        colnames(table) <- stats
        if ("mean" %in% stats) table[,"mean"] <- colMeans(X, na.rm=TRUE)
        if ("sd" %in% stats) table[,"sd"] <- sd(X)
        if ("se(mean)" %in% stats) table[, "se(mean)"] <- std.err.mean(X)
        if ("var" %in% stats) table[,"var"] <- var(X)
        if ("CV" %in% stats) table[,"CV"] <- CV(X)
        if ("IQR" %in% stats) table[, "IQR"] <- IQR(X)
        if ("skewness" %in% statistics) table[, "skewness"] <- skewness(X, type=type)
        if ("kurtosis" %in% statistics) table[, "kurtosis"] <- kurtosis(X, type=type)
        if ("quantiles" %in% statistics){
            table[,quants] <- t(apply(data[, variables, drop=FALSE], 2, quantile,
                                      probs=quantiles, na.rm=TRUE))
        }
        NAs <- colSums(is.na(data[, variables, drop=FALSE]))
        n <- nrow(data) - NAs
        result$type <- 3
    }
    else {
        table <- array(0, c(ngroups, nstats, nvars),
                       dimnames=list(Group=grps, Statistic=stats, Variable=variables))
        NAs <- matrix(0, nvars, ngroups)
        rownames(NAs) <- variables
        colnames(NAs) <- grps
        for (variable in variables){
            if ("mean" %in% stats)
                table[, "mean", variable] <- tapply(data[, variable],
                                                    groups, mean, na.rm=TRUE)
            if ("sd" %in% stats)
                table[, "sd", variable] <- tapply(data[, variable],
                                                  groups, sd, na.rm=TRUE)
            if ("se(mean)" %in% stats)
                table[, "se(mean)", variable] <- tapply(data[, variable],
                                                        groups, std.err.mean, na.rm=TRUE)
            if ("var" %in% stats)
                table[, "var", variable] <- tapply(data[, variable],
                                                   groups, var, na.rm=TRUE)
            if ("IQR" %in% stats)
                table[, "IQR", variable] <- tapply(data[, variable],
                                                   groups, IQR, na.rm=TRUE)
            if ("CV" %in% stats)
                table[, "CV", variable] <- tapply(data[, variable],
                                                  groups, CV, na.rm=TRUE)
            if ("skewness" %in% stats)
                table[, "skewness", variable] <- tapply(data[, variable],
                                                        groups, skewness, type=type)
            if ("kurtosis" %in% stats)
                table[, "kurtosis", variable] <- tapply(data[, variable],
                                                        groups, kurtosis, type=type)
            if ("quantiles" %in% statistics) {
                res <- matrix(unlist(tapply(data[, variable], groups,
                                            quantile, probs=quantiles, na.rm=TRUE)), ngroups, nquants,
                              byrow=TRUE)
                table[, quants, variable] <- res
            }
            NAs[variable,] <- tapply(data[, variable], groups, function(x)
                sum(is.na(x)))
        }
        if (nstats == 1) table <- table[,1,]
        if (nvars == 1) table <- table[,,1]
        n <- table(groups)
        n <- matrix(n, nrow=nrow(NAs), ncol=ncol(NAs), byrow=TRUE)
        n <- n - NAs
        result$type <- 4
    }
    result$table <- table
    result$statistics <- statistics
    result$n <- n
    if (any(NAs > 0)) result$NAs <- NAs
    class(result) <- "numSummary"
    result
}

# Matriz de datos da3240, del Bar?metro de octubre de 2009
anchos <- c(-9, 2, 2, -25, 1, -2, 1, -147, 1, 2)
nvar <- c('ccaa', 'prov', 'siteco', 'sitpol', 'sexo', 'edad')
d3240 <- read.fwf('DA3240', widths=anchos, col.names=nvar)
d3240[1:20,]; head(d3240) # La instrucci?n head saca por pantalla las primeras filas de la hoja de datos
names(d3240)
#---------------------------------------------------
# CONVERSI?N DE VARIABLES NUM?RICAS EN FACTORES
#---------------------------------------------------
b <- d3240
# Conversion de las variables en factores con bucle for
# (convertimos todas las variables excepto 'edad', que es la sexta y ?ltima)
for (i in 1:5) b[ , i] <- as.factor(b[ , i])
levels(b$siteco)
#---------------------------------------------
# MEDIDAS DE POSICI?N, DISPERSI?N Y FORMA
#---------------------------------------------
ed <- b$edad
ed <- ed[ed < 99]
#--------------
# Tarea 1
#--------------
ed2 <- ed

############# vemos el arreglo en R
print(ed2)

############# I calcular la media
media <- mean(ed2)
print(media)

############# II reemplazamos los valores de 31 a 50 con NA
lower_bound <- 31
upper_bound <- 50

ed2[ed2 >= lower_bound & ed2 <= upper_bound] <- NA
print(ed2)


############# III calcular media con parametro
media <- mean(ed2,na.rm=T)
print(media)


############# IV calcular error standard
# Calculate the standard deviation
standard_deviation <- sd(ed2)

# Calculate the standard error
standard_error <- standard_deviation / sqrt(length(data))

# Print the standard error
print(standard_error)

############# V numsummary

ed2num <- numSummary(ed2)
print(ed2num)

#-------------------------------------------------------------------
# IMPORTACI?N DE DATOS DESDE FICHEROS DE SPSS (Ficheros .sav)
#-------------------------------------------------------------------
library(foreign)
# Si queremos que los factores nos aparezcan con su literal
d <- read.spss('3240.sav', to.data.frame=T)
d[1:20, 1:10]

#--------------
# Tarea 2
#--------------

############# I listar nombres de variables

nombres_variables <- names(d)
print(nombres_variables)

############# II listar nombres de variables


freqMarginalCCAA <- table(d$CCAA)
print(freqMarginalCCAA)

freqMarginalP14 <- table(d$P14)
print(freqMarginalP14)

freqMarginalRECUERDO <- table(d$RECUERDO)
print(freqMarginalRECUERDO)

freqMarginalESTUDIOS <- table(d$ESTUDIOS)
print(freqMarginalESTUDIOS)

freqMarginalP27 <- table(d$P27)
print(freqMarginalP27)


#--------------
# Tarea 3
#--------------
