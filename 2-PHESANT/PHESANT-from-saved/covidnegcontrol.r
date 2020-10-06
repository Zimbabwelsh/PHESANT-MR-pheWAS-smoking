library(data.table)
library(dplyr)
library(parallel)

cneg <- fread("~/cpos33352.csv", he=T)
cpos <- fread("~/cpos33352.csv", he=T)

phenpath <- "~/data/UKB/15825/2019-05-02/derived"
phens <- list.files(phenpath)

phenout <- mclapply(phens, function(x)
{
      message(x)
      fn <- file.path(phenpath,x,"phen.txt")
      file.exists(fn)
      a <- fread(fn)
      a <- merge(cpos, a, by="eid")
      setnames(a, "eid", "cpos", "phen")
      form <- paste0("cpos ~ phen")
      return(try(summary(glm(form, a, family="binomial"))$coefficients))
}, mc.cores=6)

names(phenout) <- phens

organise <- function(l)
{
        nom <- names(l)
        lapply(nom, function(x)
        {
            l[[x]] %>% as.data.frame() %>% mutate(exposure=x) %>% {.[2,]}
        }) %>% bind_rows()
}

organise(phenout)