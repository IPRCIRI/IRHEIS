pkglist <- c("yaml","RODBC","readxl","tools","foreign","data.table",
             "stringr","XLConnect","sm","spatstat","ggplot2","haven",
             "rworldmap","rgdal","xlsx","Hmisc","reldist","dplyr","reshape2",
             "tcltk","zonator")

for(pkg in pkglist){
  if((eval(parse(text = paste0("require(",pkg,")")))==FALSE))
    install.packages(pkg)
}
