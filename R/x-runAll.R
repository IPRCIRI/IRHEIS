rfiles <- dir(pattern = glob2rx("*.R"))
rfiles
for(f in rfiles[15:21])
  source(f)
