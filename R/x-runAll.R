rfiles <- dir(pattern = glob2rx("*.R"))
rfiles
for(f in rfiles[6:21])
  source(f)