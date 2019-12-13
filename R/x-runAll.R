rfiles <- dir(pattern = glob2rx("*.R"))
rfiles
for(f in rfiles[17:21])
  source(f)
