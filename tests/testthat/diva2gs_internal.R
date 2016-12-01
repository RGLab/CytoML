library(flowWorkspace)
library(XML)
library(plyr)
ws <- openDiva(system.file('extdata/diva/PE_2.xml', package = "CytoML"))

getSampleGroups(ws)
getSamples(ws)
gs <- parseWorkspace(ws, name = 2, subset = 1)
sampleNames(gs)
getNodes(gs)
plot(gs)
plotGate(gs[[1]])

# fr <- read.FCS(file.path(path, "120897.fcs"))
# densityplot(~., fr)
# kw <- keyword(fr)
# comp <- kw[["SPILL"]]
# param <- colnames(comp)
# trans <- estimateLogicle(fr, param)
# fr_comp <- compensate(fr, comp)
# grid.arrange(
# xyplot(`Bd Horizon V450-A`~`Pacific Orange-A`, transform(fr, trans), smooth = F, xbin = 32)
# , xyplot(`Bd Horizon V450-A`~`Pacific Orange-A`, transform(fr_comp, trans), smooth = F, xbin = 32)
# )
#
#
#
