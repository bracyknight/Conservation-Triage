#assignment script

assign(paste(model, "survived", sep="."), data.frame(survived)) # mX.XXX.survived
assign(paste(model, "survstats", sep="."), data.frame(survstats))
assign(paste(model, "diversity", sep="."), data.frame(diversity))
assign(paste(model, "divstats", sep="."), data.frame(divstats))
assign(paste(model, "extinct", sep="."), data.frame(extinct))
assign(paste(model, "exstats", sep="."), data.frame(exstats))
assign(paste(model, "habitats", sep="."), data.frame(habitats))
assign(paste(model, "habstats", sep="."), data.frame(habstats))
assign(paste(model, "af.habitats", sep="."), data.frame(af.habitats))
assign(paste(model, "af.habstats", sep="."), data.frame(af.habstats))
assign(paste(model, "as.habstats", sep="."), data.frame(as.habstats))
assign(paste(model, "ma.habitats", sep="."), data.frame(ma.habitats))
assign(paste(model, "ma.habstats", sep="."), data.frame(ma.habstats))
assign(paste(model, "ne.habitats", sep="."), data.frame(ne.habitats))
assign(paste(model, "ne.habstats", sep="."), data.frame(ne.habstats))
assign(paste(model, "sp.results", sep="."), data.frame(sp.results))
assign(paste(model, "rarestats", sep="."), data.frame(rarestats))
assign(paste(model, "changestats", sep="."), data.frame(changestats))

