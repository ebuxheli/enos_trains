##outgraphic_single.r
###create ouptput plots
####RdE November 2012
	
# ####create output
output.vars = c('numberim','Remain','Englishlan')
var.names = c('Number of immigrants be increased?','Children of undocumented be allowed to stay?','English as official language?')

##graph presets
os = .4
line.os = .015
y.point = .75
ylims = c(0,1.1)
xlims = c(-.35,.35)
points.cex = 4
lab.cex = 1.5
line.lwd = 4.5
axis.cex = 1.25

colors = brewer.pal(3,'Paired')[1:2] ##colors for pairs used in plots below

# ####dose graph
# pdf('dose_combined.pdf',
# 	width = 6.5, height = 4
# 	)

par(mfrow = c(3,1)) 
par(mar = c(5,0,1,0))
par(bty = 'n')

	
##dose response graph
out.mat = final.mat.dose[,c('variable','subset','ate','quantile.lower','quantile.upper')]
out.mat$ate = as.numeric(as.character(out.mat$ate))
out.mat$quantile.lower = as.numeric(as.character(out.mat$quantile.lower))
out.mat$quantile.upper = as.numeric(as.character(out.mat$quantile.upper))

out.mat.ta = out.mat[out.mat$subset == 'ta'&out.mat$variable %in% output.vars,]
out.mat.tb = out.mat[out.mat$subset == 'tb'&out.mat$variable %in% output.vars,]

for(i in 1:length(var.names)){
	plot(x  = out.mat.ta$ate[i], y = y.point, 
		xlim = xlims,
		ylim = ylims,
		ylab = '',
		xlab = var.names[i],
		yaxt = 'n',
		type = 'n',
		cex.lab = lab.cex,
		cex.axis = axis.cex)
	lines(x = c(out.mat.ta$quantile.lower[i],out.mat.ta$ate[i]-line.os), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)
	lines(x = c(out.mat.ta$ate[i]+line.os,out.mat.ta$quantile.upper[i]), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)
	lines(x = c(out.mat.tb$quantile.lower[i],out.mat.tb$ate[i]-line.os), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)
	lines(x = c(out.mat.tb$ate[i]+line.os,out.mat.tb$quantile.upper[i]), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)

	points(x  = out.mat.ta$ate[i], y = y.point,
		pch = 19,
		cex = points.cex,
		col = colors[1])
	points(x  = out.mat.tb$ate[i], y = y.point - os,
		pch = 1,
		cex = points.cex,
		col = colors[2])
			}
dev.off()