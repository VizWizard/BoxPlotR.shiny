
# pointType - 0 = normal; 2 = jittered
# Add data points for specified columns in 'jittered' arrangement
jittered.points<-function(D, myHorizontal=FALSE, pointType, pointColors, pointTransparency, pointSize){
	# normal boxplots
	if(myHorizontal){
		for (i in c(1:ncol(D))){
			if(pointType==0){
				points(D[,i], rep(i, nrow(D)), col=rgb(t(col2rgb(pointColors[i])), max=255, alpha=255*(pointTransparency/100)), pch=16, cex=pointSize)
			} else {
				points(D[,i], jitter(rep(i, nrow(D)), amount=0.25), col=rgb(t(col2rgb(pointColors[i])), max=255, alpha=255*(pointTransparency/100)), pch=16, cex=pointSize)
			}
		}
	} else {
		# horizontal boxplots
		for (i in c(1:ncol(D))){
			if(pointType==0){
				points(rep(i, nrow(D)), D[,i], col=rgb(t(col2rgb(pointColors[i])), max=255, alpha=255*(pointTransparency/100)), pch=16, cex=pointSize)
			} else {
				points(jitter(rep(i, nrow(D)), amount=0.25), D[,i], col=rgb(t(col2rgb(pointColors[i])), max=255, alpha=255*(pointTransparency/100)), pch=16, cex=pointSize)
			}
		}
	}
}
