
# pointType - 0 = normal; 2 = jittered
# Add data points for specified columns in 'jittered' arrangement
jittered.points<-function(D, myHorizontal=FALSE, pointType, pointColors){

	# normal boxplots
	if(myHorizontal){
		for (i in c(1:ncol(D))){
			if(pointType==0){
				points(D[,i], rep(i, nrow(D)), col=pointColors[i]) 			
			} else {
				points(D[,i], jitter(rep(i, nrow(D)), 3), col=pointColors[i]) 
			}
		}
	} else {
		# horizontal boxplots
		for (i in c(1:ncol(D))){
			if(pointType==0){
				points(rep(i, nrow(D)), D[,i], col=pointColors[i])					
			} else {
				points(jitter(rep(i, nrow(D)), 3), D[,i], col=pointColors[i])
			}
		}
	}
}
