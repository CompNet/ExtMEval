
get.input.folder.path = function(n, k, h, transf.type, subset.prop){
	return(
		file.path(
			IN.FOLDER,
			paste0("n=",n,"_k=",k,"_h=",sprintf("%.4f",h),"_t=",transf.type,"_p=",sprintf("%.4f",subset.prop))
		)
	)
}


get.part.folder.path = function(n, k, h, transf.type, subset.prop){
	return(
		file.path(
			PART.FOLDER, 
			paste0("n=",n,"_k=",k,"_h=",sprintf("%.4f",h),"_t=",transf.type,"_p=",sprintf("%.4f",subset.prop))
		) 
	)
}


get.eval.folder.path = function(n, k, h, transf.type, subset.prop){
	return(	
		file.path(
			EVAL.FOLDER, 
			paste0("n=",n,"_k=",k,"_h=",sprintf("%.4f",h),"_t=",transf.type,"_p=",sprintf("%.4f",subset.prop))
		) 
	)
}


#get.data.frame.folder.path = function(){
#	return(	
#		#file.path(
#			DATA.FRAME.FOLDER
#		#) 
#	)
#}



