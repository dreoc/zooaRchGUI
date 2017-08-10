#sliderFiles <<- ""

clearSliderDot <- function(e) {
	e$sliderDot1 <- -1
	e$sliderDot2 <- -1
	e$sliderDotNum <- 0
}

sliderInit <- function(e) {
	clearSliderDot(e)
	#sliderFiles <<- ""
	e$sliderData <- list()
	e$sliderCurrImgId <- 1
}

sliderMainMenu <- function(e) {
    topMenu <- tkmenu(e$wnd)
      
    #File Menu
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
    tkadd(fileMenu, "command", label = "Open TPS file", command = function() openTpsFile(e))
    #tkadd(fileMenu, "command", label = "Open NTS file", command = function() openNtsFile())
    tkadd(fileMenu, "command", label = "Save to CSV",command = function() saveToCsv(e))
    tkadd(fileMenu, "command", label = "Exit", command = function() tkdestroy(e$wnd))     
	
	importMenu <- tkmenu(fileMenu, tearoff = FALSE)
	fileName <- paste(getwd(), "/sliderTpsFiles.txt", sep = "")	
	if(file.exists(fileName)) {
		sliderFiles <- scan(file = fileName, what = "char", sep = "\n", quiet = TRUE)
		for(i in 1:length(sliderFiles)) {
			tkadd(importMenu, "command", label = sliderFiles[i],command = function() importFile(e, i))	
		}
		e$sliderFiles <- sliderFiles
	}
	tkadd(fileMenu, "cascade", label = "Import", menu = importMenu)	
	tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
	e$sliderMenuTree <- topMenu
}

bindSliderCanvasEvent<-function(e, canvas) {
    tkitembind(canvas, "point", "<1>", function(x, y) {
	sliderOnDotSelect(e, x, y)})
	tkitembind(canvas, "point", "<3>", function(x, y) {
	itemRemove(e, x, y)})	
}

sliderGetLineIndex <- function(e, x, y) {
    tpsDataList <- e$activeDataList
    imgId <- e$currImgId
	coords <- tpsDataList[[imgId]][[3]]
    lineList <- tpsDataList[[imgId]][[8]]
	lineStatus <- tpsDataList[[1]][[9]]  

	if(length(lineList)) {
		for(i in 1:length(lineList)){ 		
			if(lineStatus[[i]] == "removed") {next}
			
			midDot <- lineList[[i]][[2]] 	
			cond1 <- (abs(as.integer(coords[[midDot]][1]) - as.integer(x)) <= 6)
			cond2 <- (abs(as.integer(coords[[midDot]][2]) - as.integer(y)) <= 6)
			if(cond1 && cond2) {
				return (i)			
			}                  
		}	
	}
	print(paste("not found line", x, y))
	return (0)
}

sliderDotCancel <- function(e, K) {
	if((e$tab == 1) && (K == "Escape")) {
		currImgId <- e$currImgId
		showDots(e)
		clearSliderDot(e)
	}	
}

sliderOnDotSelect <-function(e, x, y) {
	dotId <- getDotId(e, x,y)
	
	if(dotId != 0) {	
		canvas <- e$activeCanvas
		currImgId <- e$currImgId    
		tpsDataList <- e$activeDataList
		coords <- tpsDataList[[currImgId]][[3]] 
		dotStatus <- tpsDataList[[currImgId]][[5]]
		sliderDot1 <- e$sliderDot1
		sliderDotNum <- e$sliderDotNum
		
		x <- coords[[dotId]][1] 
		y <- coords[[dotId]][2] 
 
		if(sliderDotNum == 0) {			
			e$sliderDot1 <- dotId
			#change the color of the dot to "pink"
			changeCurrDotColor(e, x, y, "black", "pink")			
			e$sliderDotNum <- sliderDotNum+1		
		} else if(sliderDotNum == 1) {
			if(dotId == sliderDot1) {
				alertBox("Duplicate dot, invalid")
				return ()
			}			
		
			if(tpsDataList[[currImgId]][[5]][[dotId]] == "middle") {
				alertBox("It's middle, please reselect the second dot")
				return ()
			} 
		
			e$sliderDot2 <- dotId			
			#change the color of the dot to "pink"	
			changeCurrDotColor(e, x, y, "black", "pink")		
			e$sliderDotNum <- sliderDotNum+1			
		} else if(sliderDotNum == 2) {
			sliderDot2 <- e$sliderDot2
			if((dotId == sliderDot1)||(dotId == sliderDot2)) {
				alertBox("duplicate dot, invalid")
				return ()
			}
		
			#update dot status to "midle"
			for(i in 1:length(tpsDataList)){
				tpsDataList[[i]][[5]][[sliderDot2]] <- "middle"				
			}
			
			#draw line
			x1 <- coords[[sliderDot1]][1] 
			y1 <- coords[[sliderDot1]][2] 
			x2 <- coords[[sliderDot2]][1] 
			y2 <- coords[[sliderDot2]][2] 			
			item1 <- tkcreate(canvas, "line", x1, y1, x2, y2, width=2, fill="green")
			tkaddtag(canvas, "line", "withtag", item1)
			item2 <- tkcreate(canvas, "line", x2, y2, x, y, width=2, fill="green")
			tkaddtag(canvas, "line", "withtag", item2)
			
			#record line to tpsDataList
			for(i in 1:length(tpsDataList)) {
				sliderLine <- tpsDataList[[i]][[8]]
				sliderLine[[length(sliderLine)+1]] <- c(sliderDot1, sliderDot2, dotId, item1, item2)
				tpsDataList[[i]][[8]] <- sliderLine
			}
			
			#record line status to tpsDataList
			status <- tpsDataList[[1]][[9]]
			status[[length(status)+1]] <- "normal"			
			tpsDataList[[1]][[9]] <- status
			
			e$activeDataList <- tpsDataList
		
			#update line number
			nlines <- getLineNum(status)
			tkconfigure(e$sliderLineNumLabel, text = paste("n sliders =", nlines))
		
			showDots(e)
			#clear sliderDot
			clearSliderDot(e)
		}		
	}
}

changeCurrDotColor <- function(e, x, y, outline, fill) {
	canvas <- e$activeCanvas	
	tkdelete(canvas, "current")	
	item <- tkcreate(canvas, "oval", x-6, y-6, x+6, y+6, width=1, outline=outline, fill=fill)
    tkaddtag(canvas, "point", "withtag", item)
}

sliderRemoveLineOk <-function(e, x, y) {
	canvas <- e$activeCanvas
	item <- tkfind(canvas, "closest", x, y)
	
	#update line status
	currImgId <- e$currImgId
	tpsDataList <- e$activeDataList
	#myPrint(paste(currImgId, "line", item, "is to be deleted"))
	lineNumText <- ""

	index <- sliderGetLineIndex(e, x, y)
	if(index != 0) {
		#delete the lines among the tree dots
		lineList <- tpsDataList[[currImgId]][[8]]
		item1 <- lineList[[index]][[4]]
		item2 <- lineList[[index]][[5]]
		tkdelete(canvas, item1)
		tkdelete(canvas, item2)
		
		#update slider line status
		lineStatus <- tpsDataList[[1]][[9]]
		lineStatus[[index]] <- "removed"
		tpsDataList[[1]][[9]] <- lineStatus
		
		#update the second dot status to 'normal'			
		dotId <- lineList[[index]][[2]]
		for(i in 1:length(tpsDataList)) {
			dotStatus <- tpsDataList[[i]][[5]]
			dotStatus[[dotId]] <- "normal"
			tpsDataList[[i]][[5]] <- dotStatus
		}
		
		e$activeDataList <- tpsDataList
		showDots(e)	
		
		lineNumText <- "n sliders:"
	}	
	

	#update line numbers
	if(lineNumText != "") {
		nlines <- getLineNum(tpsDataList[[1]][[9]])
		tkconfigure(e$sliderLineNumLabel, text = paste(lineNumText, nlines))
	}
	
	win <- e$removeWin
	tkdestroy(win)	
}