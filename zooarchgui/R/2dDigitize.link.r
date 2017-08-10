#linkFiles <<- ""
#startDot <- -1

linkInit <- function(e) {
	e$linkData <- list()
	e$linkCurrImgId <- 1
	e$startDot <- 1
}

linkMainMenu <- function(e) {
#print("linkMainMenu")
    topMenu <- tkmenu(e$wnd)
    
    #File Menu
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
    tkadd(fileMenu, "command", label = "Open TPS file", command = function() openTpsFile(e))
    #tkadd(fileMenu, "command", label = "Open NTS file", command = function() openNtsFile())
    tkadd(fileMenu, "command", label = "Save to CSV",command = function() saveToCsv(e))
    tkadd(fileMenu, "command", label = "Exit", command = function() tkdestroy(e$wnd))     
	
	linkImportMenu <- tkmenu(fileMenu, tearoff = FALSE)
	linkFileName <- paste(getwd(), "/linkTpsFiles.txt", sep = "")
	if(file.exists(linkFileName)) {
		linkFiles <- scan(file = linkFileName, what = "char", sep = "\n", quiet = TRUE)
		for(i in 1:length(linkFiles)) {			
			tkadd(linkImportMenu, "command", label = linkFiles[i],command = function() importFile(e, i))	
		}	
		e$linkFiles <- linkFiles
	}
	
	tkadd(fileMenu, "cascade", label = "Import", menu = linkImportMenu)	
	tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
	e$linkMenuTree <- topMenu
}

bindLinkCanvasEvent<-function(e, canvas) {
    tkitembind(canvas, "point", "<1>", function(x, y) {
	linkLineStart(e, x, y)})
    tkbind(canvas, "<B1-Motion>", function(x, y) {
	linkLineCont(e, x, y)})
    tkitembind(canvas, "point", "<ButtonRelease>", function(x, y) {linkLineEnd(e, x, y)})
	tkitembind(canvas, "line", "<3>", function(x, y) {
	itemRemove(e, x, y)})
	tkitembind(canvas, "line", "<Enter>", function() {
		canvas <- e$activeCanvas
		tkdtag(canvas, "selected")
		tkaddtag(canvas, "selected", "withtag", "current")
	})
}

linkGetLineIndex <-function(e, item) {
	#print(paste("get line id for", item))
    tpsDataList <- e$activeDataList  
    lineList <- tpsDataList[[e$currImgId]][[8]]
	lineStatus <- tpsDataList[[1]][[9]]  

	if(length(lineList)) {
		for(i in 1:length(lineList)){ 
			#myPrint(paste(imgId, i, lineList[[i]][[3]]))
			if(lineStatus[[i]] == "removed") {next}			
			if(tclvalue(lineList[[i]][[3]]) == tclvalue(item)) { 				
				return (i)
			}                     
		}	
	}
	print(paste("not found line", item))
	return (0)
}

linkLineStart <- function(e, x, y) {
#print("linkLineStart")
	x <- as.numeric(x)
	y <- as.numeric(y)	
	
	dotId <- getDotId(e, x, y)
	if(dotId != 0) {	
		#startDot <<- dotId	
		e$startDot <- dotId
		
		
		canvas <- get("activeCanvas", envir = e)
		tkdtag(canvas, "selected")
		tkaddtag(canvas, "selected", "withtag", "current")
		tkitemraise(canvas,"current")	
	}
}

linkLineEnd<-function(e, x, y) {

	canvas <- e$activeCanvas
	tkdelete(canvas, "tmpline")
		
	dotId <- getDotId(e, x, y)
	startDot <- e$startDot

	if(dotId != 0) {
		if(dotId == startDot) {
			e$startDot <- -1
			return ()
		}
		tpsDataList <- e$activeDataList
		imgId <- e$currImgId
		coords <- tpsDataList[[imgId]][[3]] 
		
		#draw line
		x1 <- coords[[startDot]][1]
		y1 <- coords[[startDot]][2]
		x2 <- coords[[dotId]][1] 
		y2 <- coords[[dotId]][2] 
		item <- tkcreate(canvas, "line", x1, y1, x2, y2, width=2, fill="green")
		tkaddtag(canvas, "line", "withtag", item)			

		#record line and line status to tpsDataList
		for(i in 1:length(tpsDataList)) {
			lineList <- tpsDataList[[i]][[8]]
			lineList[[length(lineList)+1]] <- c(startDot, dotId, item)
			tpsDataList[[i]][[8]] <- lineList
		}
	
		lineStatus <- tpsDataList[[1]][[9]]
		lineStatus[[length(lineStatus)+1]] <- "normal"		
		tpsDataList[[1]][[9]] <- lineStatus
		e$activeDataList <- tpsDataList
		
		#update line number
		nlines <- getLineNum(lineStatus)
		tkconfigure(e$linkLineNumLabel, text = paste("n links =", nlines))
	
		#clear variable
		e$startDot <- -1		
	}
}
		
linkLineCont<-function(e, x, y) {
	tpsDataList <- e$activeDataList
	if(length(tpsDataList) <= 0) {
		return ()
	}
	
	startDot <- e$startDot
	
	if(startDot != -1) {
		x <- as.numeric(x)
		y <- as.numeric(y)
		canvas <- get("activeCanvas", envir = e)
		tkdelete(canvas, "tmpline")
		currImgId <- e$currImgId
		coords <- tpsDataList[[currImgId]][[3]] 
		x1 <- coords[[startDot]][1]
		y1 <- coords[[startDot]][2]
		item <- tkcreate(canvas, "line", x1, y1, x, y, width=1, fill="green")
		tkaddtag(canvas, "tmpline", "withtag", item)
	}  
}

linkRemoveLineOk <-function(e, x, y) {
	#print("linkRemoveLineOk")
	canvas <- e$activeCanvas
	item <- tkfind(canvas, "closest", x, y)
	
	#update line status
	currImgId <- e$currImgId
	tpsDataList <- e$activeDataList
	#myPrint(paste(currImgId, "line", item, "is to be deleted"))
	lineNumText <- ""

	index <- linkGetLineIndex(e, item)	
	if(index != 0) {
		tkdelete(canvas, item)			
		lineStatus <- tpsDataList[[1]][[9]]
		lineStatus[[index]] <- "removed"
		tpsDataList[[1]][[9]] <- lineStatus
		e$activeDataList <- tpsDataList
		lineNumText <- "n links:"
	}		
	
	#update line numbers
	if(lineNumText != "") {
		nlines <- getLineNum(tpsDataList[[1]][[9]])
		tkconfigure(e$linkLineNumLabel, text = paste(lineNumText, nlines))
	}
	
	win <- e$removeWin
	tkdestroy(win)	
}