#linkFiles <<- ""
#startDot <- -1

linkInit <- function(wnd) {
	assign("linkData", list(), envir = .GlobalEnv)
	assign("linkCurrImgId", 1, envir = .GlobalEnv)
	assign("startDot", -1, envir = .GlobalEnv)
}

linkMainMenu <- function(wnd) {
#print("linkMainMenu")
    topMenu <- tkmenu(wnd)
    
    #File Menu
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
    tkadd(fileMenu, "command", label = "Open TPS file", command = function() openTpsFile())
    tkadd(fileMenu, "command", label = "Open NTS file", command = function() openNtsFile())
    tkadd(fileMenu, "command", label = "Save to CSV",command = function() saveToCsv())
    tkadd(fileMenu, "command", label = "Exit", command = function() tkdestroy(wnd))     
	
	linkImportMenu <- tkmenu(fileMenu, tearoff = FALSE)
	linkFileName <- paste(getwd(), "/linkTpsFiles.txt", sep = "")
	if(file.exists(linkFileName)) {
		linkFiles <<- scan(file = linkFileName, what = "char", sep = "\n", quiet = TRUE)
		for(i in 1:length(linkFiles)) {			
			tkadd(linkImportMenu, "command", label = linkFiles[i],command = function() importFile(i))	
		}	
	}
	
	tkadd(fileMenu, "cascade", label = "Import", menu = linkImportMenu)	
	tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
	assign("linkMenuTree", topMenu, envir = .GlobalEnv)
}

bindLinkCanvasEvent<-function(canvas) {
    tkitembind(canvas, "point", "<1>", linkLineStart)
    tkbind(canvas, "<B1-Motion>", linkLineCont)
    tkitembind(canvas, "point", "<ButtonRelease>", linkLineEnd)
	tkitembind(canvas, "line", "<3>", itemRemove)
	tkitembind(canvas, "line", "<Enter>", function() {
		canvas <- get("activeCanvas", envir = .GlobalEnv)
		tkdtag(canvas, "selected")
		tkaddtag(canvas, "selected", "withtag", "current")
	})
}

linkGetLineIndex <-function(imgId, item) {
	myPrint(paste("get line id for", item))
    tpsDataList <- get("activeDataList", envir = .GlobalEnv)    
    lineList <- tpsDataList[[imgId]][[8]]
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

linkLineStart <- function(x, y) {
	x <- as.numeric(x)
	y <- as.numeric(y)	
	
	dotId <- getDotId(x, y)
	if(dotId != 0) {	
		#startDot <<- dotId	
		assign("startDot", dotId, envir = .GlobalEnv)		
		
		canvas <- get("activeCanvas", envir = .GlobalEnv)
		tkdtag(canvas, "selected")
		tkaddtag(canvas, "selected", "withtag", "current")
		tkitemraise(canvas,"current")	
	}
}

linkLineEnd<-function(x, y) {
	activeCanvas <- get("activeCanvas", envir = .GlobalEnv)
	tkdelete(activeCanvas, "tmpline")
		
	dotId <- getDotId(x, y)
	startDot <- get("startDot", envir = .GlobalEnv)
	
	if((dotId != 0) && (dotId != startDot)) {	
		tpsDataList <- get("activeDataList", envir = .GlobalEnv)
		currImgId <- get("currImgId", envir = .GlobalEnv)
		coords <- tpsDataList[[currImgId]][[3]] 
		
		#draw line
		x1 <- coords[[startDot]][1]
		y1 <- coords[[startDot]][2]
		x2 <- coords[[dotId]][1] 
		y2 <- coords[[dotId]][2] 
		item <- tkcreate(activeCanvas, "line", x1, y1, x2, y2, width=2, fill="green")
		
		tkaddtag(activeCanvas, "line", "withtag", item)	
		
		#record line and line status to tpsDataList
		for(i in 1:length(tpsDataList)) {
			lineList <- tpsDataList[[i]][[8]]
			lineList[[length(lineList)+1]] <- c(startDot, dotId, item)
			tpsDataList[[i]][[8]] <- lineList
		}
		
		lineStatus <- tpsDataList[[1]][[9]]
		lineStatus[[length(lineStatus)+1]] <- "normal"		
		tpsDataList[[1]][[9]] <- lineStatus
		assign("activeDataList", tpsDataList, envir = .GlobalEnv)
		
		#update line number
		lineNumLabel <- get("linkLineNumLabel", envir = .GlobalEnv)
		nlines <- getLineNum(lineStatus)
		tkconfigure(lineNumLabel, text = paste("n links =", nlines))
	
		#clear variable
		assign("startDot", -1, envir = .GlobalEnv)
	}
}

linkLineCont<-function(x, y) {
	x <- as.numeric(x)
	y <- as.numeric(y)
	
	startDot <- get("startDot", envir = .GlobalEnv)
	
	if(startDot != -1) {
		activeCanvas <- get("activeCanvas", envir = .GlobalEnv)
		tkdelete(activeCanvas, "tmpline")

		tpsDataList <- get("activeDataList", envir = .GlobalEnv)
		currImgId <- get("currImgId", envir = .GlobalEnv)
		coords <- tpsDataList[[currImgId]][[3]] 
		x1 <- coords[[startDot]][1]
		y1 <- coords[[startDot]][2]
		item <- tkcreate(activeCanvas, "line", x1, y1, x, y, width=1, fill="green")
		tkaddtag(activeCanvas, "tmpline", "withtag", item)
	}  
}

linkRemoveLineOk <-function(x, y) {
	#print("linkRemoveLineOk")
	canvas <- get("activeCanvas", envir = .GlobalEnv)
	item <- tkfind(canvas, "closest", x, y)
	
	#update line status
	currImgId <- get("currImgId", envir = .GlobalEnv)
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
	myPrint(paste(currImgId, "line", item, "is to be deleted"))
	lineNumText <- ""

	index <- linkGetLineIndex(currImgId, item)	
	if(index != 0) {
		tkdelete(canvas, item)			
		lineStatus <- tpsDataList[[1]][[9]]
		lineStatus[[index]] <- "removed"
		tpsDataList[[1]][[9]] <- lineStatus
		assign("activeDataList", tpsDataList, envir = .GlobalEnv)
		lineNumText <- "n links:"
	}		
	
	#update line numbers
	if(lineNumText != "") {
		lineNumLabel <- get("linkLineNumLabel", envir = .GlobalEnv)
		nlines <- getLineNum(tpsDataList[[1]][[9]])
		tkconfigure(lineNumLabel, text = paste(lineNumText, nlines))
	}
	
	win <- get("removeWin", envir = .GlobalEnv) 
	tkdestroy(win)	
}