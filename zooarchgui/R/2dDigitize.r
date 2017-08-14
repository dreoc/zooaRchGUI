
isDebug <- 0
################# main data structure ##############################
#2d digitize/link/slider
#tpsDataList[imgId][[1]]: specimen image dir
#tpsDataList[imgId][[2]]: scale factor
#tpsDataList[imgId][[3]]: landmarks
#tpsDataList[imgId][[4]]: sclae units
#tpsDataList[imgId][[5]]: landmark status ('normal', 'black', 'removed')
#tpsDataList[imgId][[6]]: ratio
#tpsDataList[imgId][[7]]: c(fitImgW, fitImgH, imgW, imgH)

#link
#tpsDataList[imgId][[8]]: link line coordinates list(c(dot1, dot2, line))
#tpsDataList[1][[9]]: line status ('normal', 'removed')

#slider
#tpsDataList[imgId][[8]]: slider line coordinates list(c(dot1, dot2, dot3, line1, line2))
#tpsDataList[1][[9]]: line status ('normal', 'removed')


switchTab <- function(e, W, x, y) {
	id <- tcl(W, "identify", "tab", x, y)

	#record activeDataList and current image id
	activeDataList <- e$activeDataList
	tab <- e$tab
	if(tab == 0) {
		e$digData <- activeDataList
		e$dotCurrImgId <- e$currImgId
	}else if (tab == 1) {
		e$linkData <- activeDataList
		e$linkCurrImgId <- e$currImgId
	}else if (tab == 2) {
		e$sliderData <- activeDataList
		e$sliderCurrImgId <- e$currImgId
	}

	#update canvas and activeDataList
	if (tclvalue(id) == 0) {
		e$tab <- 0
		menuTree <- e$dotMenuTree
		canvas <- e$digCanvas
		dataList <- e$digData
		currImgId <- e$dotCurrImgId
	} else if (tclvalue(id) == 1) {
		e$tab <- 1
		canvas <- e$linkCanvas
		dataList <- e$linkData
		currImgId <- e$linkCurrImgId
		menuTree <- e$linkMenuTree
	}else if (tclvalue(id) == 2) {
		e$tab <- 2
		canvas <- e$sliderCanvas
		dataList <- e$sliderData
		currImgId <- e$sliderCurrImgId
		menuTree <- e$sliderMenuTree
	}

	#switch the Menu
	wnd <- e$wnd
	tkconfigure(wnd, menu = menuTree)

	e$activeCanvas <- canvas
	e$activeDataList <- dataList
	e$currImgId <- currImgId
}

mainFrame <-function(e) {
	#print("mainFrame")
	tn <- ttknotebook(e$wnd)

	tkbind(tn, '<Button-1>', function(W, x, y) {
	switchTab(e, W, x, y)})
	tkbind(e$wnd, "<Key>", function(K) {
	sliderDotCancel(e, K)})

	sliderFrame <- createFrame(e, tn, "slider")
	linkFrame <- createFrame(e, tn, "link")
	digitizeFrame <- createFrame(e, tn, "digitize")

	tkadd(tn,digitizeFrame,text="2dDigitize")
	tkadd(tn,linkFrame,text="link")
	tkadd(tn,sliderFrame,text="Slider")

	tkpack(tn)
}

createFrame <- function(e, parent, id = "") {
#print("createFrame")
	myFrame <- ttkframe(parent)
	displayFrame <- ttkframe(myFrame)
	imgFrame <- ttkframe(displayFrame)
	if(id == "digitize") {
		ctlFrame <- digCreateCtlFrame(e, displayFrame)
	}else if (id == "link"){
		ctlFrame <- createCtlFrame(e, displayFrame, "link")
	}else if(id == "slider") {
		ctlFrame <- createCtlFrame(e, displayFrame, "slider")
	}
	tkpack(displayFrame)
	sapply(list(imgFrame, ctlFrame), tkpack, side = "left", padx = 6)

	canvas <- tkcanvas(imgFrame, relief="raised", width=800, height=600, background='white')
	tkpack(canvas, side="top", fill="x")

	btnFrame <- createBtnFrame(e, myFrame)
	tkpack(btnFrame)

	tkpack(myFrame)

	if(id == "digitize") {
		bindDigCanvasEvent(e, canvas)
		e$digCanvas <- canvas
		e$activeCanvas <- canvas
	}else if(id == "link"){
		bindLinkCanvasEvent(e, canvas)
		e$linkCanvas <- canvas
	}else {
		bindSliderCanvasEvent(e, canvas)
		e$sliderCanvas <- canvas
	}

	return (myFrame)
}

createBtnFrame <- function(e, parent) {
	btnFrame <- ttkframe(parent)
	prevBtn <- ttkbutton(btnFrame, text = "< Previous", command = function() onPrevious(e))
    nextBtn <- ttkbutton(btnFrame, text = "Next >",command = function() onNext(e))

    tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "both", side = "left")
    sapply(list(prevBtn, nextBtn), tkpack, side = "left", padx = 6)
	tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "both", side = "left")

	return (btnFrame)
}

createCtlFrame <-function(e, parent, id = "") {
	ctlFrame <- ttkframe(parent)
	path <- ttklabel(ctlFrame, text = "Specimen Id: NA")
	tkpack(path)

	lineNumLabel <- ttklabel(ctlFrame, text = "n links: 0")
	tkpack(lineNumLabel)

	if(id == "slider") {
		e$sliderImgPath <- path
		e$sliderLineNumLabel <- lineNumLabel
	}else if(id == "link") {
		e$linkImgPath <- path
		e$linkLineNumLabel <- lineNumLabel
	}

	return (ctlFrame)
}

alertBox <- function(msg) {
	win <- tktoplevel(height=100, width=180)
	tkwm.title(win, "Alert")
	label = tklabel(win, text=msg)
	okBtn <- ttkbutton(win, text = "OK", command = function() tkdestroy(win))
	sapply(list(label, okBtn), tkpack, padx = 6, pady = 16)
}

getLineNum <- function(lineStatus) {
	len <- 0
	if(length(lineStatus)) {
		for(i in 1:length(lineStatus)) {
			if(lineStatus[[i]] != "removed") {len = len+1}
		}
	}
	return (len)
}

itemRemove <-function(e, x, y) {
	if(e$tab == 1) {
		popUpRemoveWindow(e, x, y, 'Do you want to delete this line?', "linkLine")
	}else if(e$tab ==2) {
		popUpRemoveWindow(e, x, y, 'Do you want to delete this line?', "sliderLine")
	}
}

popUpRemoveWindow <- function(e, x, y, msg, item) {
	win <- tktoplevel()

	label = tklabel(win, text=msg)
	tkpack(label, fill = "x", padx = 5, pady = 5)

	btnFrame <- ttkframe(win)
	tkpack(btnFrame, fill = "x", padx = 5, pady = 5)
	cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() tkdestroy(win))
	if(item == "digdot") {
		okBtn <- ttkbutton(btnFrame, text = "ok",command = function() digRemoveDotOk(e, x, y))
	}else if(item == "linkLine") {
		okBtn <- ttkbutton(btnFrame, text = "ok",command = function() linkRemoveLineOk(e, x, y))
	}else if(item == "sliderLine") {
		okBtn <- ttkbutton(btnFrame, text = "ok",command = function() sliderRemoveLineOk(e, x, y))
	}

	tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "y", side = "left")
	sapply(list(cancelBtn, okBtn), tkpack, side = "left", padx = 6)

	e$removeWin <- win
	tkfocus(win)
}

showLines <- function(e) {
	#print("begin showLines")
    tpsDataList <- e$activeDataList
	imgId <- e$currImgId
	coords <- tpsDataList[[imgId]][[3]]
    lineList <- tpsDataList[[imgId]][[8]]
	status <- tpsDataList[[1]][[9]]

	canvas <- e$activeCanvas
	tkdelete(canvas, "line")
	nlines <- 0	
	tab <- e$tab
	
    if(length(lineList)) {
        for(i in 1:length(status)){
            if(status[[i]] == "removed") {next}

			nlines <- nlines +1

            dot1 <- as.integer(lineList[[i]][1])
			dot2 <- as.integer(lineList[[i]][2])
            x1 <- coords[[dot1]][1]
            y1 <- coords[[dot1]][2]
			x2 <- coords[[dot2]][1]
            y2 <- coords[[dot2]][2]

			item <- tkcreate(canvas, "line", x1, y1, x2, y2, width=2, fill="green")
			tkaddtag(canvas, "line", "withtag", item)
			#print(paste(imgId,"showLines: draw line", item, dot1, dot2))
			if(tab == 1) {
				tpsDataList[[imgId]][[8]][[i]] <- c(dot1, dot2, item)
			} else if(tab == 2) {
				dot3 <- as.integer(lineList[[i]][3])
				x3 <- coords[[dot3]][1]
				y3 <- coords[[dot3]][2]
				item2 <- tkcreate(canvas, "line", x2, y2, x3, y3, width=2, fill="green")
				tkaddtag(canvas, "line", "withtag", item)
				tpsDataList[[imgId]][[8]][[i]] <- c(dot1, dot2, dot3, item, item2)
			}
        }

		e$activeDataList <- tpsDataList

		if(tab == 1) {
			tkconfigure(e$linkLineNumLabel, text = paste("n links =", nlines))
		}else if(tab == 2) {
			tkconfigure(e$sliderLineNumLabel, text = paste("n sliders =", nlines))
		}
    }
}

showDots <- function(e) {
	#print("begin showDots")
	imgId <- e$currImgId
    tpsDataList <- e$activeDataList
    coords <- tpsDataList[[imgId]][[3]]
	statusList <- tpsDataList[[imgId]][[5]]

	canvas <- e$activeCanvas
	tkdelete(canvas, "point")
    if(length(coords)) {
        for(i in 1:length(statusList)){
            if(statusList[[i]] == "removed") {next}

            x <- as.integer(coords[[i]][1])
            y <- as.integer(coords[[i]][2])

			if(statusList[[i]] == "black") {

				fill <- "black"
				outline <- "red"
				if((x == -1) && (y == -1)) { next }
			} else if (statusList[[i]] == "middle") {

				fill <- "white"
				outline <- "red"
			}else {

				fill <- "red"
				outline <- "black"
			}
            item <- tkcreate(canvas, "oval", x - 6, y - 6, x + 6, y + 6,
                         width=1, outline=outline,
                         fill=fill)
            tkaddtag(canvas, "point", "withtag", item)

			dotLabel <- tkcreate(canvas, "text", x + 12, y, text=i, fill="red", font=c("Helvetica", 12))
			tkaddtag(canvas, "label", "withtag", dotLabel)
        }
    }
}

getRatio <- function(specimen=NULL, attrs=NULL) {
	#print("getRatio")
	ratio <- 0
	if(!is.null(specimen)) {
		if(!file.exists(specimen)) {
			print(paste("File does not exists:", specimen, "Ignore it!!"))
			return (c(0, 0, 0))
		}
		
		img <- tclVar()
		tcl('image', 'create', 'photo', img, file=specimen)
		height <- as.integer(tcl('image', 'height', img))
		width <- as.integer(tcl('image', 'width', img))	
	} else if(!is.null(attrs)) {
		width <- attrs[1]
		height <- attrs[2]	
	}

	if(height/600 > 1) {
		ratio <- as.integer(height/600)
		fitCanvasH <- height/ratio
		fitCanvasW <- width/ratio
	}else {
		ratio <- as.integer(600/height)
		#zoom
		fitCanvasH <- height*ratio
		fitCanvasW <- width*ratio
		ratio <- 1/ratio
	}
	#print(paste(imgFile, "height:", height, "width:", width, "ratio:", ratio, "fitCanvasH:", fitCanvasH, "fitCanvasW:", fitCanvasW))

	return (c(ratio, fitCanvasW, fitCanvasH, width, height))
}

showPicture <- function(e) {
	#print("enter showPicture")
	#clear canvas
    canvas <- e$activeCanvas
    tkdelete(canvas, "all")

	tpsDataList <- e$activeDataList
	id <- e$currImgId
	ratio <- tpsDataList[[id]][[6]]
	canvasH <- tpsDataList[[id]][[7]][2]
	canvasW <- tpsDataList[[id]][[7]][1]
	tkconfigure(canvas, width=canvasW, height=canvasH)

	showLines(e)
	showDots(e)

	if(e$tab != 0) {
		updateCtrlFrame(e)
	}
}

updateCtrlFrame <- function(e) {
	tpsDataList <- e$activeDataList
	if(e$tab == 1) {
		lineNumLabel <- e$linkLineNumLabel
		lineText <- "n links ="
		pathLabel <- e$linkImgPath
	}else if(e$tab == 2) {
		lineNumLabel <- e$sliderLineNumLabel
		lineText <- "n sliders ="
		pathLabel <- e$sliderImgPath
	}

	nlines <- 0
	specID <- "NA"
	if(length(tpsDataList) > 0) {
		nlines <- getLineNum(tpsDataList[[1]][[9]])
		specID <- tpsDataList[[e$currImgId]][1]
	}

	tkconfigure(lineNumLabel, text = paste(lineText, nlines))
    tkconfigure(pathLabel, text = paste("Specimen Id: ", specID))
}

onNext <- function(e) {
    tpsDataList <- e$activeDataList
	nSpecimens <- length(tpsDataList)
	if(nSpecimens > 0) {
		if(e$tab == 0) {
			digOnNext(e)
		}else {
			currImgId <- e$currImgId

			#check if it is the last picture
			if(currImgId < nSpecimens) {
				e$currImgId <- currImgId +1
				showPicture(e)
			} else {
				alertBox("It's the last specimen")
			}
		}
	}
}

onPrevious <- function(e) {
    tpsDataList <- e$activeDataList
	if(length(tpsDataList) > 0) {
		if(e$tab == 0) {
			digOnPrevious(e)
		}else {
			currImgId <- e$currImgId
			if(currImgId > 1) {
				e$currImgId <- currImgId -1
				showPicture(e)
			} else {
				alertBox("It's the first specimen")
			}
		}
	}
}

saveToCsv <- function(e) {
	tpsDataList <- e$activeDataList
	if(length(tpsDataList) <= 0) {
		alertBox("Nothing to be saved")
		return ()
	}
	#select the location
	filename <- tclvalue(tkgetSaveFile(filetypes="{TPS {.csv}}"))
	if (!nchar(filename)) {
		return ()
	}

	if (length(grep(".csv",x = filename)) <= 0 ) {
		filename <- paste(filename,".csv", sep = "")
    }

	file.create(filename, showWarnings=TRUE)
	lineCoord <- tpsDataList[[1]][[8]]
	lineStatus <- tpsDataList[[1]][[9]]
	lineNum <- getLineNum(lineStatus)

	ncolumn <- 2
	if(e$tab == 2) {
		write("before,slide,after",filename,append = TRUE)
		ncolumn <- 3
	}
	selected <- matrix(NA, nrow = lineNum, ncol = ncolumn)

	id <- 1
	j <- 1
	while(id <= lineNum){
		if(lineStatus[[j]] != "removed") {
			selected[id, 1] <- as.integer(lineCoord[[j]][1])
			selected[id, 2] <- as.integer(lineCoord[[j]][2])
			if(e$tab == 2) {
				selected[id, 3] <- as.integer(lineCoord[[j]][3])
			}
			id <- id + 1
		}
		j <- j+1
	}

	write.table(selected, filename, sep = ",", col.names = FALSE, row.names = FALSE,append=TRUE)
}

importTpsFile <- function(e, tpsfile) {
	tpsdata <- readland.tps2(file=tpsfile, specID = "ID")

	#######################################################################################
	# 7.30.2017 - EOC added to get tpsdata with its original
	# filename into the global environment
	file.name <- sub(x = basename(tpsfile), pattern = ".tps",replacement = "",ignore.case = TRUE)
	pos<-1
	envir <- as.environment(pos)
	assign(file.name, tpsdata$coords, envir = envir)
	############################################################################################

	olddat <- tpsdata$coords
	inscale <- tpsdata$scale
	attrs <- tpsdata$attrs

	filelist <- dimnames(olddat)[[3]]
	nSpecimens <- dim(olddat)[3]
	nlandmarks <- dim(olddat)[1]
	tpsDataList <- list()

	specId <- 1
	for(i in 1:nSpecimens){	
		if(length(attrs) > 0) {
			ratioV <- getRatio(attrs=attrs[[i]])	
		}else {
			speciName <- filelist[[i]]
			
			if(!file.exists(speciName)) {
				nSpecimens <- nSpecimens-1
				print(paste(speciName, "doesn't exist. Ignore it!!"))
				next
			}

			ext <- file_ext(speciName)
			if((ext != "gif") && (ext != "GIF")) {
				if(!file.exists(system.file(package="zooaRchGUI", "bin", "ffmpeg-3.2-win32-static", "bin", "ffmpeg.exe"))) {
					checkFfmpeg()
					return ()
				}				
				speciName <- findPPM(filelist[[i]])
			}

			ratioV <- getRatio(specimen = speciName)		
		}
			
		ratio <- ratioV[1]
		canvasH <- ratioV[3]
		if(ratio == 0) {
			nSpecimens <- nSpecimens-1
			next
		}
		#print(paste("ratio", ratio, "canvas h", canvasH, "canvas w", canvasW))

		tpsDataList[[specId]] <- list(filelist[[i]], inscale[i], list(), "inches", list(), ratio, c(ratioV[2], ratioV[3], ratioV[4], ratioV[5]), list(), list())

		coords <- olddat[, , i]
		temp <- list()
		statusList <- list()

		for (j in 1:nlandmarks){
			if(!is.na(coords[j, 1])) {
				temp[[j]] <- c(as.numeric(coords[j, 1])/ratio, canvasH-as.numeric(coords[j, 2])/ratio)
				statusList[[j]] <- "normal"
			}else {
				temp[[j]] <- c(-1, -1)
				statusList[[j]] <- "black"
			}
		}
		tpsDataList[[specId]][[5]] <- statusList		#dot status list
		tpsDataList[[specId]][[3]] <- temp 	#landmarks

		specId <- specId+1
	}

	if(nSpecimens > 0) {
		e$activeDataList <- tpsDataList
		e$currImgId <- 1
		e$landmarkNum <- nlandmarks
		
		tab <- e$tab
		if(tab == 0) {
			digitizeInit(e)
			e$digData <- tpsDataList
			e$digCurrImgId <- 1
			digUpdateSpecNumber(e, nSpecimens)
			digShowPicture(e)
		}else if (tab == 1) {
			linkInit(e)
			e$linkData <- tpsDataList		
			showPicture(e)
		}else if(tab == 2) {
			sliderInit(e)
			e$sliderData <- tpsDataList		
			showPicture(e)
		}
	}
}

importFile <- function(e, id) {
	if(e$tab == 1) {
		files <- e$linkFiles	
	} else if(e$tab == 2) {
		files <- e$sliderFiles
	}
	tpsfile <- files[id]
	importTpsFile(e, tpsfile)
}

openTpsFile <- function(e) {
    tpsfileName <- tclvalue(tkgetOpenFile( filetypes = "{{tps file} {.tps}}"))

    if (tpsfileName != "") {		
		importTpsFile(e, tpsfileName)
		tab <- e$tab
		#record user opened tps file recently
		if(tab != 0) {
			importFileList <- ""
			if(tab == 1) {
				importFileList <- "linkTpsFiles.txt"
			}else if(tab == 2) {
				importFileList <- "sliderTpsFiles.txt"
			}
			if(file.exists(importFileList)) {
				content <- scan(file = importFileList, what = "char", sep = "\n", quiet = TRUE)
				existing <- grep(tpsfileName, content, TRUE)
				if(length(existing) > 0) {
					#myPrint(paste(tpsfileName,"already exists, ignore it"))
					return ()
				}
			}else {
				file.create(importFileList, showWarnings = TRUE)
			}
			write(tpsfileName,importFileList,append = TRUE)
		}
	}
}

getDotId <- function(e, x, y) {
	#print("getDotId")
    tpsDataList <- e$activeDataList
    currImgId <- e$currImgId
    coords <- tpsDataList[[currImgId]][[3]]
	statusList <- tpsDataList[[currImgId]][[5]]
	if(length(coords)) {
		for(i in 1:length(coords)){
			if(statusList[[i]] == "removed") {next}
			if( (abs(as.integer(coords[[i]][1]) - as.integer(x)) <= 6)
					&& (abs(as.integer(coords[[i]][2]) - as.integer(y)) <= 6)) {
				return (i)
			}
		}
	}

	return (0)
}

writeland.tps<-function(A, file, scale = NULL, attrs = NULL, specID = TRUE, imgPath = TRUE){
  n<-dim(A)[3]
  k<-dim(A)[2]
  p<-dim(A)[1]

  lmline<-ifelse(k==2,paste("LM=",p,sep=""), paste("LM3=",p,sep=""))
  file.create(file, showWarnings=TRUE)
  if(!is.null(scale)){
    scaleline<-paste("SCALE", "=", scale, sep="")
  }
  
  for(i in 1:n){
    write(lmline,file,append = TRUE)
    write.table(A[,,i],file,col.names = FALSE, row.names = FALSE,append=TRUE)
	if(!is.null(attrs)){
      width <- paste("WIDTH=", attrs[1],sep="")
	  height <- paste("HEIGHT=", attrs[2],sep="")
      write(width,file,append = TRUE)
	  write(height,file,append = TRUE)
    }
	
	if(imgPath==TRUE){
      if(is.null(dimnames(A)[[3]])) dimnames(A)[[3]] <- c(1:dim(A)[3])
      imgPathLine<-paste("IMAGE=",dimnames(A)[[3]][i],sep="")
      write(imgPathLine,file,append = TRUE)
    }
    if(!is.null(scale)){
      if(length(scaleline) == 1){write(scaleline,file,append=TRUE)}
      if(length(scaleline) > 1){write(scaleline[i],file,append=TRUE)}
    }
    if(specID==TRUE){
      if(is.null(dimnames(A)[[3]])) dimnames(A)[[3]] <- c(1:dim(A)[3])
      idline<-paste("ID=",dimnames(A)[[3]][i],sep="")
      write(idline,file,append = TRUE)
    }
    write("",file,append = TRUE)
  }
}

##Function to read tps file for digitize2d (streamlined for specific use)
readland.tps2 <- function (file, specID = c("None", "ID", "imageID"))
{
  ignore.case = TRUE
  specID <- match.arg(specID)
  tpsfile <- scan(file = file, what = "char", sep = "\n", quiet = TRUE)
  lmdata <- grep("LM=", tpsfile, ignore.case)
  if (length(lmdata !=0)) {
    nland <- as.numeric(sub("LM=", "", tpsfile[lmdata], ignore.case))
    k <- 2
  }
  if (length(lmdata) == 0) {
    lmdata <- grep("LM3=", tpsfile, ignore.case)
    nland <- as.numeric(sub("LM3=", "", tpsfile[lmdata], ignore.case))
    k <- 3
  }
  n <- nspecs <- length(lmdata)
  if (max(nland) - min(nland) != 0) {
    stop("Number of landmarks not the same for all specimens.")
  }
  p <- nland[1]
  imscale <- as.numeric(sub("SCALE=", "", tpsfile[grep("SCALE",
                                                       tpsfile, ignore.case)], ignore.case))
  if (is.null(imscale)) {
    imscale = array(0, nspecs)
  }
  if (length(imscale)==0) {
    imscale = array(0, nspecs)
  }
  if (length(imscale) != nspecs) {
    imscale = array(1, nspecs)
  }
  tmp <- tpsfile[-(grep("=", tpsfile))]
  options(warn = -1)
  tmp <- matrix(as.numeric(unlist(strsplit(tmp,"\\s+"))),ncol = k, byrow = T)

  coords <- aperm(array(t(tmp), c(k, p, n)), c(2, 1, 3))
  #  imscale <- aperm(array(rep(imscale, p * k), c(n, k, p)), c(3, 2, 1))
  #  coords <- coords * imscale
  coords<-coords[1:nland,,]
  if(n==1) coords <- array(coords, c(nland,k,n))
  if (specID == "imageID") {
    imageID <- (sub("IMAGE=", "", tpsfile[grep("IMAGE", tpsfile, ignore.case)],
                    ignore.case))
    if (length(imageID) != 0) {
      imageID <- sub(".jpg", "", imageID, ignore.case)
      imageID <- sub(".tif", "", imageID, ignore.case)
      imageID <- sub(".bmp", "", imageID, ignore.case)
      imageID <- sub(".tiff", "", imageID, ignore.case)
      imageID <- sub(".jpeg", "", imageID, ignore.case)
      imageID <- sub(".jpe", "", imageID, ignore.case)
      dimnames(coords)[[3]] <- as.list(imageID)
    }
  }
  if (specID == "ID") {
    ID <- sub("ID=", "", tpsfile[grep("ID=", tpsfile, ignore.case)], ignore.case)
    if (length(ID) != 0) {
      dimnames(coords)[[3]] <- as.list(ID)
    }
  }
  
    width <- sub("WIDTH=", "", tpsfile[grep("WIDTH=", tpsfile, ignore.case)], ignore.case)
	height <- sub("HEIGHT=", "", tpsfile[grep("HEIGHT=", tpsfile, ignore.case)], ignore.case)
	attrs <- list()
	if((length(width) > 0) && (length(width) == length(height))) {
	    for(i in 1: length(width)) {
			attrs[[i]] <- c(as.integer(width[[i]]), as.integer(height[[i]]))
		}	
	}
  
  return(list(coords = coords,scale=imscale, attrs = attrs)  )
}

myPrint <- function(content) {
	if(isDebug) {
		print(content)
	}
}
