scaleUnitVal <- tclVar("inches")
applyToAll <- tclVar("yes")

clearScaleDot <- function(e) {
	e$scaleDot1 <- c(0, 0)
	e$scaleDot2 <- c(0, 0)
	e$scaleDotNum <- 0
	e$scaleMode <- 0
}

digitizeInit <- function(e) {
	e$font <- 10
	e$landmarkNum <- 5
	e$zoom <- 0
	e$digCurrImgId <- 1
	e$digData <- list()

	clearScaleDot(e)
}

dotMainMenu <- function(e) {
#print("dotMainMenu")
    topMenu <- tkmenu(e$wnd)
    tkconfigure(e$wnd, menu = topMenu)

    #File Menu
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
    tkadd(fileMenu, "command", label = "Create tps file",command = function() openSpecimens(e))
    tkadd(fileMenu, "command", label = "Open tps file", command = function() openTpsFile(e))
    tkadd(fileMenu, "command", label = "Save", command = function() savetoTpsFile(e))
    tkadd(fileMenu, "command", label = "Exit", command = function() tkdestroy(e$wnd))
    tkadd(topMenu, "cascade", label = "File", menu = fileMenu)

	e$dotMenuTree <- topMenu
}

#draw canvas and other widgets
digCreateCtlFrame <- function(e, parent) {
#print("digCreateCtlFrame")

	digCtlFrame <- ttkframe(parent)

	setScaleBtn <- ttkbutton(digCtlFrame, text = "Digitize scale", command = function() setScale(e))
	setLandmarkNumBtn <- ttkbutton(digCtlFrame, text = "Set number of landmarks", command = function() setLandmarkNum(e))

    e$scaleLabel = tklabel(digCtlFrame, text='Scale Factor: not set')

    e$imgPath <- ttklabel(digCtlFrame, text = "Path:")
    zoomBtn1 <- ttkbutton(digCtlFrame, text = "zoom +", command = function() onZoom(e, "in"))
    zoomBtn2 <- ttkbutton(digCtlFrame, text = "zoom -",command = function() onZoom(e, "out"))
    fitBtn <- ttkbutton(digCtlFrame, text = "Fit",command = function() onFit(e))

    e$labelLandmarkVar <-tclVar("1")
    labelLandmark <- ttkcheckbutton(digCtlFrame, text = "Label Landmark",variable=e$labelLandmarkVar, command= function()onLabelLandMark(e))

    fontAdd <- ttkbutton(digCtlFrame, text = "Label Size +", command = function() onFontAdd(e))
    fontDec <- ttkbutton(digCtlFrame, text = "Label Size -",command = function() onFontDec(e))

	e$specimenNumLabel <- ttklabel(digCtlFrame, text = "Number of Specimens: 0")
	e$landMarkNumLabel <- ttklabel(digCtlFrame, text = "Number of Landmarks: 0")

    e$missLandmarkVar <-tclVar("0")
    e$missLandmarkCheBtn <- ttkcheckbutton(digCtlFrame, text = "Missing Landmark",variable=e$missLandmarkVar)

    sapply(list(setScaleBtn, setLandmarkNumBtn, e$scaleLabel, e$imgPath, zoomBtn1, zoomBtn2, fitBtn, labelLandmark, fontAdd, fontDec, e$specimenNumLabel, e$landMarkNumLabel, e$missLandmarkCheBtn), tkpack)
	return (digCtlFrame)
}

bindDigCanvasEvent <-function(e, canvas) {
    tkbind(canvas, "<Double-Button-1>", function(x, y) {
	dotAdd(e, x, y)})
    tkbind(canvas, "<B1-Motion>", function(x, y) {
	dotMove(e, x, y)})
	tkbind(canvas, "<ButtonRelease-1>", function(x, y) {
	dotRelease(e, x, y)})
    tkitembind(canvas, "point", "<1>", function(x, y) {
	dotSelect(e, x, y)})
    tkitembind(canvas, "point", "<3>", function(x, y) {
	dotRemove(e, x, y)})
}

digUpdateSpecNumber <-function(e, num) {
	#print("digUpdateSpecNumber")
	tkconfigure(e$specimenNumLabel, text = paste("Number of Specimens: ", num))
}

digShowPicture <- function(e) {
#print("digShowPicture")
	onFit(e)
	tpsDataList <- e$activeDataList
	imgId <- e$currImgId
	imgFile <- tpsDataList[[imgId]][[1]]

    #update image path
    pathLabel <- e$imgPath
    tkconfigure(pathLabel, text = paste("Path: ", imgFile))

	#update scale factor
	scaleFactor <- tpsDataList[[imgId]][[2]]
	if(scaleFactor) {
		tkconfigure(e$scaleLabel, text = paste("Scale Factor: ", format(scaleFactor, digits = 5)))
	} else {
		tkconfigure(e$scaleLabel, text = paste("Scale Factor: ", "not set"))
	}

	#update number of landmarks
	dotNum <- digGetDotNum(e)
	tkconfigure(e$landMarkNumLabel, text = paste("Number of Landmarks: ", dotNum))
}

onZoom <- function(e, type) {
	#calculate current zoom level
	if(type == "in") {
		currZoom = as.integer(e$zoom)+1
	}
	else if(type == "out") {
		currZoom = as.integer(e$zoom)-1
	}

	#build image according to zoom level
	zoomImg <- tclVar()
    tcl('image', 'create', 'photo', zoomImg)
	if(currZoom > 0) {
		tcl(zoomImg, 'copy', e$fitImage, zoom=(1+currZoom)) #large
	}else if(currZoom < 0){
		tcl(zoomImg, 'copy', e$fitImage, subsample=(1+abs(currZoom)))
	} else {
		zoomImg <- e$fitImage
	}

	canvas <- e$activeCanvas
	tpsDataList <- e$activeDataList
	imgId <- e$currImgId
	tkdelete(canvas, "all")
	canvasH <- tpsDataList[[imgId]][[7]][2]
	canvasW <- tpsDataList[[imgId]][[7]][1]

	tkconfigure(canvas, width=canvasW, height=canvasH)
	tkcreate(canvas, "image", as.integer(canvasW)/2, as.integer(canvasH)/2, image=zoomImg)

	#show dots and lable when size is back to normal
	if(currZoom == 0) {
		showDots(e)
		onLabelLandMark(e)
	}

	e$zoom <- currZoom
}

onFontAdd <- function(e) {
    e$font <- e$font + 1
    tkdelete(e$activeCanvas, "label")
    onLabelLandMark(e)
}

onFontDec <- function(e) {
	e$font <- e$font - 1
    tkdelete(e$activeCanvas, "label")
    onLabelLandMark(e)
}

onFit <- function(e) {
	#print("enter onFit")

	id <- e$currImgId
	canvas <- e$activeCanvas

	#show image
	tpsDataList <- e$activeDataList
	imgFile <- tpsDataList[[id]][[1]]
	zoomImg <- tclVar()
	tcl('image', 'create', 'photo', zoomImg)
    img <- tclVar()
    tcl('image', 'create', 'photo', img, file=imgFile)

	#get ratio
	ratio <- tpsDataList[[id]][[6]]
	canvasH <- tpsDataList[[id]][[7]][2]
	canvasW <- tpsDataList[[id]][[7]][1]

	height <- as.integer(tcl('image', 'height', img))

	ratio <- as.integer(height/600)
	if(ratio < 1) {
		tcl(zoomImg, 'copy', img, zoom=as.integer(600/height))
	} else {
		tcl(zoomImg, 'copy', img, subsample=ratio)
	}

	#print(paste("id:", id, "height:", height, "width:", "NA", "ratio:", ratio, "canvasH:", canvasH, "canvasW:", canvasW))
	tkconfigure(canvas, width=canvasW, height=canvasH)
	tkcreate(canvas, "image", as.integer(canvasW)/2, as.integer(canvasH)/2, image=zoomImg)
	#assign("digData", tpsDataList, envir = .GlobalEnv)

	#reset zoom level
	e$fitImage <- zoomImg
	e$zoom <- 0
	e$activeCanvas <- canvas

	showDots(e)
	onLabelLandMark(e)
}

label<-function(e, id, x, y) {
	#print("label")
    canvas <- e$activeCanvas
    l <- tkcreate(canvas, "text", x + 12, y, text=id, fill="red", font=c("Helvetica", e$font))
    tkaddtag(canvas, "label", "withtag", l)
}

onLabelLandMark <- function(e) {
#print("onLabelLandMark")
    tkdelete(e$activeCanvas, "label")

    if (tclvalue(e$labelLandmarkVar) == "1") {
        tpsDataList <- e$activeDataList
		if(length(tpsDataList) > 0) {
			imgId <- e$currImgId
			temp <- tpsDataList[[imgId]][[3]]
			statusList <- tpsDataList[[imgId]][[5]]

			if(length(temp)) {
				id <- 1
				for(i in 1:length(statusList)){
					#print(paste("i:", i, "status:", statusList[[i]]))
					if(statusList[[i]] == "removed") {next}
					label(e, id, temp[[i]][1], temp[[i]][2])
					id <- id + 1
				}
			}
		}
    }
}

#show next specimen
digOnNext <- function(e) {
	#check if it is the last picture
	if(e$scaleMode) {
		alertBox("Please finish scale calculation")
	} else if(e$currImgId < length(e$activeDataList)) {
		#check if the landmark is enough
		dotNum <- digGetDotNum(e)
		if(dotNum >= as.integer(e$landmarkNum)) {
			e$currImgId <- e$currImgId+1
			digShowPicture(e)
		} else {
			alertBox("Incorrect number of landmarks")
		}
    } else {
        alertBox("It's the last specimen")
    }
}

#show the previous specimen
digOnPrevious <- function(e) {
	if(e$scaleMode) {
		alertBox("Please finish scale calculation")
	} else if(e$currImgId > 1) {
		#check if the landmark is enough
		dotNum <- digGetDotNum(e)
		if(dotNum >= as.integer(e$landmarkNum)) {
			e$currImgId <- e$currImgId-1
			digShowPicture(e)
		} else {
			alertBox("Incorrect number of landmarks")
		}

    } else {
        alertBox("It's the first specimen")
    }
}

getPicScale <- function(e, scale) {
	#get ratio
	id <- e$currImgId
	dot1 <- e$scaleDot1
	dot2 <- e$scaleDot2
    tpsDataList <- e$activeDataList
	ratio <- tpsDataList[[id]][[6]]
	fitImgH <- as.integer(tpsDataList[[id]][[7]])

	x1 <- as.double(dot1[1]*ratio)
	y1 <- as.double(fitImgH-dot1[2])*ratio
	x2 <- as.double(dot2[1]*ratio)
	y2 <- as.double(fitImgH-dot2[2])*ratio

    scale <- as.numeric(scale)

	return (scale/sqrt(sum((x2-x1)^2+(y2-y1)^2)))
}

onSetScaleOk <- function(e) {
	if(e$scaleDotNum < 2) {
		alertBox("Incorrect number of scale dots")
		return (0)
	}

	#get distance
    distance <- tclvalue(tkget(e$distanceEntry))

	#calculate scale factor
	scaleFactor <- getPicScale(e, distance)
	#print(paste("scale factor:",scaleFactor))

	#show scale factor
	tkconfigure(e$scaleLabel, text = paste("Scale Factor: ", format(scaleFactor, digits = 5)))

	#update tpsDataList
	tpsDataList <- e$activeDataList
	currImgId <- e$currImgId
	if(tclvalue(applyToAll) == "yes") {
		#print(paste("apply to all", tclvalue(applyToAll)))
		#print(paste("scaleUnitVal", tclvalue(scaleUnitVal)))
		for(i in 1:length(tpsDataList)) {
			tpsDataList[[i]][[2]] <- scaleFactor
			tpsDataList[[i]][[4]] <- tclvalue(scaleUnitVal)
		}
	} else {
		tpsDataList[[currImgId]][[2]] <- scaleFactor
		tpsDataList[[currImgId]][[4]] <- tclvalue(scaleUnitVal)
	}

	#clear variables
	clearScaleDot(e)
	e$activeDataList <- tpsDataList

	tkdestroy(e$setScaleWin)
}

onlandmarkNumOk <- function(e) {
	#get user input value
    e$landmarkNum <- tclvalue(tkget(e$landmarkEntry))

	# turn to the first picture
	tpsDataList <- e$activeDataList
	e$currImgId <- 1
	digShowPicture(e)

	tkdestroy(e$landmarkNumWin)
}

createSetScaleWind <- function(e) {
	win <- tktoplevel()
	tkwm.title(win, "Set Scale")

	label_frame <- ttkframe(win)
	tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
	tkgrid.columnconfigure(label_frame, 0, weight = 1)
	tkgrid.columnconfigure(label_frame, 1, weight = 1)
	tkgrid.columnconfigure(label_frame, 2, weight = 1)
	tkgrid.columnconfigure(label_frame, 1, weight = 1)

	distancelabel = tklabel(label_frame, text='distance: ')
	e$distanceEntry = tkentry(label_frame, textvariable=tclVar("10"))
	tkgrid(distancelabel, row = 0, column = 0, sticky = "e")
	tkgrid(e$distanceEntry, row = 0, column = 1, sticky = "e")

	unitlabel = tklabel(label_frame, text='unit: ')
	unitCombox <- ttkcombobox(label_frame, state = "readonly", values = c('inches', 'millimeters', 'centimeters'), textvariable = scaleUnitVal)
	tkgrid(unitlabel, row = 1, column = 0, sticky = "e")
	tkgrid(unitCombox, row = 1, column = 1, sticky = "e")
	e$unitCombox <- unitCombox

	applyLabel = tklabel(label_frame, text='apply to all specimens')
	applyCombox <- ttkcombobox(label_frame, state = "readonly", values = c('yes', 'no'), textvariable = applyToAll)
	tkgrid(applyLabel, row = 2, column = 0, sticky = "e")
	tkgrid(applyCombox, row = 2, column = 1, sticky = "e")
	e$applyCombox <- applyCombox

	noteLable <- tklabel(win, text='Note: Please place two dots for scale setting')
	tkpack(noteLable, expand = TRUE, fill = "both", padx = 5, pady = 5)

	btnFrame <- ttkframe(win)
	tkpack(btnFrame, fill = "x", padx = 5, pady = 5)
	cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() tkdestroy(win))
	okBtn <- ttkbutton(btnFrame, text = "ok",command = function() onSetScaleOk(e))

	tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "y", side = "left")
	sapply(list(cancelBtn, okBtn), tkpack, side = "left", padx = 6)

	e$setScaleWin <- win
	tkfocus(win)
}

setScale <- function(e) {
	canvas <- e$activeCanvas
	tkdelete(canvas, "scale")
	tpsDataList <- e$activeDataList

	if(length(tpsDataList) == 0) {
		alertBox("no picture open")
	} else {
		e$scaleMode <- 1
		createSetScaleWind(e)
	}
}

setLandmarkNum <- function(e) {
	win <- tktoplevel()
	tkwm.title(win, "Set Landmark Number")

	entryFrame <- ttkframe(win)
	tkpack(entryFrame, expand = TRUE, fill = "both", padx = 5, pady = 5)
	label = tklabel(entryFrame, text='Set landmark Number: ')

    e$landmarkEntry = tkentry(entryFrame, textvariable=tclVar(e$landmarkNum))
	sapply(list(label, e$landmarkEntry), tkpack, side = "left", padx = 6)

	btnFrame <- ttkframe(win)
	tkpack(btnFrame, fill = "x", padx = 5, pady = 5)
	cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() tkdestroy(win))
	okBtn <- ttkbutton(btnFrame, text = "ok",command = function() onlandmarkNumOk(e))

	tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "y", side = "left")
	sapply(list(cancelBtn, okBtn), tkpack, side = "left", padx = 6)

	e$landmarkNumWin <- win
	tkfocus(win)
}

savetoTpsFile <- function(e) {
    currImgId <- e$currImgId
    tpsDataList <- e$activeDataList

	#check if the landmark is enough
	dotNum <- digGetDotNum(e)
	landmarkNum <- as.integer(e$landmarkNum)
	if(dotNum < landmarkNum) {
		alertBox("Incorrect number of landmarks")
		return ()
	}

	#select the location
	filename <- tclvalue(tkgetSaveFile(filetypes="{TPS {.tps}}"))
	if (!nchar(filename)) {
		return ()
	}

    newdata <- array(0, c(landmarkNum, 2, length(tpsDataList)))
    filelist <- list()
    scalebar <- list()

    for(i in 1:length(tpsDataList)){
        # get image file list
        filelist[[length(filelist)+1]] <- tpsDataList[[i]][[1]]

        #get scale list
        scalebar[[length(scalebar)+1]] <- tpsDataList[[i]][[2]]

		#get ratio
		ratio <- tpsDataList[[i]][[6]]
		fitImgH <- as.integer(tpsDataList[[i]][[7]][2])
		#print(paste("i:", i, " ratio: ", ratio, " fitImgH: ", fitImgH))

        #construct landmark matrix
		coordi <- tpsDataList[[i]][[3]]
		statusList <- tpsDataList[[i]][[5]]
		#print(paste("landmarknum:", landmarkNum))
        selected <- matrix(NA, nrow = landmarkNum, ncol = 2)
        id <- 1
		j <- 1
        while(id <= landmarkNum){
            if(statusList[[j]] != "removed") {

				if(statusList[[j]] == "black") {
					selected[id, 1] <- NA
					selected[id, 2] <- NA
				} else {
					selected[id, 1] <- as.double(coordi[[j]][1]*ratio)
					selected[id, 2] <- as.double(fitImgH-coordi[[j]][2])*ratio
				}
				id <- id + 1
			}

			j <- j+1
        }
        #print("landmark: ")
        #print(selected)
        newdata[, , i] <- selected
    }

    dimnames(newdata)[[3]] <- as.list(filelist)

    ##################################################################################
    # 7.30.2017 - EOC change: added conditional structure to avoid double ".tps" when
    # saving file
    if (length(grep(".tps",x = filename)) > 0 ) {
      writeland.tps(newdata, filename, scalebar)
    } else {
      writeland.tps(newdata, paste(filename,".tps", sep = ""), scalebar)
      }
    ##################################################################################
}

# get image file
openSpecimens <- function(e) {
  #print("openSpecimens")
  #############################################################
  # 8.9.2017 EOC added "title" argument to tkgetOpenFile
  # only GIF files are serched for until other files can be read
  fileStr <- tclvalue(tkgetOpenFile( filetypes = "{{GIF file} {.gif}}",
                                     multiple=TRUE, title="Select Images to Digitize"))
  ###########################################################

  ################################################################
  # 8.10.2017 EOC changed seems that when spaces are in file name, brackets {} are placed in output of tkgetOpenFile
  # but when no spaces, strng is without brackets. I inserted conditional to detect whether {} are present and separate
  # files differently when they are or not present.######
  if (length(grep(pattern = "}",x = fileStr)) >0 ){
    ################################################################
    # 8.9.2017 EOC changed strsplit's pattern from " " to removing
    # brackets {}, " " became a problem when filenames had spaces.
    imgList <- unlist(strsplit(fileStr, "} ",fixed = FALSE))
    imgList <- gsub(pattern = "}",replacement = "",x = imgList)
    imgList <- gsub(pattern = "\\{",replacement = "",x = imgList)
    ################################################################
  } else {
    imgList <- unlist(strsplit(fileStr, " ",fixed = FALSE))
  }
  ##################################################################

    nSpecimens <- length(imgList)

    if (nSpecimens != 0) {
		#initialize tpsDataList
        tpsDataList <- list()
        for(i in 1:length(imgList)){
			ratioV <- getRatio(imgList[[i]])
			ratio <- ratioV[1]
			canvasW <- ratioV[2]
			canvasH <- ratioV[3]

			if(ratio == 0) {
				nSpecimens <- nSpecimens-1
				next
			}

			tpsDataList[[length(tpsDataList)+1]] <- list(imgList[[i]], 0, list(), "inches", list(), ratio, c(canvasW, canvasH))
        }

		if(nSpecimens > 0) {
			#initialize
			digitizeInit(e)

			e$activeDataList <- tpsDataList
			e$digData <- tpsDataList
			e$currImgId <- 1

			digShowPicture(e)

			tkconfigure(e$specimenNumLabel, text = paste("Number of Specimens: ", nSpecimens))
		}
    }
}

updateDotList <- function(e, x, y, operate, moveDotId = 0, status="normal") {
    tpsDataList <- e$activeDataList
    currImgId <- e$currImgId
    temp <- tpsDataList[[currImgId]][[3]]
	statusList <- tpsDataList[[currImgId]][[5]]
    dotId <- getDotId(e, x, y)
	if(dotId != 0) {
		newAdded <- FALSE
	}else {
		newAdded <- TRUE
	}

    if (operate == "add") {
        if(dotId == 0) {
            temp[[length(temp)+1]] <- c(x, y)
			statusList[[length(statusList)+1]] <- status
            #newAdded <- digGetDotNum(statusList)
        }
    } else if (operate == "move") {
        temp[[moveDotId]] <- c(x,y)
    } else if(operate == "remove") {
        #temp[dotId] <- NA
		statusList[dotId] <- "removed"
    }

    tpsDataList[[currImgId]][[3]] <- temp
	tpsDataList[[currImgId]][[5]] <- statusList

    e$activeDataList <- tpsDataList
    return (newAdded)
}

dotAdd<-function(e, x, y) {
	tpsDataList <- e$activeDataList
	if(length(tpsDataList) <= 0) {
		return ()
	}

    x<-as.integer(x)
    y<-as.integer(y)

	canvas <- e$activeCanvas
	scaleDotNum <- e$scaleDotNum

	scaleMode <- e$scaleMode
	if(scaleMode) {
		scaleDotNum <- scaleDotNum+1
		tkdelete(canvas, "scaleline")
        item <- tkcreate(canvas, "oval", x - 2, y - 2, x + 2, y + 2,
                         width=1, outline="black", fill="red")
		tkaddtag(canvas, "scale", "withtag", item)

		if(scaleDotNum == 1) {
			e$scaleDot1 <- c(x,y)
			e$scaleDotNum <- scaleDotNum
		} else if(scaleDotNum == 2) {
			e$scaleDot2 <- c(x,y)
			e$scaleDotNum <- scaleDotNum
			scaleDot1 <- e$scaleDot1
			item <- tkcreate(canvas, "line", scaleDot1[1], scaleDot1[2], x, y, width=1, fill="red")
			tkaddtag(canvas, "scaleline", "withtag", item)
			e$scaleMode <- 0
		}
	} else {
		fill <- "red"
		outline <- "black"
		dotStatus <- "normal"
		if (tclvalue(e$missLandmarkVar) == "1") {
			fill <- "black"
			outline <- "red"
			dotStatus <- "black"
			updateMissLandMark(e, 0)
		}
		res <- updateDotList(e, x, y, "add", status=dotStatus)

		if(res) {
			item <- tkcreate(canvas, "oval", x - 6, y - 6, x + 6, y + 6,
							 width=1, outline=outline,
							 fill=fill)
			tkaddtag(canvas, "point", "withtag", item)

			#add label for this dot
			dotNum <- digGetDotNum(e)
			if((tclvalue(e$labelLandmarkVar) == "1")) {
				label(e, dotNum, x, y)
			}

			tkconfigure(e$landMarkNumLabel, text = paste("Number of Landmarks: ", dotNum))
		}
	}
}

updateMissLandMark <-function(e, value) {
	e$missLandmarkVar <-tclVar(value)
	tkconfigure(e$missLandmarkCheBtn, variable=e$missLandmarkVar)
}

dotSelect<-function(e, x, y) {
	if(!e$scaleMode) {
		canvas <- e$activeCanvas
		x <- as.numeric(x)
		y <- as.numeric(y)
		tkdtag(canvas, "selected")
		tkaddtag(canvas, "selected", "withtag", "current")
		tkitemraise(canvas,"current")
		dotId <- getDotId(e, x, y)
		e$selectedDot <- dotId
	}
}

dotRelease <- function(e, x, y) {
	if(length(e$activeDataList) <= 0) {
		return ()
	}

	if(!e$scaleMode) {
		#redraw the label
		canvas <- e$activeCanvas
		tkdelete(canvas, "label")
		onLabelLandMark(e)
		e$selectedDot <- 0
	}
}

dotMove <- function(e, x, y) {
    ## This procedure is invoked during mouse motion events.
    ## It drags the current item.
    ##
    ## Arguments:
    ## x, y -    The coordinates of the mouse.

	if(length(e$activeDataList) <= 0) {
		return ()
	}

	if(!e$scaleMode) {
		x <- as.numeric(x)
		y <- as.numeric(y)
		tpsDataList <- e$activeDataList
		currImgId <- e$currImgId
		temp <- tpsDataList[[currImgId]][[3]]
		dotId <- e$selectedDot
		if(dotId) {
			canvas <- e$activeCanvas
			tkmove(canvas, "selected", x - temp[[dotId]][1], y -  temp[[dotId]][2])
			updateDotList(e, x, y, "move", dotId)
		}
	}
}

digRemoveDotOk <-function(e, x, y) {
	updateDotList(e, x, y, "remove")
	canvas <- e$activeCanvas
	tkdelete(canvas, "point")
	tkdelete(canvas, "label")

	showDots(e)

	#redraw the label
	onLabelLandMark(e)

	#update landmark number
	dotNum <- digGetDotNum(e)
	tkconfigure(e$landMarkNumLabel, text = paste("Number of Landmarks: ", dotNum))

	win <- e$removeWin
	tkdestroy(win)
}

digGetDotNum <- function(e) {
	#check if the landmark is enough
	imgId <- e$currImgId
	tpsDataList <- e$activeDataList
	statusList <- tpsDataList[[imgId]][[5]]

	len <- 0
	if(length(statusList)) {
		for(i in 1:length(statusList)) {
			if(statusList[[i]] != "removed") {len = len+1}
		}
	}
	return (len)
}

dotRemove <-function(e, x, y) {
	if(!e$scaleMode) {
		popUpRemoveWindow(e, x, y, 'Do you want to delete this landmark?', "digdot")
	}
}
