font <-10

scaleMode <- 0

scaleUnitVal <- tclVar("inches")
applyToAll <- tclVar("yes")

distance <- 0
scaleDot1 <- c(0, 0)
scaleDot2 <- c(0, 0)
scaleDotNum <- 0

zoomH <- 600

dotMainMenu <- function(wnd) {
#print("dotMainMenu")
    topMenu <- tkmenu(wnd)
    tkconfigure(wnd, menu = topMenu) 
	
    #File Menu
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)  # TOP menu
    tkadd(fileMenu, "command", label = "Create tps file",command = function() openSpecimens())
    tkadd(fileMenu, "command", label = "Open tps file", command = function() openTpsFile())
    tkadd(fileMenu, "command", label = "Save", command = function() savetoTpsFile())
    tkadd(fileMenu, "command", label = "Exit", command = function() tkdestroy(wnd))
    tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
	
	assign("dotMenuTree", topMenu, envir = .GlobalEnv)
}

digitizeInit <- function() {

	assign("landmarkNum", 5, envir = .GlobalEnv)

	#initialize
	scaleMode <<- 0
	applyToAll <<- tclVar("choose one")
	scaleUnitVal <<- tclVar("choose one")
	distance <<- 0
	scaleDot1 <<- c(0, 0)
	scaleDot2 <<- c(0, 0)
	scaleDotNum <<- 0
}

#draw canvas and other widgets
digCreateCtlFrame <- function(parent) {
#print("digCreateCtlFrame")

	digCtlFrame <- ttkframe(parent)

	setScaleBtn <- ttkbutton(digCtlFrame, text = "Digitize scale", command = function() setScale())
	setLandmarkNumBtn <- ttkbutton(digCtlFrame, text = "Set number of landmarks", command = function() setLandmarkNum())

    scaleLabel = tklabel(digCtlFrame, text='Scale Factor: not set')
    assign("scaleLabel", scaleLabel, envir = .GlobalEnv)

    path <- ttklabel(digCtlFrame, text = "Path:")
    assign("imgPath", path, envir = .GlobalEnv)
    zoomBtn1 <- ttkbutton(digCtlFrame, text = "zoom +", command = function() onZoom("in"))
    zoomBtn2 <- ttkbutton(digCtlFrame, text = "zoom -",command = function() onZoom("out"))
    fitBtn <- ttkbutton(digCtlFrame, text = "Fit",command = function() onFit())

    labelLandmarkVar <-tclVar("1")
    assign("labelLandmark", labelLandmarkVar, envir = .GlobalEnv)
    labelLandmark <- ttkcheckbutton(digCtlFrame, text = "Label Landmark",variable=labelLandmarkVar, command=onLabelLandMark)

    fontAdd <- ttkbutton(digCtlFrame, text = "Label Size +", command = function() onFontAdd())
    fontDec <- ttkbutton(digCtlFrame, text = "Label Size -",command = function() onFontDec())

	specimenNumLabel <- ttklabel(digCtlFrame, text = "Number of Specimens: 0")
    assign("specimenNumLabel", specimenNumLabel, envir = .GlobalEnv)

	landMarkNumLabel <- ttklabel(digCtlFrame, text = "Number of Landmarks: 0")
    assign("landMarkNumLabel", landMarkNumLabel, envir = .GlobalEnv)

    missLandmarkVar <-tclVar("0")
    assign("missLandmark", missLandmarkVar, envir = .GlobalEnv)
    missLandmark <- ttkcheckbutton(digCtlFrame, text = "Missing Landmark",variable=missLandmarkVar)
	assign("missLandmarkCheBtn", missLandmark, envir = .GlobalEnv)
    sapply(list(setScaleBtn, setLandmarkNumBtn, scaleLabel, path, zoomBtn1, zoomBtn2, fitBtn, labelLandmark, fontAdd, fontDec, specimenNumLabel, landMarkNumLabel, missLandmark), tkpack)
	return (digCtlFrame)
}

bindDigCanvasEvent <-function(canvas) {    
    tkbind(canvas, "<Double-Button-1>", dotAdd)
    tkbind(canvas, "<B1-Motion>", dotMove)
	tkbind(canvas, "<ButtonRelease-1>", dotRelease)
    tkitembind(canvas, "point", "<1>", dotSelect)
    tkitembind(canvas, "point", "<3>", dotRemove)
}

digUpdateSpecNumber <-function(num) {
	#print("digUpdateSpecNumber")
	specimenNumLabel <- get("specimenNumLabel", envir = .GlobalEnv)
	tkconfigure(specimenNumLabel, text = paste("Number of Specimens: ", num))
}

digShowPicture <- function() {
#print("digShowPicture")
	onFit()

	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
	imgId <- get("currImgId", envir = .GlobalEnv)
	imgFile <- tpsDataList[[imgId]][[1]]

    #update image path
    pathLabel <- get("imgPath", envir = .GlobalEnv)
    tkconfigure(pathLabel, text = paste("Path: ", imgFile))

	#update scale factor
	scaleLabel <- get("scaleLabel", envir = .GlobalEnv)
	scaleFactor <- tpsDataList[[imgId]][[2]]
	if(scaleFactor) {
		tkconfigure(scaleLabel, text = paste("Scale Factor: ", format(scaleFactor, digits = 5)))
	} else {
		tkconfigure(scaleLabel, text = paste("Scale Factor: ", "not set"))
	}

	#update number of landmarks
	dotNum <- digGetDotNum()
	landMarkNumLabel <- get("landMarkNumLabel", envir = .GlobalEnv)
	tkconfigure(landMarkNumLabel, text = paste("Number of Landmarks: ", dotNum))
}

onZoom <- function(type) {
	#calculate current zoom level
	zoom <- get("zoom", envir = .GlobalEnv)
	if(type == "in") {
		currZoom = as.integer(zoom)+1
	}
	else if(type == "out") {
		currZoom = as.integer(zoom)-1
	}

	#build image according to zoom level
	zoomImg <- tclVar()
    tcl('image', 'create', 'photo', zoomImg)
	fitImage <- get("fitImage", envir = .GlobalEnv)
	if(currZoom > 0) {
		tcl(zoomImg, 'copy', fitImage, zoom=(1+currZoom)) #large
	}else if(currZoom < 0){
		tcl(zoomImg, 'copy', fitImage, subsample=(1+abs(currZoom)))
	} else {
		zoomImg <- fitImage
	}

	canvas <- get("activeCanvas", envir = .GlobalEnv)
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
	imgId <- get("currImgId", envir = .GlobalEnv)
	tkdelete(canvas, "all")
	canvasH <- tpsDataList[[imgId]][[7]][2]
	canvasW <- tpsDataList[[imgId]][[7]][1]

	tkconfigure(canvas, width=canvasW, height=canvasH)
	tkcreate(canvas, "image", as.integer(canvasW)/2, as.integer(canvasH)/2, image=zoomImg)

	#show dots and lable when size is back to normal
	if(currZoom == 0) {
		showDots(imgId)
		onLabelLandMark()
	}

	assign("zoom", currZoom, envir = .GlobalEnv)
}

onFontAdd <- function() {
    assign("font", font + 1, envir = .GlobalEnv)
	canvas <- get("activeCanvas", envir = .GlobalEnv)
    tkdelete(canvas, "label")
    onLabelLandMark()
}

onFontDec <- function() {
    assign("font", font - 1, envir = .GlobalEnv)
	canvas <- get("activeCanvas", envir = .GlobalEnv)
    tkdelete(canvas, "label")
    onLabelLandMark()
}

onFit <- function() {
	#print("enter onFit")
	
	id <- get("currImgId", envir = .GlobalEnv)	
	canvas <- get("activeCanvas", envir = .GlobalEnv)
	
	#show image
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
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
	assign("fitImage", zoomImg, envir = .GlobalEnv)
	assign("zoom", 0, envir = .GlobalEnv)
	assign("activeCanvas", canvas, envir = .GlobalEnv)

	showDots(id)
	onLabelLandMark()
}

label<-function(id, x, y) {
	#print("label")
    canvas <- get("activeCanvas", envir = .GlobalEnv)
    font <- get("font",  envir = .GlobalEnv)
    l <- tkcreate(canvas, "text", x + 12, y, text=id, fill="red", font=c("Helvetica", font))
    tkaddtag(canvas, "label", "withtag", l)
}

onLabelLandMark <- function() {
#print("onLabelLandMark")
	canvas <- get("activeCanvas", envir = .GlobalEnv)
    tkdelete(canvas, "label")
	
    labelLandmarkVar <- get("labelLandmark", envir = .GlobalEnv)
    if (tclvalue(labelLandmarkVar) == "1") {
        tpsDataList <- get("activeDataList", envir = .GlobalEnv)

		if(length(tpsDataList) > 0) {
			imgId <- get("currImgId", envir = .GlobalEnv)
			temp <- tpsDataList[[imgId]][[3]]
			statusList <- tpsDataList[[imgId]][[5]]
			if(length(temp)) {
				id <- 1
				for(i in 1:length(statusList)){
					#print(paste("i:", i, "status:", statusList[[i]]))
					if(statusList[[i]] == "removed") {next}
					label(id, temp[[i]][1], temp[[i]][2])
					id <- id + 1
				}
			}
		}        
    } 
}

#show next specimen
digOnNext <- function() {
    currImgId <- get("currImgId", envir = .GlobalEnv)
    tpsDataList <- get("activeDataList", envir = .GlobalEnv)

	#check if it is the last picture
	if(scaleMode) {
		alertBox("Please finish scale calculation")
	} else if(currImgId < length(tpsDataList)) {
		#check if the landmark is enough
		dotNum <- digGetDotNum()
		landmarkNum <- get("landmarkNum", envir = .GlobalEnv)
		if(dotNum >= as.integer(landmarkNum)) {
			assign("currImgId", currImgId+1, envir = .GlobalEnv)
			digShowPicture()
		} else {
			alertBox("Incorrect number of landmarks")
		}

    } else {
        alertBox("It's the last specimen")
    }
}

#show the previous specimen
digOnPrevious <- function() {
    currImgId <- get("currImgId", envir = .GlobalEnv)
    tpsDataList <- get("activeDataList", envir = .GlobalEnv)

	if(scaleMode) {
		alertBox("Please finish scale calculation")
	} else if(currImgId > 1) {
		#check if the landmark is enough
		dotNum <- digGetDotNum()
		landmarkNum <- get("landmarkNum", envir = .GlobalEnv)
		if(dotNum >= as.integer(landmarkNum)) {
			assign("currImgId", currImgId-1, envir = .GlobalEnv)
			digShowPicture()
		} else {
			alertBox("Incorrect number of landmarks")
		}

    } else {
        alertBox("It's the first specimen")
    }
}

getPicScale <- function(dot1,dot2, scale) {
	#get ratio
	id <- get("currImgId", envir = .GlobalEnv)
    tpsDataList <- get("activeDataList", envir = .GlobalEnv)
	ratio <- tpsDataList[[id]][[6]]
	fitImgH <- as.integer(tpsDataList[[id]][[7]])

	x1 <- as.double(dot1[1]*ratio)
	y1 <- as.double(fitImgH-dot1[2])*ratio
	x2 <- as.double(dot2[1]*ratio)
	y2 <- as.double(fitImgH-dot2[2])*ratio

    scale <- as.numeric(scale)

	return (scale/sqrt(sum((x2-x1)^2+(y2-y1)^2)))
}

onSetScaleOk <- function() {
	if(scaleDotNum < 2) {
		alertBox("Incorrect number of scale dots")
		return (0)
	}

	#get distance
	entry <- get("distanceEntry", envir = .GlobalEnv)
    distance <<- tclvalue(tkget(entry))

	#calculate scale factor
	scaleFactor <- getPicScale(scaleDot1, scaleDot2, distance)
	#print(paste("scale factor:",scaleFactor))
	
	#show scale factor
	scaleLabel <- get("scaleLabel", envir = .GlobalEnv)
	tkconfigure(scaleLabel, text = paste("Scale Factor: ", format(scaleFactor, digits = 5)))

	#update tpsDataList
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
	currImgId <- get("currImgId", envir = .GlobalEnv)
	if(tclvalue(applyToAll) == "yes") {
		for(i in 1:length(tpsDataList)) {
			tpsDataList[[i]][[2]] <- scaleFactor
			tpsDataList[[i]][[4]] <- tclvalue(scaleUnitVal)
		}
	} else {
		tpsDataList[[currImgId]][[2]] <- scaleFactor
		tpsDataList[[currImgId]][[4]] <- tclvalue(scaleUnitVal)
	}

	#clear variables
	scaleMode <<- 0
	scaleDot1 <<- c(0, 0)
	scaleDot2 <<- c(0, 0)
	scaleDotNum <<- 0
	assign("activeDataList", tpsDataList, envir = .GlobalEnv)

	win <- get("setScaleWin", envir = .GlobalEnv)
	tkdestroy(win)
}

onlandmarkNumOk <- function() {
	#get user input value
	entry <- get("landmarkEntry", envir = .GlobalEnv)
    landmarkNum <- tclvalue(tkget(entry))
	assign("landmarkNum", landmarkNum, envir = .GlobalEnv)

	# turn to the first picture
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
	assign("currImgId", 1, envir = .GlobalEnv)
	digShowPicture()

	win <- get("landmarkNumWin", envir = .GlobalEnv)
	tkdestroy(win)
}

createSetScaleWind <- function() {
	win <- tktoplevel()
	tkwm.title(win, "Set Scale")

	label_frame <- ttkframe(win)
	tkpack(label_frame, expand = TRUE, fill = "both", padx = 5, pady = 5)
	tkgrid.columnconfigure(label_frame, 0, weight = 1)
	tkgrid.columnconfigure(label_frame, 1, weight = 1)
	tkgrid.columnconfigure(label_frame, 2, weight = 1)
	tkgrid.columnconfigure(label_frame, 1, weight = 1)

	distancelabel = tklabel(label_frame, text='distance: ')
	distanceEntry = tkentry(label_frame, textvariable=tclVar("10"))
	tkgrid(distancelabel, row = 0, column = 0, sticky = "e")
	tkgrid(distanceEntry, row = 0, column = 1, sticky = "e")
	assign("distanceEntry", distanceEntry, envir = .GlobalEnv)

	unitlabel = tklabel(label_frame, text='unit: ')
	unitCombox <- ttkcombobox(label_frame, state = "readonly", values = c('inches', 'millimeters', 'centimeters'), textvariable = scaleUnitVal)
	tkgrid(unitlabel, row = 1, column = 0, sticky = "e")
	tkgrid(unitCombox, row = 1, column = 1, sticky = "e")
	assign("unitCombox", unitCombox, envir = .GlobalEnv)

	applyLabel = tklabel(label_frame, text='apply to all specimens')
	applyCombox <- ttkcombobox(label_frame, state = "readonly", values = c('yes', 'no'), textvariable = applyToAll)
	tkgrid(applyLabel, row = 2, column = 0, sticky = "e")
	tkgrid(applyCombox, row = 2, column = 1, sticky = "e")
	assign("applyCombox", applyCombox, envir = .GlobalEnv)

	noteLable <- tklabel(win, text='Note: Please place two dots for scale setting')
	tkpack(noteLable, expand = TRUE, fill = "both", padx = 5, pady = 5)

	btnFrame <- ttkframe(win)
	tkpack(btnFrame, fill = "x", padx = 5, pady = 5)
	cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() tkdestroy(win))
	okBtn <- ttkbutton(btnFrame, text = "ok",command = function() onSetScaleOk())

	tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "y", side = "left")
	sapply(list(cancelBtn, okBtn), tkpack, side = "left", padx = 6)

	assign("setScaleWin", win, envir = .GlobalEnv)
	tkfocus(win)
}

setScale <- function() {
	canvas <- get("activeCanvas", envir = .GlobalEnv)
	tkdelete(canvas, "scale")
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)

	if(length(tpsDataList) == 0) {
		alertBox("no picture open")
	} else {
		scaleMode <<- 1
		createSetScaleWind()		
	}
}

setLandmarkNum <- function() {
	win <- tktoplevel()
	tkwm.title(win, "Set Landmark Number")

	entryFrame <- ttkframe(win)
	tkpack(entryFrame, expand = TRUE, fill = "both", padx = 5, pady = 5)
	label = tklabel(entryFrame, text='Set landmark Number: ')
	landmarkVal <- get("landmarkNum", envir = .GlobalEnv)

    entry = tkentry(entryFrame, textvariable=tclVar(landmarkVal))
	sapply(list(label, entry), tkpack, side = "left", padx = 6)
	assign("landmarkEntry", entry, envir = .GlobalEnv)

	btnFrame <- ttkframe(win)
	tkpack(btnFrame, fill = "x", padx = 5, pady = 5)
	cancelBtn <- ttkbutton(btnFrame, text = "cancel", command = function() tkdestroy(win))
	okBtn <- ttkbutton(btnFrame, text = "ok",command = function() onlandmarkNumOk())

	tkpack(ttklabel(btnFrame, text = " "), expand = TRUE, fill = "y", side = "left")
	sapply(list(cancelBtn, okBtn), tkpack, side = "left", padx = 6)

	assign("landmarkNumWin", win, envir = .GlobalEnv)
	tkfocus(win)
}

savetoTpsFile <- function() {
    currImgId <- get("currImgId", envir = .GlobalEnv)
    tpsDataList <- get("activeDataList", envir = .GlobalEnv)

	#check if the landmark is enough
	dotNum <- digGetDotNum()
	landmarkNum <- as.integer(get("landmarkNum", envir = .GlobalEnv))
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
openSpecimens <- function() {
	#print("openSpecimens")
    fileStr <- tclvalue(tkgetOpenFile( filetypes = "{{image file} {.bmp .jpg .png .gif}}", multiple=TRUE))
    imgList <- unlist(strsplit(fileStr, " "))
    assign("currImgId", 1, envir = .GlobalEnv)
	nSpecimens <- length(imgList)
	
    if (nSpecimens != 0) {
		#initialize tpsDataList
        tpsDataList <- list()
        for(i in 1:length(imgList)){
			ratio <- getRatio(imgList[[i]])
			if(ratio == 0) { 
				nSpecimens <- nSpecimens-1
				next 
			}
		
			tpsDataList[[length(tpsDataList)+1]] <- list(imgList[[i]], 0, list(), "inches", list(), ratio, c(fitCanvasW, fitCanvasH))
        }
        assign("activeDataList", tpsDataList, envir = .GlobalEnv)
		assign("digData", tpsDataList, envir = .GlobalEnv)
		assign("currImgId", 1, envir = .GlobalEnv)

        digShowPicture()
		assign("zoom", 0, envir = .GlobalEnv)

		specimenNumLabel <- get("specimenNumLabel", envir = .GlobalEnv)
		tkconfigure(specimenNumLabel, text = paste("Number of Specimens: ", nSpecimens))

		#initialize
		digitizeInit()
    }
}

addScaleDot <- function(x, y) {
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
    currImgId <- get("currImgId", envir = .GlobalEnv)
	temp <- tpsDataList[[currImgId]][[5]]

	temp[[length(temp)+1]] <- c(x, y)

    return (length(temp))
}

updateDotList <- function(x, y, operate, moveDotId = 0, status="normal") {
    tpsDataList <- get("activeDataList", envir = .GlobalEnv)
    currImgId <- get("currImgId", envir = .GlobalEnv)
    temp <- tpsDataList[[currImgId]][[3]]
	statusList <- tpsDataList[[currImgId]][[5]]
    dotId <- getDotId(x, y)
    newAdded <- -1

    if (operate == "add") {
        if(dotId == 0) {
            temp[[length(temp)+1]] <- c(x, y)
			statusList[[length(statusList)+1]] <- status
            newAdded <- digGetDotNum(statusList)
        }
    } else if (operate == "move") {
        temp[[moveDotId]] <- c(x,y)
    } else if(operate == "remove") {
        #temp[dotId] <- NA
		statusList[dotId] <- "removed"
    }

    tpsDataList[[currImgId]][[3]] <- temp
	tpsDataList[[currImgId]][[5]] <- statusList

    assign("activeDataList", tpsDataList, envir = .GlobalEnv)
    return (newAdded)
}

dotAdd<-function(x, y) {
	tpsDataList <- get("activeDataList", envir = .GlobalEnv)
	if(length(tpsDataList) <= 0) {
		return ()
	}

    x<-as.integer(x)
    y<-as.integer(y)

	if(scaleMode) {
		scaleDotNum <<- scaleDotNum+1
		canvas <- get("activeCanvas", envir = .GlobalEnv)
		tkdelete(canvas, "scaleline")
        item <- tkcreate(canvas, "oval", x - 2, y - 2, x + 2, y + 2,
                         width=1, outline="black", fill="red")
		tkaddtag(canvas, "scale", "withtag", item)

		if(scaleDotNum == 1) {
			scaleDot1 <<- c(x,y)
		} else if(scaleDotNum == 2) {
			scaleDot2 <<- c(x,y)
			item <- tkcreate(canvas, "line", scaleDot1[1], scaleDot1[2], scaleDot2[1], scaleDot2[2], width=1, fill="red")
			tkaddtag(canvas, "scaleline", "withtag", item)
			scaleMode <<- 0
		}
	} else {
		fill <- "red"
		outline <- "black"
		dotStatus <- "normal"
		missLandmarkVar <- get("missLandmark", envir = .GlobalEnv)
		if (tclvalue(missLandmarkVar) == "1") {
			fill <- "black"
			outline <- "red"
			dotStatus <- "black"			
			updateMissLandMark(0)
		}
		res <- updateDotList(x, y, "add", status=dotStatus)

		if(res != -1) {
			canvas <- get("activeCanvas", envir = .GlobalEnv)
			item <- tkcreate(canvas, "oval", x - 6, y - 6, x + 6, y + 6,
							 width=1, outline=outline,
							 fill=fill)
			tkaddtag(canvas, "point", "withtag", item)

			#add label for this dot
			labelLandmarkVar <- get("labelLandmark", envir = .GlobalEnv)
			if((tclvalue(labelLandmarkVar) == "1")) {
				label(res, x, y)
			}

			dotNum <- digGetDotNum()
			landMarkNumLabel <- get("landMarkNumLabel", envir = .GlobalEnv)
			tkconfigure(landMarkNumLabel, text = paste("Number of Landmarks: ", dotNum))
		}
	}
}

updateMissLandMark <-function(value) {
	missLandmarkVar <-tclVar(value)
	missLandmarkCheBtn  <- get("missLandmarkCheBtn", envir = .GlobalEnv)
	tkconfigure(missLandmarkCheBtn, variable=missLandmarkVar)
	assign("missLandmark", missLandmarkVar, envir = .GlobalEnv)
}
			
dotSelect<-function(x, y) {
	if(!scaleMode) {
		x <- as.numeric(x)
		y <- as.numeric(y)
		tkdtag(canvas, "selected")
		tkaddtag(canvas, "selected", "withtag", "current")
		tkitemraise(canvas,"current")
		dotId <- getDotId(x, y)
		assign("selectedDot", dotId, envir = .GlobalEnv)
		lastX <<- x
		lastY <<- y
	}
}

dotRelease <- function(x, y) {
	if(!scaleMode) {
		#redraw the label
		canvas <- get("activeCanvas", envir = .GlobalEnv)
		tkdelete(canvas, "label")
		onLabelLandMark()
		assign("selectedDot", 0, envir = .GlobalEnv)
	}
}

dotMove <- function(x, y) {
    ## This procedure is invoked during mouse motion events.
    ## It drags the current item.
    ##
    ## Arguments:
    ## x, y -    The coordinates of the mouse.
	if(!scaleMode) {
		x <- as.numeric(x)
		y <- as.numeric(y)

		tpsDataList <- get("activeDataList", envir = .GlobalEnv)
		currImgId <- get("currImgId", envir = .GlobalEnv)
		temp <- tpsDataList[[currImgId]][[3]]
		dotId <- get("selectedDot", envir = .GlobalEnv)

		if(dotId) {
			tkmove(canvas, "selected", x - temp[[dotId]][1], y -  temp[[dotId]][2])
			updateDotList(x, y, "move", dotId)
		}
	}
}

digRemoveDotOk <-function(x, y) {
	updateDotList(x, y, "remove")
	canvas <- get("activeCanvas", envir = .GlobalEnv)
	tkdelete(canvas, "point")
	tkdelete(canvas, "label")
	currImgId <- get("currImgId", envir = .GlobalEnv)
	showDots(currImgId)

	#redraw the label
	onLabelLandMark()

	#update landmark number
	dotNum <- digGetDotNum()
	landMarkNumLabel <- get("landMarkNumLabel", envir = .GlobalEnv)
	tkconfigure(landMarkNumLabel, text = paste("Number of Landmarks: ", dotNum))

	win <- get("removeWin", envir = .GlobalEnv)
	tkdestroy(win)
}

digGetDotNum <- function(statusList = "") {
	#check if the landmark is enough 
	if(statusList == "") {
		imgId <- get("currImgId", envir = .GlobalEnv)
		tpsDataList <- get("activeDataList", envir = .GlobalEnv)
		statusList <- tpsDataList[[currImgId]][[5]]	
	}
	
	len <- 0
	if(length(statusList)) {
		for(i in 1:length(statusList)) {
			if(statusList[[i]] != "removed") {len = len+1}		
		}	
	}
	return (len)
}

dotRemove <-function(x, y) {

	if(!scaleMode) {
		popUpRemoveWindow(x, y, 'Do you want to delete this landmark?', "digdot")
	}
}