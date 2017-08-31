# Readland Function
layout.readland<-function(e){
    #Data Model: enviroment called e
    e$datatype<-tclVar(".nts")

    tkwm.title(e$wnd, "readland_fun")
    tkconfigure(e$layout, text = "Read Landmark")

    columnConfig(e$layout)
    #Data Type Radiobuttons
    put_label(e$layout, "Type:", 1,0, sticky="w")
    rb_frame<-ttkframe(e$layout)
    sapply(c(".nts", ".tps", "multi.nts"), function(i) {
               radio_button<-tk2radiobutton(rb_frame, variable = e$datatype, text = i, value = i)
               tkpack(radio_button, side = "left")
               })
    tkgrid(rb_frame, row = 1, column = 1, sticky = "w")
}

#Ok Function
run.readland<-function(e) {
    switch(tclvalue(e$datatype),
           .nts = {name <- tclvalue(tkgetOpenFile(
                                                  filetypes = "{{NTS Files} {.nts}}"))
           if (name == "") return(data.frame());
           data <- as.array(readland.nts(name))
           file.name <- sub(x = basename(name), pattern = ".nts",replacement = "",ignore.case = TRUE)
           pos<-1
           envir <- as.environment(pos)
           assign(file.name, data, envir = envir)},
           .tps = {name <- tclvalue(tkgetOpenFile(
                                                  filetypes = "{{TPS Files} {.tps}}"))
           if (name == "") return(data.frame());
           data <- as.array(readland.tps(name))
           file.name <- sub(x = basename(name), pattern = ".tps",replacement = "",ignore.case = TRUE)
           pos<-1
           envir <- as.environment(pos)
           assign(file.name, data, envir = envir)},
           multi.nts = {name <- (choose.files(multi = TRUE, filters = as.matrix(c("NTS Files (*.nts)", "*.nts"))))
           if (name == "") return(data.frame());
           data <- as.array(readmulti.nts(name))
           pos<-1
           envir <- as.environment(pos)
           assign(x = "landmark.data", value = data, envir = envir)}
           )
    tkdestroy(e$wnd)
}

# Gpagen Function
layout.gpagen<-function(e){
    array.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(array) if(class(get(array))[1] == "array")c(unlist(array)))), "Load User File")
    matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))), "None","Load User File")

    #Data Model: enviroment called e
    e$dataName<-tclVar("Choose Data")
    e$dataName2<-tclVar("Choose Data")
    e$dataName3<-tclVar("Choose Data")
    e$array<-tclVar("NULL")
    e$curve<-tclVar("NULL")
    e$surface<-tclVar("NULL")
    e$type<-tclVar("ProcD")
    e$progress<-tclVar(1)

    tkwm.title(e$wnd, "gpagen_fun")
    tkconfigure(e$layout, text = "gpagen")

    columnConfig(e$layout)
    #Data Combobox
    put_label(e$layout, "Input Data:",0,0,sticky="w")
    data_combo <- ttkcombobox(e$layout, state = "readonly",
                              values = array.fun(),
                              textvariable = e$dataName)
    tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2)
    tkbind(data_combo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName, "array"))
    tkfocus(data_combo)

    #Curve Combobox
    put_label(e$layout, "Input Curve:",2,0,sticky="w")
    data_combo <- ttkcombobox(e$layout, state = "readonly",
                              values = c(matrix.fun(),dfs.fun()),
                              textvariable = e$dataName2)
    tkgrid(data_combo, row = 2, column = 1, sticky="w", padx = 2)
    if(tclvalue(e$dataName2) == "Choose Data" | tclvalue(e$dataName2) == "None" ){
      e$curve<-NULL
    }

    tkbind(data_combo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName2, "curve"))
    tkfocus(data_combo)

    #Surface Combobox
    put_label(e$layout, "Input Surface:",3,0,sticky="w")
    data_combo <- ttkcombobox(e$layout, state = "readonly",
                              values = c(matrix.fun(),dfs.fun()),
                              textvariable = e$dataName3)
    tkgrid(data_combo, row = 3, column = 1, sticky="w", padx = 2)
    if(tclvalue(e$surface) == "Choose Data" | tclvalue(e$surface) == "None" ){
      e$surface<-NULL
    }
    tkbind(data_combo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName3, "surface"))
    tkfocus(data_combo)

    #Bending or ProcD Radiobuttons
    put_label(e$layout, "Type:", 4,0, sticky="w")
    rb_frame<-ttkframe(e$layout)
    sapply(c("ProcD", "Bending Energy"), function(i) {
               radio_button<-tk2radiobutton(rb_frame, variable = e$type, text = i, value = i)
               tkpack(radio_button, side = "left")
                              })
    tkgrid(rb_frame, row = 4, column = 1, sticky = "w")

    #Plot Progress Checkbox
    put_label ( e$layout , "Plot Progress:" , 5 , 0, sticky = "w")
    progress_check <-ttkcheckbutton (e$layout , variable = e$progress)
    tkgrid (progress_check , row = 5 , column = 1 , sticky = "w" ,padx = 2)
}

#Ok Function
run.gpagen<-function(e) {
  #######################################################################
  # 8.7.2017 - EOC added to fix errors reading surface and curve files - BEGIN

  if (class(e$type)=="tclVar"){
    if(tclvalue(e$type) == "ProcD"){
      e$type<-TRUE
    } else{
      e$type<-FALSE
    }
  }

  if (class(e$curve)=="tclVar"){
    if(is.null(tclvalue(e$curve))){
      e$curve<-NULL
    }
  }

  if (class(e$curve)!="tclVar"){
    if(is.null(e$curve)){
      e$curve<-NULL
    }
  }

  if (class(e$surface)=="tclVar"){
    if(is.null(tclvalue(e$surface))){
      e$surface<-NULL
    } else if(tclvalue(e$surface)=="NULL"){
      e$surface<-NULL
    }
  }

  if (class(e$surface)!="tclVar"){
    if(is.null(e$surface)){
      e$surface<-NULL
    }
  }


  if(is.matrix(e$curve)==FALSE & is.null(e$curve)==FALSE){
    e$curve<-as.matrix(e$curve)
  }

  if(is.matrix(e$surface)==FALSE & is.null(e$surface)==FALSE){
    e$surface<-as.matrix(e$surface)
  }
  ############################-END-###########################################
    Y.gpa <- gpagen(e$array, curves = e$curve, surfaces = e$surface, ProcD = e$type, print.progress = as.numeric(tclvalue(e$progress)))
    summary(Y.gpa)
    plot(Y.gpa)
    pos<-1
    envir <- as.environment(pos)
    assign("gparesults", Y.gpa, envir = envir)
    tkdestroy(e$wnd)
}

digitize2D <-function() {
    e <- new.env()



    e$wnd <- tktoplevel(width=800, height=600)
    tktitle(e$wnd) <- "2D Digitizing"

	dotMainMenu(e)
    linkMainMenu(e)
	sliderMainMenu(e)

	mainFrame(e)

	#setwd("d:\\userdata\\slider_link")
	#initialize data
	sliderInit(e)
	linkInit(e)
	digitizeInit(e)

	e$activeDataList <- list()
	e$currImgId <- 1
	e$tab <- 0
}

