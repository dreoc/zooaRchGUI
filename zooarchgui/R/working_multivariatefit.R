# Multivariate Vector-fitting Function
layout.multivarfit<-function(e){
  matrix.fun<-function()c(unlist(lapply(c(ls(envir = .GlobalEnv),ls("package:zooaRchGUI")), function(matrix) if(class(get(matrix))[1] == "matrix")c(unlist(matrix)))), "Load User File")

  e$dataName<-tclVar("Choose Ordination")
  e$dataName2<-tclVar("Choose Variables")
  e$dataFrame<-tclVar("NULL")
  e$dataFrame2<-tclVar("NULL")
  e$permutations<-tclVar(999)
  e$choice<-tclVar(2)

  #Begin GUI setup
  tkwm.title(e$wnd, "zooaRchGUI")
  tkconfigure(e$layout, text = "Multivariate Association Using Vector Fitting")
  columnConfig(e$layout)

  #First Data Combobox
  put_label(e$layout, "Ordination:",0,0,sticky="w")
  data_combo <- ttkcombobox(e$layout, state = "readonly",
                            values = c(matrix.fun()[-length(matrix.fun())],dfs.fun()),
                            textvariable = e$dataName)
  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2, pady = 2)
  tkbind(data_combo, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName))
  tkfocus(data_combo)

  tkgrid(data_combo, row = 0, column = 1, sticky="w", padx = 2, pady = 2)
  # tkbind(data_combo, "<<ComboboxSelected>>", function(){
  #          updateDataFrame(e, e$dataName)
  #
  #          e$inputs = (2:ncol(e$dataFrame))
  #          tkdelete(e$list1, 0, "end")
  #          for (input in e$inputs){
  #              tkinsert(e$list1, "end", input)
  #          }
  #          tkselection.set(e$list1, 0)
  # })
  # tkfocus(data_combo)

  #Second Data Combobox
  put_label(e$layout, "Dataset:",2,0,sticky="w")
  data_combo2 <- ttkcombobox(e$layout, state = "readonly",
                             values = c(matrix.fun()[-length(matrix.fun())],dfs.fun()),
                             textvariable = e$dataName2)
  tkgrid(data_combo2, row = 2, column = 1, sticky="w", padx = 2, pady = 2)
  tkbind(data_combo2, "<<ComboboxSelected>>", function(){
    updateDataFrame(e, e$dataName)

    e$inputs = (colnames(e$dataFrame))
    tkdelete(e$list1, 0, "end")
    for (input in e$inputs){
      tkinsert(e$list1, "end", input)
    }
    tkselection.set(e$list1, 0)
  })
  tkfocus(data_combo2)

  # tkbind(data_combo2, "<<ComboboxSelected>>", function() updateDataFrame(e, e$dataName2))
  # tkfocus(data_combo2)

  #Choice Combobox
  put_label(e$layout, "Variables:", 3, 0, sticky = "w")
  e$list1 <- smartListbox(e$layout, colnames(e$dataFrame))
  #choice_combo<-ttkcombobox(e$layout, state = "readonly", values = (2:ncol(e$dataFrame)), textvariable = e$choice)
  tkgrid(e$list1, row = 3, column = 1, sticky = "w", padx = 2, pady = 2)
  tkfocus(e$list1)

  #Choice Combobox
  put_label(e$layout, "N Axes:", 4, 0, sticky = "w")
  e$list1 <- smartListbox(e$layout, (2:ncol(e$dataFrame)))
  #choice_combo<-ttkcombobox(e$layout, state = "readonly", values = (2:ncol(e$dataFrame)), textvariable = e$choice)
  tkgrid(e$list1, row = 4, column = 1, sticky = "w", padx = 2, pady = 2)
  tkfocus(e$list1)

  #Permutation Slider
  put_label(e$layout, "Permutations:", 7, 0, sticky = "w")
  conf_level_frame <- ttkframe(e$layout)
  tkgrid(conf_level_frame, row = 7, column = 1, columnspan = 2,
         sticky = "w")
  conf_level_scale <- ttkscale(conf_level_frame,
                               from = 99, to = 10000,
                               variable = e$permutations)
  #Permutation Spinbox
  tkspinbox <- function(parent, ...)
    tkwidget(parent, "tk::spinbox", ...)
  conf_level_spin <- tkspinbox(conf_level_frame,
                               from = 99, to = 10000, increment = 1,
                               textvariable = e$permutations, width = 5)
  tkpack(conf_level_scale, side = "left")
  tkpack(conf_level_spin, side = "left")
}
