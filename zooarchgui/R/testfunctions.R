rm(list = ls())
source("C:/Users/eotarola/Box/Box_LCA/Documents/Research/zooarchGUI/zooarchgui/R/ConfigureFrame.r")
source("C:/Users/eotarola/Box/Box_LCA/Documents/Research/zooarchGUI/zooarchgui/R/Menu.r")
source("C:/Users/eotarola/Box/Box_LCA/Documents/Research/zooarchGUI/zooarchgui/R/zooarchGUI.r")
source("C:/Users/eotarola/Box/Box_LCA/Documents/Research/zooarchGUI/zooarchgui/R/File.r")
source("C:/Users/eotarola/Box/Box_LCA/Documents/Research/zooarchGUI/zooarchgui/R/MultivariateStatistics.r")

library(tcltk2)
library(zooaRchGUI)
name<-"multivarfit"
comboNum=0
dir="ver"
e <- new.env()
class(e) <- name
# createConfigureFrame(e, comboNum, dir)


e$okLabel <- "ok"
e$cancelLabel <- "cancel"

e$wnd <- tktoplevel()
tkwm.geometry(e$wnd, "300x300+300+300")
frame <- ttkframe(e$wnd, padding = c(3,3,12,12))
tkpack(frame, expand = TRUE, fill = "both")
e$layout <- ttklabelframe(frame, padding = 10)
tkpack(e$layout, expand = TRUE, fill = "both", padx = 5, pady = 5)
if (comboNum > 0) {
  comboxLayout(e, comboNum, dir)
}

layout(e);

#Draw OK Cancel Button
button_frame <- ttkframe(frame)
cancel_button <- ttkbutton(button_frame, text = e$cancelLabel, command=function() tkdestroy(e$wnd))
ok_button <- ttkbutton(button_frame, text = e$okLabel, command=function() run(e))
tkpack(button_frame, fill = "x", padx = 5, pady = 5)
tkpack(ttklabel(button_frame, text = " "), expand = TRUE, fill = "y", side = "left")
sapply(list(cancel_button, ok_button), tkpack, side = "left", padx = 6)
tkbind("TButton", "<Return>", function(W) tcl(W, "invoke"))
tkfocus(e$wnd)


