tt <- tktoplevel()
tkpack(l1<-tklabel(tt,text="\nPlease Close the open file"))


tt2 <- tktoplevel()
nb <- tk2notebook(tt2, tabs = c("Test", "Button"))
tkpack(nb, fill = "both", expand = 1)
tb1 <- tk2notetab(nb, "Test")

lab <- tk2label(tb1, text = "Open File")
tkpack(lab)
tb2 <- tk2notetab(nb, "\n\nPlease close the open file and ckick the button to continue\n\n")
but <- tk2button(tb2, text = "Click me", command = function() tkdestroy(tt2))
tkgrid(but)
tk2notetab.select(nb, "Button")
tk2notetab.text(nb) # Text of the currently selected tab

tcltk.test <- function(x1=1:10, x2=10:1) {
 library("tcltk")
# define first toplevel-widget
 tt           <- tktoplevel()
 tktitle(tt)  <- "Diagnostics"
 label.widget <- tklabel(tt, text="Choose data for plot!")
 rbut.wid1    <- tkradiobutton(tt, text="x1", value="0", variable="choice")
 rbut.wid2    <- tkradiobutton(tt, text="x2", value="1", variable="choice")
 but.done     <- tkbutton(tt, text="FINISHED", command=function(){
                                                         tclVar$done <- "T"
                                                         tkdestroy(tt)
                                                       } )
 tkpack(label.widget, rbut.wid1, rbut.wid2, but.done)
# wait until FINISHED is pressed
 tclVar$choice <- "0"
 tkwait.variable("done")
# plot x1 or x2
 if(tclVar$choice == "0") x <- x1
 if(tclVar$choice == "1") x <- x2
 if(is.null(names(x))) names(x) <- x
 plot(x)
# define second toplevel widget
 tt2          <- tktoplevel()
 tktitle(tt2) <- "Action"
 but.wid21    <- tkbutton(tt2, text="print summary",
                          command=function()print(summary(x)))
 but.wid22    <- tkbutton(tt2, text="identify outlier",
                          command=function()identify(x))
 but.wid23    <- tkbutton(tt2, text="exit", command=function(){
                                                      tclVar$done<-"T"
                                                      tkdestroy(tt2)
                                                    } )
 tkpack(but.wid21, but.wid22, but.wid23)
# wait until exit is pressed
 tclVar$done <- "F"
 tkwait.variable("done")
}

