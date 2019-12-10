# This is just an example to get you started. A typical binary package
# uses this file as the main entry point of the application.

import nigui

when isMainModule:
  echo("AoC Day 7 graphical representation")
  
  app.init()

  var win = newWindow()
  var mainContainer = newLayoutContainer(Layout_Horizontal)
  win.add(mainContainer)
  var controlContainer = newLayoutContainer(Layout_Vertical)
  mainContainer.add(controlContainer)
  var stepButton = newButton("Step")
  controlContainer.add(stepButton)
  var runButton = newButton("Run")
  controlContainer.add(runButton)
  var stopButton = newButton("Stop")
  controlContainer.add(stopButton)
  var quitButton = newButton("Quit")
  controlContainer.add(quitButton)
  win.show()
  app.run()

