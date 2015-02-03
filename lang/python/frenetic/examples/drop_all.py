# An application that drops all traffic.
from frenetic import app
from frenetic.syntax import *


class MyApp(app.App):
  pass

MyApp().start()
app.update(drop)
app.start()

