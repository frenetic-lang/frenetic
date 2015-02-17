# An application that drops all traffic.
import frenetic
from frenetic.syntax import *

class MyApp(frenetic.App):
  pass

app = MyApp()
app.update(drop)
app.start_event_loop()