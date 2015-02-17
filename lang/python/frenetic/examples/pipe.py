# An application that drops all traffic, but sets up a pipe.
import frenetic
from frenetic.syntax import *

app = frenetic.App()
app.update(Filter(Test(Location(Physical(1)))) >> Mod(Location(Pipe("foo"))))
app.start_event_loop()

