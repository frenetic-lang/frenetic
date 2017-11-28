# An application that drops all traffic, but sets up a pipe.
import frenetic
from frenetic.syntax import *

app = frenetic.App()
app.update(Filter(PortEq(1)) >> SendToController("foo"))
app.start_event_loop()

