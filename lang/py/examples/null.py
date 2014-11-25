from ryu.lib.packet import packet
import base64
from netkat import webkat
from netkat.syntax import *

class NullApp(webkat.App):
  def switch_up(self,switch_id):
    webkat.update(drop())


if __name__ == '__main__':
    webkat.client_id = "null"
    NullApp().start()
    webkat.start()