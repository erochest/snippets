
# copied from
# http://peadrop.com/blog/2007/06/18/pickle-an-interesting-stack-language/
# to experiment with pickles

import code
import pickle
import sys

sys.ps1 = "pik> "
sys.ps2 = "...> "
banner = "Pik -- The stupid pickle loader.\nPress Ctrl-D to quit."

class PikConsole(code.InteractiveConsole):
    def runsource(self, source, filename="<stdin>"):
        if not source.endswith(pickle.STOP):
            return True  # more input is needed
        try:
            print repr(pickle.loads(source))
        except:
            self.showsyntaxerror(filename)
        return False

pik = PikConsole()
pik.interact(banner)


