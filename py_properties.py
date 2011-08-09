
# This is a short-hand syntax for defining properties using the f{get,set,del}
# methods. I found this originally in a vim snipmate file, and tracking it
# down, found an example and explanation at
# http://adam.gomaa.us/blog/2008/aug/11/the-python-property-builtin/.

def Property(func):
    """This passes the output of func (a dict) to property. """
    return property(**func())

class ClassName(object):
    """docstring for ClassName"""
    def __init__(self, arg):
        super(ClassName, self).__init__()
        self.arg = arg

    @Property
    def foo():
        doc = "The foo property."

        def fget(self):
            return self._foo
        def fset(self, value):
            self._foo = value
        def fdel(self):
            del self._foo

        return locals()
        

