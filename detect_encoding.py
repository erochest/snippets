
"""\
I stole this from
http://www.voidspace.org.uk/python/weblog/arch_d7_2008_03_22.shtml#e951 He
developed this to sniff source file encoding for the tokenize standard module.
The somewhat odd interface for this function is because of its use by that
module.

tags: python3 unicode utf8

"""


import re
from codecs import lookup


cookie_re = re.compile(r'coding[:=]\s*([-\w.]+)')


def detect_encoding(readline):
    """\
    The detect_encoding() function is used to detect the encoding that should
    be used to decode a Python source file. It requires one argument, a
    readline function.

    It will call readline a maximum of twice, and return the encoding used and
    a list of any lines it has read in.

    It detect the encoding from the presence of a utf-8 bom or an encoding
    cookie as specified in pep-0263. If both a bom and a cookie are present,
    but disagree, a SyntaxError will be raised.

    If no encoding is specified, then the default of 'utf-8' will be returned.

    """

    utf8_bom = '\xef\xbb\xbf'
    bom_found = False
    encoding = None

    def read_or_stop():
        try:
            return readline()
        except StopIteration:
            return ''

    def find_cookie(line):
        try:
            line_string = line.decode('ascii')
        except UnicodeDecodeError:
            pass
        else:
            matches = cookie_re.findall(line_string)
            if matches:
                encoding = matches[0]
                if bom_found and lookup(encoding).name != 'utf-8':
                    # This behaviour mimics the Python interpreter
                    raise SyntaxError('encoding problem: utf-8')
                return encoding

    first = read_or_stop()
    if first.startswith(utf8_bom):
        bom_found = True
        first = first[3:]
    if not first:
        return ('utf-8', [])

    encoding = find_cookie(first)
    if encoding:
        return (encoding, [first])

    second = read_or_stop()
    if not second:
        return ('utf-8', [first])

    encoding = find_cookie(second)
    if encoding:
        return (encoding, [first, second])

    return ('utf-8', [first, second])

