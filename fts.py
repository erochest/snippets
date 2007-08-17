

# This is adapted from
# http://eigenclass.org/hiki.rb?simple+full+text+search+engine


from __future__ import with_statement

import codecs
from contextlib import closing
import csv
import os
import re
from zipfile import ZipFile, ZIP_DEFLATED


__version__ = '$Revision:$'


MAX_WORD_LEN = 30


class FullTextSearch(object):
    re_word_start = re.compile(ur'''
            \b \w
        ''',
        re.VERBOSE|re.UNICODE,
        )

    def __init__(self):
        self.full_text = []
        self.suffixes = []

    def add_document(self, name, contents):
        self.full_text.append( (name, contents.replace(u'\0', u' ')) )

    def add_file(self, name, encoding='utf-8'):
        name = os.path.abspath(name)
        with codecs.open(name, 'r', encoding) as f:
            self.full_text.append( (name, f.read()) )

    def save_full_text(self, filename):
        with closing(ZipFile(filename, 'w', ZIP_DEFLATED)) as zip:
            for (name, text) in self.full_text:
                zip.writestr(name, text.encode('utf-8'))

    def load_full_text(self, filename):
        self.full_text = []
        with closing(ZipFile(filename, 'r', ZIP_DEFLATED)) as zip:
            for name in zip.namelist():
                text = zip.read(name).decode('utf-8')
                self.full_text.append( (name, text) )

    def save_index(self, filename):
        with open(filename, 'wb') as f:
            writer = csv.writer(f)
            writer.writerows(self.suffixes)

    def load_index(self, filename):
        with open(filename, 'rb') as f:
            reader = csv.reader(f)
            self.suffixes = [ (int(r[0]), int(r[1])) for r in reader if r ]

    def save(self, basename):
        self.save_full_text(basename+'.zip')
        self.save_index(basename+'.csv')

    def load(self, basename):
        self.load_full_text(basename+'.zip')
        self.load_index(basename+'.csv')

    def index(self):
        self.suffixes = []
        for (i, (_, text)) in enumerate(self.full_text):
            self.suffixes += ( (i, s) for s in self.__find_suffixes(text) )
        self.suffixes.sort(key=self.__sort_key)

    def __find_suffixes(self, text):
        return ( m.start() for m in self.re_word_start.finditer(text) )

    def __sort_key(self, x):
        return self.full_text[x[0]][1][x[1]:x[1]+MAX_WORD_LEN]

    def __binary_search(self, term, start, end):
        middle = (start + end) / 2
        (i, pos) = self.suffixes[middle]
        if self.full_text[i][1].startswith(term, pos):
            return middle
        elif start == end:
            return None
        pivot = self.full_text[i][1][pos:pos+MAX_WORD_LEN]
        if term <= pivot:
            return self.__binary_search(term, start, middle)
        elif term > pivot:
            return self.__binary_search(term, middle+1, end)

    def search(self, term):
        hit = self.__binary_search(term, 0, len(self.suffixes)-1)
        while hit is not None:
            (i, pos) = self.suffixes[hit]
            if self.full_text[i][1].startswith(term, pos):
                name = self.full_text[i][0]
                yield (i, name, pos)
                hit += 1
            else:
                hit = None


def _test():
    fts = FullTextSearch()
    print 'loading documents'
    for (root, dirs, files) in os.walk(r'C:\corpora\sample\text'):
        for fn in files:
            fts.add_file(os.path.join(root, fn))
    print 'indexing'
    fts.index()
    for term in ('enron', 'Enron', 'Abramhoff', 'Reed'):
        print '>>> searching for', term
        for (i, name, pos) in fts.search(term):
            print '\t', name, '@', pos
            print '\t', ' '.join(fts.full_text[i][1][pos-20:pos+21].split())
            print
    print 'done.'


if __name__ == '__main__':
    import pdb
    _test()

