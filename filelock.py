

from __future__ import with_statement

from contextlib import contextmanager
import getopt
import os
import pywintypes
import sys
import threading
import time
import win32con
import win32file


FILENAME = r'C:\home\eric\tmp\locktest.txt'

LOCK_EX = win32con.LOCKFILE_EXCLUSIVE_LOCK
LOCK_SH = 0
LOCK_NB = win32con.LOCKFILE_FAIL_IMMEDIATELY
__overlapped = pywintypes.OVERLAPPED()


def lock(file, flags):
    hfile = win32file._get_osfhandle(file.fileno())
    win32file.LockFileEx(hfile, flags, 0, -0x7fff0000, __overlapped)


def unlock(file):
    hfile = win32file._get_osfhandle(file.fileno())
    win32file.UnlockFileEx(hfile, 0, -0x7fff0000, __overlapped)


@contextmanager
def lockedfile(filename):
    f = open(filename, 'a+')
    lock(f, LOCK_EX)
    yield f
    f.close()


def test(name):
    print name, 'locking'
    with lockedfile(FILENAME) as f:
        print name, 'acquired lock'
        f.seek(0, 2)
        f.write('%s locking at %s\n' % (name, time.asctime()))
        time.sleep(60)
        f.seek(0, 2)
        f.write('%s unlocking at %s\n' % (name, time.asctime()))
        f.flush()
        print name, 'unlocking'


def delayed_call(wait, f, *args, **kwargs):
    time.sleep(wait)
    return f(*args, **kwargs)


def main():
    if os.path.exists(FILENAME):
        print 'removing'
        os.remove(FILENAME)
    print 'creating threads'
    t0 = threading.Thread(target=test, args=('primum',))
    t1 = threading.Thread(target=delayed_call, args=(15, test, 'secundum'))
    print 'starting'
    t0.start()
    t1.start()
    print 'joining'
    t0.join()
    t1.join()
    print 'done'
    os.startfile(FILENAME)


if __name__ == '__main__':
    main()

