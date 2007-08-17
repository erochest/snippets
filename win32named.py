

"""\
usage: win32named.py [client|server]
"""

# this is an example of creating a client/server using win32 named pipes for
# ipc.
# i took this from
# http://mail.python.org/pipermail/python-list/2005-March/314328.html
#
# for a c# implementation, see
# http://www.codeproject.com/cs/threads/dotnetnamedpipespart1.asp
# and
# http://www.codeproject.com/cs/threads/dotnetnamedpipespart2.asp


from ctypes import *


GENERIC_READ = 0x80000000
GENERIC_WRITE = 0x40000000
OPEN_EXISTING = 0x3
PIPE_ACCESS_DUPLEX = 0x3
PIPE_TYPE_MESSAGE = 0x4
PIPE_READMODE_MESSAGE = 0x2
PIPE_WAIT = 0x0
PIPE_UNLIMITED_INSTANCES = 255
BUFSIZE = 4096
NMPWAIT_USE_DEFAULT_WAIT = 0
INVALID_HANDLE_VALUE = -1
ERROR_PIPE_CONNECTED = 535
ERROR_PIPE_BUSY = 231
ERROR_MORE_DATA = 234

CLIENT_MESSAGE = "Default message from client\0"
SRV_MESSAGE = 'Default answer from server\0'
PIPENAME = r'\\.\pipe\pythontest'


def read_write_client_pipe_thread(pipe):
    buf = create_string_buffer(BUFSIZE)
    read = c_ulong(0)
    while True:
        success = windll.kernel32.ReadFile(
            pipe, buf, BUFSIZE, byref(read), None,
            )
        if success == 1 or read.value != 0:
            print buf.value
            written = c_ulong(0)
            success = windll.kernel32.WriteFile(
                pipe, c_char_p(SRV_MESSAGE), len(SRV_MESSAGE), byref(written),
                None,
                )
        else:
            break
        if (not success) or len(SRV_MESSAGE) != written.value:
            print "Could not reply to the client's request from the pipe."
            break
        else:
            print "Number of bytes written:", written.value
    windll.kernel32.FlushFileBuffers(pipe)
    windll.kernel32.DisconnectNamedPipe(pipe)
    windll.kernel32.CloseHandle(pipe)


def server():
    THREADFUNC = CFUNCTYPE(c_int, c_int)
    thread_func = THREADFUNC(read_write_client_pipe_thread)
    while True:
        pipe = windll.kernel32.CreateNamedPipeA(
            PIPENAME, PIPE_ACCESS_DUPLEX,
            PIPE_TYPE_MESSAGE | PIPE_READMODE_MESSAGE | PIPE_WAIT,
            PIPE_UNLIMITED_INSTANCES, BUFSIZE, BUFSIZE,
            NMPWAIT_USE_DEFAULT_WAIT, None,
            )
        if pipe == INVALID_HANDLE_VALUE:
            print 'Error creating named pipe'
            return 0
        connected = windll.kernel32.ConnectNamedPipe(pipe, None)
        if (connected == 0 and
            windll.kernel32.GetLastError() == ERROR_PIPE_CONNECTED):
            connected = 1
        if connected == 1:
            thread_id = c_ulong(0)
            thread = windll.kernel32.CreateThread(None, 0, thread_func, pipe,
                                                  0, byref(thread_id))
            if thread == -1:
                print 'Create thread failed'
                return 0
            else:
                windll.kernel32.CloseHandle(thread)
        else:
            print 'Could not connect to the pipe'
            windll.kernel32.CloseHandle(pipe)
    return 0


def client():
    while True:
        pipe = windll.kernel32.CreateFileA(
            PIPENAME, GENERIC_READ|GENERIC_WRITE, 0, None, OPEN_EXISTING,
            0, None,
            )
        if pipe != INVALID_HANDLE_VALUE:
            break
        else:
            print 'Invalid handle value'
        if windll.kernel32.GetLastError() != ERROR_PIPE_BUSY:
            print 'Could not open pipe'
            return
        elif windll.kernel32.WaitNamedPipeA(PIPENAME, 20000) == 0:
            print 'Could not open pipe'
            return
    mode = c_ulong(PIPE_READMODE_MESSAGE)
    success = windll.kernel32.SetNamedPipeHandleState(
        pipe, byref(mode), None, None,
        )
    if not success:
        print 'SetNamedPipeHandleState failed'
    written = c_ulong(0)
    success = windll.kernel32.WriteFile(
        pipe, c_char_p(CLIENT_MESSAGE), len(CLIENT_MESSAGE), byref(written),
        None,
        )


def main():
    if len(sys.argv) != 2 or '-h' in sys.argv or '--help' in sys.argv:
        print __doc__
        return
    if sys.argv[1] == 'server':
        server()
    elif sys.argv[1] == 'client':
        client()
    else:
        print __doc__
        return

if __name__ == '__main__':
    main()


