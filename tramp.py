

# From PEP 342, a generator/coroutine based task scheduler.


import collections
import types


class Trampoline(object):
    """Manage communications between coroutines"""

    running = False

    def __init__(self):
        self.queue = collections.deque()

    def add(self, coroutine):
        """Request that a coroutine be executed"""
        print 'Trampoline.add(%r)' % (coroutine,)
        self.schedule(coroutine)

    def run(self):
        print 'Trampoline.run()'
        result = None
        self.running = True
        try:
            while self.running and self.queue:
                func = self.queue.popleft()
                result = func()
            return result
        finally:
            self.running = False

    def stop(self):
        print 'Trampoline.stop()'
        self.running = False

    def schedule(self, coroutine, stack=(), v=None, *exc):
        print 'Trampoline.schedule(%r, %r, %r, *%r)' % (coroutine, stack, v, exc)
        def resume():
            print 'resume()'
            try:
                if exc:
                    value = coroutine.throw(v, *exc)
                else:
                    value = coroutine.send(v)
            except:
                value = v
                if stack:
                    # Send the error back to the "caller"
                    self.schedule(stack[0], stack[1], *sys.exc_info())
                else:
                    # Nothing left in this pseudothread to handle it, let it
                    # propagate to the run loop.
                    raise
            if isinstance(value, types.GeneratorType):
                # Yielded to a specific coroutine, push the current one on the
                # stack, and call the new one with no args.
                self.schedule(value, (coroutine, stack))
            elif stack:
                # Yielded a result, pop the stack and send the value to the
                # caller.
                self.schedule(stack[0], stack[1], value)
            # else: This pseudothread has ended.
        self.queue.append(resume)

# An example echo server, also from the PEP.
#
# This presumes the existence of "nonblocking_read", "nonblocking_write", and
# other I/O coroutines that, e.g., raise ConnectionLost if the connection is
# closed).

# Here are some implementations of the nonblocking functions for testing
# purposes.
def listening_socket(host, service):
    print '>>> listening_socket(%r, %r)' % (host, service)
    return 0

def nonblocking_accept(sock):
    print '>>> nonblocking_accept(%r)' % (sock,)
    sock += 1
    if sock < 5:
        yield sock

def nonblocking_read(sock):
    print '>>> nonblocking_read(%r)' % (sock,)
    return str(sock)

def nonblocking_write(sock, data):
    print 'nonblocking_write(%r, %r)' % (sock, data)

class ConnectionLost(Exception):
    pass

# Coroutine function that echos data back on a connected socket.
def echo_handler(sock):
    while True:
        try:
            data = (yield nonblocking_read(sock))
            yield nonblocking_write(sock, data)
        except ConnectionLost:
            pass

# Coroutine function that listens for connections on a socket, and then
# launches a service "handler" coroutine to service the connection.
def listen_on(trampoline, sock, handler):
    for i in xrange(10):
        print '>>> listening', i, '...'
        # Get the next incoming connection
        connected_socket = (yield nonblocking_accept(sock))
        # Start another coroutine to handle the connection.
        trampoline.add( handler(connected_socket) )

# Run the echo server.
def echo():
    # Create a scheduler to manage all our coroutines.
    t = Trampoline()
    # Create a coroutine instance to run the echo_handler on incoming
    # connections.
    server = listen_on(
        t, listening_socket("localhost", "echo"), echo_handler,
        )
    # Add the coroutine to the scheduler.
    t.add(server)
    # Loop forever, accepting connections and servicing them "in parallel."
    t.run()

echo()

