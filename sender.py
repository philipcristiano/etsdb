#!/usr/bin/env python

import socket
import time


CARBON_SERVER = '0.0.0.0'
CARBON_PORT = 2003

def run(seq):
    sock = socket.socket()
    sock.connect((CARBON_SERVER, CARBON_PORT))
    for i in seq:
        message = 'foo.bar.baz {0} {0}\n'.format(i)
        print 'sending message:\n%s' % message
        sock.sendall(message)
    sock.close()


if __name__ == '__main__':
    run(xrange(100000))
