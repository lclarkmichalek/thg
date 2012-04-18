"""
hacker.protocol
===============

An implementation of the Hacker Games protocol in python.
"""

import socket
import json

PROTOCOL_VERSION = "0.2"

def recieve_connections(port, count):
    """
    A generator that accepts connections on the given port. Yields Connection
    objects of the sockets that connect.
    """
    sock = socket.socket()
    sock.bind((socket.gethostname(), port))
    sock.listen(count)
    for _ in range(count):
        con, addr = sock.accept()
        con.send(PROTOCOL_VERSION + "\n")
        yield Connection(con)

def connect(host, port):
    """
    Connects to the given address, and returns a Connection object that wraps
    the generated socket.
    """
    sock = socket.socket()
    sock.connect((host, port))
    con = Connection(sock)
    assert str(con.recieve_message()) == PROTOCOL_VERSION
    return con

class Connection():
    """
    Wraps a raw socket.
    """

    def __init__(self, socket):
        self.sock = socket
        self.flo = socket.makefile()

    def recieve_message(self):
        """
        Recieves a message from the connection. Raises ValueError if there
        is a problem parsing input.
        """
        line = self.flo.readline()
        return json.loads(line)

    def send_message(self, message):
        """
        Formats the given message according to the HGP, and then sends it over
        socket.
        """
        encoded = json.dumps(message)
        self.flo.write(encoded + "\n")
        self.flo.flush()
