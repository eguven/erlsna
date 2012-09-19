#!/usr/bin/env python3

"""Python3 driver for erlsna"""

import socket
import sys
import random
import math
import time
import datetime
import os

import subprocess
import select

HOST = "localhost"
PORT = 8082

def new_sock(host=HOST,port=PORT):
    sock=socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect((host,port))
    print(sock.recv(1024))
    return sock

def do_import(path):
    """Import file to live erlsna system"""
    try:
        f = open(path, "r")
    except IOError:
        print("File not found: %s" % path)
        return
    sock = new_sock()

    line = f.readline().strip()
    while line:
        if not line.startswith("#"):
            msg = line+"\r\n"
            sock.send(msg.encode())
            sock.recv(1024)
        line = f.readline().strip()

    f.close()
    sock.close()
    print("Import Complete")

class ERLSNA(object):
    """An object providing API to erlsna system"""
    newline = "\r\n".encode('utf-8')

    def __init__(self,live=True):
        if live:
            self.sock = new_sock()
        else:
            self.sock = None

    def disconnect(self):
        if self.sock:
            self.sock.send("quit\r\n".encode('utf-8'))
            self.sock.close()
            self.sock = None

    def connect(self):
        if self.sock:
            print("Old socket was alive, killing it with fire!")
            self.sock.close()
            self.sock = new_sock()
        else:
            self.sock = new_sock()

    def __empty_socket(self,timeout=60):
        data = bytearray()
        # set timeout to 0 after first pass
        while True:
            readable, w, e = select.select([self.sock],[],[],timeout)
            if not readable: break
            data.extend(readable[0].recv(1024))
            timeout = 0
        return data

    def _send_or_return(self, b_array, with_response):
        if self.sock:
            b_array.extend(self.newline)
            # flush
            self.__empty_socket(timeout=0)
            self.sock.send(bytes(b_array))
            #resp = self.sock.recv(1024*16)
            resp = self.__empty_socket()
            if with_response:
                return resp.decode('utf-8')
        else:
            return b_array.decode('utf-8')

    def _parse_interval(self, t_start, t_end):
        if (t_start is not None and t_end and
                isinstance(t_start,int) and isinstance(t_end,int)):

            s = "%d %d" % (t_start, t_end)
        elif t_start and t_end:
            s = "%s %s" % (t_start, t_end)
        else:
            s = ""
        return s.encode('utf-8')

    def _is_graphfile_valid(self,fname):
        return (fname is not None and isinstance(fname,str) and
                (fname.endswith(".png") or fname.endswith(".PNG"))
               )

    def add_agent(self,aid,timestamp=None,data=None,with_response=False):
        b = bytearray()
        b.extend(("agent %s " % aid).encode('utf-8'))
        if data:
            for k,v in data.items():
                b.extend(("%s %s " % (k,v)).encode('utf-8'))
        if timestamp and isinstance(timestamp,int):
            b.extend(str(timestamp).encode('utf-8'))
        elif timestamp:
            b.extend(timestamp.encode('utf-8'))
        # send or return string
        return self._send_or_return(b, with_response)

    def relation(self, from_id, to_id, value, timestamp=None, adjust=False, with_response=False):
        b = bytearray()
        if adjust:
            b.extend(("adjust_relation %s %s %s " % (from_id, to_id, str(value))).encode('utf-8'))
        else:
            b.extend(("relation %s %s %s " % (from_id, to_id, str(value))).encode('utf-8'))
        if timestamp and isinstance(timestamp,int):
            b.extend(str(timestamp).encode('utf-8'))
        elif timestamp:
            b.extend(timestamp.encode('utf-8'))
        # send or return string
        return self._send_or_return(b, with_response)

    def delete_agent(self, aid, with_response=False):
        b = ("delete_agent %s\r\n" % aid).encode('utf-8')
        # send or return string
        return self._send_or_return(b, with_response)

    def degree_centrality(self, aid, t_start=None, t_end=None, with_response=True):
        b = bytearray()
        b.extend(("algorithm degree_centrality %s " % aid).encode('utf-8'))
        b.extend(self._parse_interval(t_start, t_end))
        return self._send_or_return(b, with_response)

    def engagement_analysis(self, aid, t_start=0, t_end=2147483647,
                            timestep=3600*24, with_response=True,
                            with_graph_png=None):
        b = bytearray()
        b.extend(("algorithm engagement_analysis %s " % aid).encode('utf-8'))

        b.extend(self._parse_interval(t_start, t_end))
        b.extend((" %d" % timestep).encode('utf-8'))
        retval = self._send_or_return(b, with_response)

        if with_response and self._is_graphfile_valid(with_graph_png):
            note = "(in %f days)" % (timestep/(3600*24))
            ResultParser.engagement_analysis(retval, aid, with_graph_png,
                                             note=note)
        elif with_response:
            return retval

    def relative_analysis(self, aid, timestep=3600*24, with_response=True,
                          with_graph_png=None):
        b = bytearray()
        b.extend(("algorithm relative_engagement %s %d" % (aid, timestep)).
                  encode('utf-8'))
        retval = self._send_or_return(b, with_response)
        if with_response and self._is_graphfile_valid(with_graph_png):
            note = "(in %f days)" % (timestep/(3600*24))
            ResultParser.relative_engagement(retval, aid, with_graph_png,
                                           note=note)
        elif with_response:
            return retval

    def subscribe(self, aid, opstr, value):
        """Preliminary method to subscribe:
        Notify when closeness centrality of agent becomes `opstr` `value`
        opstr -- "<" | ">"
        """
        supported_ops = ["<",">","=","*"]
        if opstr not in supported_ops:
            emsg = "supported opstr are %s" % supported_ops
            raise Exception(emsg)
        v = float(value)
        s = "subscribe %s %s %f" % (aid, opstr, v)
        b = bytearray(s.encode('utf-8'))
        print(self._send_or_return(b,True))
        print("Waiting notify, blocking...")
        while True:
            try:
                notify = self.sock.recv(128)
                if not notify:
                    print("Received empty, connection must have been closed")
                    break
                print("Received %s, blocking..." % notify.decode('utf-8'))
            except KeyboardInterrupt:
                print("Exiting...")
                break
        

class ResultParser(object):
    basepath = os.path.abspath(os.path.dirname(__file__))+"/"

    @classmethod
    def engagement_analysis(cls, result, aid, graph_png, note=" "):
        """Returned data is tagged following Erlang conventions eg:
        1324654231 in_diff 1 out_diff 2
        """
        R_graph_script = cls.basepath+"graph_script_2.R"
        tmpfile="/tmp/%s_data_%s"%(aid, datetime.datetime.utcnow().isoformat())
        with open(tmpfile,"wb") as f:
            f.write(result.encode('utf-8'))
        if 0 == subprocess.call(["Rscript",R_graph_script,tmpfile,
                                 graph_png,note]):
            print("Graph %s has been generated from data %s" %
                  (graph_png,tmpfile))
        else:
            print("Failed, permissions to create PNG?")

    @classmethod
    def relative_engagement(cls, result, aid, graph_png, note=" "):
        """Return data format as follows
        1334843540 i_network 474 o_network 450 i_agent 58 o_agent 17
        """
        R_graph_script = cls.basepath+"/graph_script_3.R"
        tmpfile="/tmp/%s_data_%s"%(aid, datetime.datetime.utcnow().isoformat())
        with open(tmpfile,"wb") as f:
            f.write(result.encode('utf-8'))
        if 0 == subprocess.call(["Rscript",R_graph_script,tmpfile,
                                 graph_png,note]):
            print("Graph %s has been generated from data %s" %
                  (graph_png,tmpfile))
        else:
            print("Failed, permission to create PNG?")

# some stuff to help during development
alphabet = ["P","R","S","T","U","V","W","X","Y","Z"]
agents = []

def random_name(): return "".join(random.sample(alphabet, 5))

def push(n,pn=1.3):
    """Push random data to erlsna"""
    sock = new_sock()
    sock.send("agent PRSTU\r\n".encode())
    sock.recv(1024)
    agents.append("PRSTU")
    for a in range(0,n):
        name = random_name()
        sock.send(("agent %s\r\n" % name).encode())
        sock.recv(1024)
        agents.append(name)
    for r in range(0,math.floor(math.pow(n,pn))):
        l = random.sample(agents, 2)
        msg = "relation %s %s 1\r\n"%tuple(l)
        sock.send(msg.encode())
        sock.recv(1024*8)
    print("DONE")
    sock.close()

