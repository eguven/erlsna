#!/usr/bin/env python3
import nntplib
import pymongo
import datetime
import sys
import re
import erlsna

# FMT: '%a, %d %b %Y %H:%M:%S %z'

def get_mongodb():
    # default host:port
    MongoConn = pymongo.connection.Connection()
    return MongoConn["nntpdata"]

def groupname2collection(gname):
    """gmane.linux.kernel -> gmane_linux_kernel"""
    return gname.replace(".","_")

def collection2groupname(cname):
    """gmane_linux_kernel -> gmane.linux.kernel"""
    return cname.replace("_",".")

def parseEmailAddress(addr):
    """
    Parses an email such as "John Smith <jsmith@example.com>" and returns the jsmith@example.com part
    """
    #debug("parsing e-mail",addr,level=3)
    aman=addr
    #for p,r in [("<"," <"),(">","> "),("("," ("),(")",") ")]:
    #    aman=aman.replace(p,r)
    try:
        r=re.compile("^(.*?)[\\s<]*(\\S+@\\S+)[>\\s]*(.*?)$")
        n1,em,n2=r.findall(aman)[0]
        n=n1.strip()+" "+n2.strip()
        n=n.strip().lower()
        for p in ["(",")","\"","'"]:n=n.replace(p,"")
        em=em.replace(">","").replace("_REMOVE","").replace("REMOVE","").replace("/","")
        x1,x2=em.split("@")
    except:
        x1,x2,n=aman.strip(),"",""
    email=x1+"@"+x2
    #debug("Parsed email",addr,"to mail:",email,", name:",n,level=3)
    return email

def debug(*args,**kw):
    """
    Print debug info to stderr
    """
    if "progress" in kw: progress = kw["progress"]
    else:progress = False

    if progress: sys.stderr.write("\r")
    else: sys.stderr.write("\n")
    for a in args: sys.stderr.write(str(a)+" ")
    if not progress: sys.stderr.write("\n")

def getInput(query):
    tmp = input(query+" ")
    if tmp == "y":
        return True
    if tmp == "n":
        return False
    if tmp == "q":
        debug("Exiting")
        sys.exit(0)
    debug("Please answer in y/n/q")
    return getInput(query)

def datetime2timestamp(dt):
    delta = dt - datetime.datetime(1970,1,1)
    return delta.days*24*3600 + delta.seconds

def timestamp2datetime(ts):
    return datetime.datetime.utcfromtimestamp(ts)

def nntp_date2timestamp(dstr,as_datetime=False):
    # format without offset, python fail for broken %z
    fmt = "%a, %d %b %Y %H:%M:%S"
    final_dt = None
    if dstr.find("+") > 0:
        dt, offset = dstr.split(" +")
        d_base = datetime.datetime.strptime(dt,fmt)
        if not offset.startswith("0000"):
            delta = datetime.timedelta(hours=int(offset[:2]), minutes=int(offset[2:4]))
            final_dt = d_base + delta
        else:
            final_dt =  d_base
    elif dstr.find("-") > 0:
        dt, offset = dstr.split(" -")
        d_base = datetime.datetime.strptime(dt,fmt)
        if not offset.startswith("0000"):
            delta = datetime.timedelta(hours=int(offset[:2]), minutes=int(offset[2:4]))
            final_dt =  - delta
        else:
            final_dt =  d_base
    if as_datetime:
        return final_dt
    else:
        return datetime2timestamp(final_dt)


class NNTPCollector(object):
    patterns = {"date":"NNTP-Posting-Date:","sender":"From:","refs":"References:"}

    def __init__(self,*args,**kwargs):
        self.host = kwargs["host"]
        self.port = kwargs["port"]
        self.groupname = kwargs["groupname"]
        self.collection = get_mongodb()[groupname2collection(self.groupname)]
        self.collection.ensure_index([("article_no",pymongo.DESCENDING),
                                      ("sender",pymongo.ASCENDING)])

        #if not self._nntp:
        self.set_nntp()

        next_tmp = self.get_next_article_no(last_saved=True)
        if next_tmp:
            self._next_no = next_tmp
        else:
            self._next_no = self.get_next_article_no()

    def __str__(self):
        s = "NNTP Server %s:%d with Group: %s\nCount: %d First: %d Last: %d\nNextNo: %d\n"
        return s % (self.host, self.port, self.groupname, self._count,
                    self._first, self._last, self.get_next_article_no())

    def set_nntp(self):
        n = nntplib.NNTP(self.host, self.port, "", "", True)
        resp, count, first, last, name = n.group(self.groupname)
        self._nntp = n
        self._count = int(count)
        self._first = int(first)
        self._last = int(last)  

    def get_next_article_no(self,last_saved=False):
        if last_saved:
            doc = self.collection.find_one(sort=[("article_no",pymongo.DESCENDING)])
            return doc["article_no"]+1 if doc else None
        else:
            try:
                return self._next_no
            except:
                return self._first

    def inc_next_article_no(self):
        self._next_no += 1

    def group_has_more(self):
        return self.get_next_article_no() <= self._last

    def save_article(self, id, i, sender_email, tstamp, refs):
        doc = {"_id":id, "article_no":i, "sender": sender_email, "tstamp": tstamp, "refs": refs}
        self.collection.save(doc, safe=True)
        debug("Saved", i, self.groupname, progress=True)
        

    def collect(self):
        while self.group_has_more():
            i = self.get_next_article_no()
            try:
                resp, no, id, headerlist = self._nntp.head(str(i))
                # Section 3.6: http://tools.ietf.org/html/rfc3977
                id = id.decode("ascii")
                vals = {}
                for h in headerlist:
                    h = h.decode("utf-8")
                    for pk in self.patterns:
                        p=self.patterns[pk]
                        if h.find(p)==0: vals[pk]=h[len(p):].strip()
                refs=vals.get("refs","").split()
                sender_email=parseEmailAddress(vals["sender"])
                self.save_article(id,i,sender_email,vals["date"],refs)
                self.inc_next_article_no()

            except nntplib.NNTPTemporaryError as e:
                debug("NNTP error:",i,e)
                if e.response.startswith('423'.encode()): # 423 Bad article number
                    debug("> Skipping article",i)
                    self.inc_next_article_no()
            except UnicodeDecodeError as e:
                debug("Unicode error:",i,e)
                debug("> Skipping article",i)
                self.inc_next_article_no()
            
            except KeyboardInterrupt:
                debug("INTERRUPTED")
                return

    def dump(self, live=False):
        API = erlsna.ERLSNA(live=live)
        col = self.collection
        if live:
            API.add_agent(self.groupname,timestamp=1)
            for i in range(0,1+col.count()//100):
                cur = col.find(sort=[("article_no",pymongo.ASCENDING)],
                               skip=i*100,limit=100)
                for doc in cur:
                    ts = nntp_date2timestamp(doc["tstamp"])
                    sender = doc["sender"]
                    # multiple add_agent does not overwrite
                    API.add_agent(sender,timestamp=ts)
                    target = None
                    if doc["refs"]:
                        # get last reference's sender
                        target_doc = col.find_one({"_id":doc["refs"][-1]})
                        if target_doc: target = target_doc["sender"]
                        else: debug("Ref not found")
                    if target:
                        API.add_agent(target,timestamp=ts)
                        API.relation(sender, target, 1, timestamp=ts, adjust=True)
                    else:
                        API.relation(sender, self.groupname, 1, timestamp=ts, adjust=True)
            debug("Live push complete")
            return

        elif not live:
            with open(groupname2collection(self.groupname)+".graph") as f:
                pass

    def email_fixer(self):
        """This is a fix for a specific case where sender e-mail (From:)
        header is sent in two consecutive header lines eg:
            [ b'From: SomeSenderName',
              b' <some.email@domain.tld>
            ]
        User is queried for the fix.
        """
        
        infostr = "This script will iterate over the whole collection and find possibly corrupt 'sender' entries in documents, retrieve the article again and ask the user for action.\nANSWER in y/n/q\n"
        
        debug(infostr)
        col = self.collection
        total = self.collection.count()
        if not getInput("%d documents in collection, continue?" % total):
            sys.exit(0)

        self._fixer_run_fix()

    def _fixer_run_fix(self):
        col = self.collection
        #fixed_count = 0
        # batches of 100
        for batch in range(0,1+(col.count()//100)):
            cur = col.find(sort=[("article_no",pymongo.ASCENDING)],
                           skip=batch*100,limit=100)
            for doc in cur:
                if self._fixer_is_fix_required(doc):
                    old, new = self._fixer_get_fix(doc)
                    if getInput("Article ID: %d\nOld: %s\nNew: %s\nApply fix?" % (doc["article_no"], old, new)):
                        self._fixer_apply_fix_to_all(old, new)
                        # recurse if something is fixed
                        self._fixer_run_fix()
                        #doc["sender"] = new
                        #col.save(doc)
                        #fixed_count += 1
                        #debug("Saved with new email\n", new)
        #debug("Fixed %d documents." % fixed_count)

    def _fixer_get_fix(self,doc):
        resp, no, id, headerlist = self._nntp.head(str(doc["article_no"]))
        old = None
        new = None
        for h in headerlist:
            h = h.decode("utf-8")

            if old and new:
                return (old, new)
            if old and not new:
                # we got old one, this is the new
                new = parseEmailAddress(h)
                continue
            
            if h.find("From:")==0:
                old = parseEmailAddress(h[len("From:"):].strip())

    def _fixer_is_fix_required(self,doc):
        """Splitting '@' does not result in 2, non-empty strings"""
        l = doc["sender"].split("@")
        return (len(l) != 2 or not l[0] or not l[1])

    def _fixer_apply_fix_to_all(self,old,new):
        """Update all documents matching for a particular fix"""
        col = self.collection
        count = col.find({"sender":old}).count()
        col.update({"sender":old},{"$set":{"sender":new}},safe=True,multi=True)
        debug("Fixed %d documents\n(%s -> %s)\n" % (count, old, new))

    def list_collections(self):
        """List groups currently maintained"""
        _db = self.collection.database
        names = _db.collection_names()
        names.remove("system.indexes")
        fixed_names = [collection2groupname(colname) for colname in names]
        for n in fixed_names:
            debug(n)


def main():
    helps = ["-h","--help","help"]
    args = sys.argv[1:]
    if args and args[0] == "list":
        n.list_collections()
        return

    if not args or len(args) != 3 or args[0] in helps:
        debug("Usage: %s <nntp-server> <group-name> [collect|emailfixer|livedump]" % sys.argv[0])
        debug("Usage: %s [help|list]\n" % sys.argv[0])
        return

    if ":" in args[0]:
        host, port = args[0].split(":")[0], int(args[0].split(":")[1])
    else:
        host, port = args[0], 119

    groupname = args[1]
    n = NNTPCollector(host=host, port=port, groupname=groupname)
    print("\n"+str(n))

    if args[2] == "collect":
        n.collect()

    if args[2] == "emailfixer":
        n.email_fixer()

    if args[2] == "livedump":
        n.dump(live=True)


def fixxer1():
    col = get_mongodb()[groupname2collection("gmane.comp.lang.erlang.general")]
    count = 1
    for i in range(0,90):
        cur = col.find(sort=[("article_no",pymongo.ASCENDING)],
                       skip=i*100,limit=101) # not sure how limit worked, better be safe
        for doc in cur:
            s = "".join(doc["sender"].split(">")[:-1])+">"
            doc["sender"] = parseEmailAddress(s)
            print(str(count), doc["sender"])
            col.save(doc)
            count +=1

def printer():
    col = get_mongodb()[groupname2collection("gmane.comp.lang.erlang.general")]
    count = 0
    for i in range(0,150):
        cur = col.find(sort=[("article_no",pymongo.ASCENDING)],
                       skip=i*100,limit=100) #,fields={"sender":1,"_id":0})
        for doc in cur:
            l = doc["sender"].split("@")
            if len(l) != 2 or not l[0] or not l[1]:
                print(doc,"\n")
        #x = input("ENTER to continue")

if __name__ == "__main__":
    main()










