## erlsna ##

### About ###
erlsna is a proof-of-concept application for my Computer Scince undergraduate
thesis: Social Network Analysis software for longitudinal node experience
analysis on dynamic networks. [paper](https://dl.dropbox.com/u/17449254/erlsna_thesis.pdf)

erlsna follows Agent-based model where individuals (vertices on graph) are mapped
to agent processes.

Though not highlighed in detail in the paper, erlsna provides a **PubSub** mechanism
for real-time notifications on individuals. See below.

[Eren Güven](https://twitter.com/cyberfart)

### Status ###
erlsna has limited functionality and lacks documentation. Feel free to reach out to me
if you need assistance. I intend to improve on this subject whenever I can.

### Requirements ###
+ Erlang R13B03 or newer (not tested with older releases)
+ [Rebar](https://github.com/basho/rebar) (not mandatory but advised)

+ TCP port 8082 (configurable in erlsna.app.src)

#### Optional Requirements ####
Python3 (for python driver and nntp_collector)
MongoDB (for nntp_collector storage)
R (for some charts) & [zoo](http://cran.r-project.org/web/packages/zoo/index.html) package (I think)

### Running erlsna ###

    ./autorebar # rebar clean, compile and generate
    ./rel/mynode/bin/mynode console

For info on **TCP message syntax**, see the paper, APPENDIX B.
For information on *analyses and example graphs*, see the paper, Chapter 5.

### erlsna Python3 driver ###

    import os
    import erlsna
    api = erlsna.ERLSNA(live=True)
    png_path = os.path.expanduser("~/engagement_graph.png")
    api.engagement_analysis("foo@bar.com", with_graph_png=png_path)

For available keywords and further driver uses, refer to the **erlsna.py** source code.

### PubSub and real time capabilities ###
This was developed for a demo. Currently PubSub is only available for **Closeness Centrality**
metric. One can subscribe to an agent (through Python driver or raw TCP message over Telnet)
with a condition and get notified whenever it holds true. An example:

#### Telnet ####
    # Telnet
    subscribe abc@example.com > 0.123

Will notify the Telnet client whenever the agent's ("abc@example.com") closeness centrality
**goes over** 0.123. The publish will only happen once per **going over threshold**.

#### Python ####
    api = ERLSNA()
    api.subscribe("abc@example.com", "<", 0.02)

+ > : goes over given value
+ < : goes below given value
+ = : becomes equal to given value
+ * : every time a change happens (a dummy value is expected after eg: `subscribe abc@example.com * 0`)

### nntp_collector ###
Used to collect data from NNTP servers for testing and demonstration, requires
[MongoDB](http://www.mongodb.org/) to be running.

**collect:** collect (from where it left off) new articles from given server & group
**emailfixer:** to fix dual-line "From:" header issue in NNTP responses, will prompt user for fixes
**livedump:** push data to running erlsna node

