# erlnode
Erlang cnode functionality based on encapsulating erl_interface C library using napi and node-addon-api.

This project is just started, what you see here is just a test of the basic techniques. There is nothing useful yet.

erlang.js is used for conversions from a javascript format, representing erlang data types, to erlang binary format.
But erlang.js has not implemented binary_to_term, so a fork of that repo is part of this project. Only translation
of small atom implemented presently.

##Prerequisites
Probably works on newer versions. Does often not work on older versions. I just document my setup.

* macOS High Sierra (10.13.6) 
* node v8.12.0
* yarn 1.9.4 or npm
* erlang 21.0.8
* clone https://github.com/hellmelt/erlang.js.git to a directory next door

##How to build and run
* Set environment ERL_TOP to the root of the erlang installation. You can find it by starting an erlang shell and type
```
code:root_dir().
```
* yarn install
* start an erlang node
```
erl -sname nodeName -setcookie cookieString
```
* node start (edit name of erlang node to connect to)
* Go to erlang node, send a message
```
{any, nodeName@host} ! myAtom.
```
