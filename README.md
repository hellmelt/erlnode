# erlnode
Erlang cnode functionality based on encapsulating erl_interface C library using napi and cnode-addon-api.

This project is very beta. You can create cnodes, connect and accept, send and receive terms.

erlang.js is used for conversions from a javascript format, representing erlang data types, to erlang binary format, and vice versa.
But erlang.js has not implemented all required conversions, so a fork of that repo is part of this project.

## Prerequisites
Probably works on newer versions. Does often not work on older versions. I just document my setup.

* macOS High Sierra (10.13.6)
* cnode v8.12.0
* yarn 1.7.0 (or npm)
* maybe global install of cnode-gyp?
* XCode command line tools, like C++ compiler and linker?
* erlang 20.2.2
* clone https://github.com/hellmelt/erlang.js.git to a directory next door

## How to build
* Set environment ERL_TOP to the root of the erlang installation. You can find it by starting an erlang shell and type
```
code:root_dir().
```
* yarn install
## How to use
See the code in directory test.

## Automatic tests
### Prerequisites
Compile the erlang module
```
cd test
erlc teste.erl
```
### Run the tests
```
yarn test
```
or
```
npm run test
```
