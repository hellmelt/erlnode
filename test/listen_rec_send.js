const tap = require('tap');
const { exec } = require('child_process');
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

const cnode = new cNode('Oreo', 'testjs');
cnode.serverCb(1234, (ipadr, nodename) => console.log(ipadr, nodename));
