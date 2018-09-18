const erlInterface= require('./build/release/erlnode.node');
const erlang = require('../erlang.js');

const hostname = require('os').hostname;

try {
  const obj = new erlInterface.ErlNode(
    {
      cookie: 'Oreo',
      thisNodeName: 'js',
      connect: 'anders@' + hostname().split('.')[0],
    	receiveCallback: (message) => {console.log(erlang.binary_to_term(message))}
    });

  obj.connect('erik@' + hostname().split('.')[0], (buffer) => {console.log("Second connect: ", erlang.binary_to_term(buffer))});

} catch(error) {
  console.log('Error: ', error);
}

//let node2 = new erlInterface.ErlNode('hjelm@dhcp-184-203', 'Oreo');
