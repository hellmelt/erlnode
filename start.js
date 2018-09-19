const erlInterface= require('./build/release/erlnode.node');
const erlang = require('../erlang.js');

const hostname = require('os').hostname;

try {
  const obj = new erlInterface.ErlNode(
    {
      cookie: 'Oreo',
      thisNodeName: 'js',
    	receiveCallback: (message) => {console.log(erlang.binary_to_term(message))}
    });

  const node2 = new erlInterface.ErlNode(    {
    cookie: 'Oreo',
    thisNodeName: 'js2',
    receiveCallback: (message) => {console.log(erlang.binary_to_term(message))}
  });

  const cA = obj.connect('anders@' + hostname().split('.')[0]);
	obj.receive(cA, (buffer) => {console.log("anders connection: ", erlang.binary_to_term(buffer))});

  const cE = obj.connect('erik@' + hostname().split('.')[0]);
  obj.receive(cE, (buffer) => {console.log("erik connection: ", erlang.binary_to_term(buffer))});

  const cH = node2.connect('erik@' + hostname().split('.')[0]);
  node2.receive(cH, (buffer) => {console.log("erik connection from node 2:", erlang.binary_to_term(buffer))});
} catch(error) {
  console.error('Something went wrong with your cnodes.');
  console.error(error);
}

