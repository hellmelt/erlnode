const erlInterface= require('./build/release/erlnode.node');
const erlang = require('../erlang.js');

try {
  const obj = new erlInterface.ErlNode({cookie: 'Oreo', connect: 'anders@Hjelms-MacBook', //});//,
  //const obj = new erlInterface.ErlNode({cookie: 'Oreo', connect: 'anders@dhcp-184-203'});
  // const obj = new erlInterface.ErlNode({cookie: 'Oreo', connect: 'anders@dhcp-184-203' ,
  receiveCallback: (message) => {console.log(erlang.binary_to_term(message))}});
	// obj.receiveAsync((buf) => {console.log("Async: ", buf); console.log(erlang.binary_to_term(buf))});
	//const buf = obj.receive();
	//console.log(buf);
  //console.log(erlang.binary_to_term(buf));
} catch(error) {
	console.log('Error: ', error);
}
 
//let node2 = new erlInterface.ErlNode('hjelm@dhcp-184-203', 'Oreo');
