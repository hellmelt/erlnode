const erlInterface= require('./build/release/erlnode.node');
const erlang = require('../erlang.js');

console.log(erlang.term_to_binary({atom: 'jsatom'}));

console.log(erlInterface.hej());

//console.log(erlInterface.erlConnectInit(1, 'Oreo', 0));

//console.log(erlInterface.erlConnect('anders@dhcp-184-203'));
try {
let obj = new erlInterface.ErlNode({cookie: 'Oreo', connect: 'anders@Hjelms-MacBook'});//, 
	//receiveCallback: (message) => console.log(message)});
	const buf = obj.receive();
	console.log(buf);
  console.log(erlang.binary_to_term(buf));
} catch(error) {
	console.log('Error: ', error);
}
 
//let node2 = new erlInterface.ErlNode('hjelm@dhcp-184-203', 'Oreo');
