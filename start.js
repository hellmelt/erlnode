const erlInterface= require('./build/release/erlnode.node');

console.log(erlInterface.hej());

//console.log(erlInterface.erlConnectInit(1, 'Oreo', 0));

//console.log(erlInterface.erlConnect('anders@dhcp-184-203'));
try {
let obj = new erlInterface.ErlNode({cookie: 'Oreo', connect: 'anders@Hjelms-MacBook',
receiveCallback: (message) => console.log(message)});
} catch(error) {
	console.log('Error: ', error);
}
 
//let node2 = new erlInterface.ErlNode('hjelm@dhcp-184-203', 'Oreo');

//console.log(obj.receive());

