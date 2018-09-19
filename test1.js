const cNode = require('./api.js').cNode;
const hostname = require('os').hostname;

const myNode = new cNode('Oreo', 'test');

const connAnders = myNode.connect('anders@' + hostname().split('.')[0]);

myNode.receive(connAnders).then((result) => {
	console.log("Received: ", result);
	setTimeout(() => {
		myNode.send(connAnders, result.from, {a: 'atomFromJS'})
	}, 20000);
});

const receiver = async (connection) => {
	const result = await myNode.receive(connection);
	console.log("Received: ", result);
}
// receiver(connAnders).then((res) => {console.log("We did it")});
