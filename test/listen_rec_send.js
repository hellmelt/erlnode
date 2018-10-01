const tap = require('tap');
const erl_for_server = require('./helpers/start_erlang').erl_for_server;
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect Receive Send', (cT) => {
	cT.plan(4);

	const cnode = new cNode('Oreo', 'testjs');
	cnode.serverCb(0, async (conn, nodename) => {
		console.log('Connected: ', conn, nodename);
		cT.ok((conn > 0), 'Connection established');
		let { status, from, to, term } = await cnode.receive(conn)
		cT.same(term, {a: 'atomFromErl'}, 'Received atom');
		cnode.send(conn, from, {a: 'atomFromJS'});
		cnode.receiveCb(conn, (status, from, to, term) => {
			cT.same(term, {t: [{a: 'atomFromErl'}, 'StringFromErl', 42]}, 'Received tuple/3');
			cnode.send(conn, from, {t: [{a: 'atomFromJS'}, 'StringFromJS', 43]});
		});
	});

	let myNodeName = 'testjs@' + hostname().split('.')[0];
	myNodeName = myNodeName.toLowerCase();

	erl_for_server('teste', 'Oreo', 'teste', 'doSendRec', myNodeName,
	(result) => {
		cT.equal(result, null, 'Erlang node received correct terms');
		cT.end();
	});
});
