const tap = require('tap');
const erl_for_server = require('./helpers/start_erlang').erl_for_server;
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

const server_rec_send = async (cnode, conn, childTest, suffix) => {
			let { status, from, to, term } = await cnode.receive(conn)
			childTest.same(term, {a: 'atomFromErl' + suffix}, 'Received atom');
			cnode.send(conn, from, {a: 'atomFromJS' + suffix});
			cnode.receiveCb(conn, (status, from, to, term) => {
			childTest.same(term, {t: [{a: 'atomFromErl' + suffix}, 'StringFromErl' + suffix, 42 + parseInt(suffix)]}, 
				'Received tuple/3');
			cnode.send(conn, from, {t: [{a: 'atomFromJS' + suffix}, 'StringFromJS' + suffix, 142 + parseInt(suffix)]});
		});
}

tap.test('Connect Receive Send one server one cnode callback', (cT) => {
	cT.plan(4);
	const cnode = new cNode('Oreo', 'testjs');
	const suffix = '0';
	cnode.serverCb(0, async (conn, nodename) => {
			console.log('Connected: ', conn, nodename);
			cT.ok((conn > 0), 'Connection established');
			server_rec_send(cnode, conn, cT, suffix);
		});
	let cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
	erl_for_server('teste', 'Oreo', 'teste', 'send_rec', cnodename + ' ' + suffix,
	(result) => {
		cT.equal(result, null, 'Erlang node received correct terms');
	});
});

tap.test('Connect Receive Send one server one cnode promise', (cT) => {
	cT.plan(4);
	const cnode = new cNode('Oreo', 'testjs');
	const suffix = '0';
	cnode.server(0).then(({ connection: conn, nodename }) => {
		console.log('Connected: ', conn, nodename);
		cT.ok((conn > 0), 'Connection established');
		server_rec_send(cnode, conn, cT, suffix);
	});
	let cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
	erl_for_server('teste', 'Oreo', 'teste', 'send_rec', cnodename + ' ' + suffix,
	(result) => {
		cT.equal(result, null, 'Erlang node received correct terms');
	});
});

tap.test('Connect Receive Send two servers one cnode', (cT) => {
	cT.plan(8);
	const cnode = new cNode('Oreo', 'testjs1');
	const suffix_1 = '1';
	const suffix_2 = '2';
	
	cnode.serverCb(0, (conn, nodename) => {
		console.log('Connected: ', conn, nodename);
		cT.ok((conn > 0), 'Connection established');
		server_rec_send(cnode, conn, cT, suffix_1);
	});
	let cnodename_1 = 'testjs1@' + hostname().toLowerCase().split('.')[0];
	erl_for_server('teste1', 'Oreo', 'teste', 'send_rec', cnodename_1 + ' ' + suffix_1,
	(result) => {
		cT.equal(result, null, 'Erlang node received correct terms');
	});

	cnode.server(0).then(({ connection: conn, nodename}) => {
		console.log('Connected: ', conn, nodename);
		cT.ok((conn > 0), 'Connection established');
		server_rec_send(cnode, conn, cT, suffix_2);
	});
	erl_for_server('teste2', 'Oreo', 'teste', 'send_rec', cnodename_1 + ' ' + suffix_2,
	(result) => {
		cT.equal(result, null, 'Erlang node received correct terms');
	});
});
