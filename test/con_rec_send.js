const tap = require('tap');
const hostname = require('os').hostname;
const erl_for_client = require('./helpers/start_erlang').erl_for_client;
const cNode = require('../api.js').cNode;

tap.test('Connect Receive Send to one node one cnode', (cT) => {
	cT.plan(4);
	let cnodename = 'testjs@' + hostname().split('.')[0];
	cnodename = cnodename.toLowerCase();
	erl_for_client('teste', 'Oreo', 'teste', 'send_rec_delay', cnodename, 

		async () => {
			const cnode = new cNode('Oreo', 'testjs');
			const conn = cnode.connect('teste@' + hostname().split('.')[0]);
			cT.ok((conn > 0), 'Connection established');
			let { status, from, to, term } = await cnode.receive(conn);
			cT.same(term, {a: 'atomFromErl'}, 'Received atom');
			cnode.send(conn, from, {a: 'atomFromJS'});
			cnode.receiveCb(conn, (status, from, to, term) => {
				cT.same(term, {t: [{a: 'atomFromErl'}, 'StringFromErl', 42]}, 'Received tuple/3');
				cnode.send(conn, from, {t: [{a: 'atomFromJS'}, 'StringFromJS', 43]});
			});			
		},

		(res) => {
			cT.equal(res, null, 'Erlang node received correct terms');
			cT.end();	
		});
});

tap.test('Connect Receive Send to two nodes one cnode', (cT) => {
	cT.plan(8);
	const cnode = new cNode('Oreo', 'testjs');
	let cnodename = 'testjs@' + hostname().split('.')[0];
	cnodename = cnodename.toLowerCase();
	erl_for_client('test1', 'Oreo', 'teste', 'send_rec_delay', cnodename, 

		async () => {
			const conn = cnode.connect('test1@' + hostname().split('.')[0]);
			cT.ok((conn > 0), 'Connection established');
			let { status, from, to, term } = await cnode.receive(conn);
			cT.same(term, {a: 'atomFromErl'}, 'Received atom');
			cnode.send(conn, from, {a: 'atomFromJS'});
			cnode.receiveCb(conn, (status, from, to, term) => {
				cT.same(term, {t: [{a: 'atomFromErl'}, 'StringFromErl', 42]}, 'Received tuple/3');
				cnode.send(conn, from, {t: [{a: 'atomFromJS'}, 'StringFromJS', 43]});
			});			
		},

		(res) => {
			cT.equal(res, null, 'Erlang node received correct terms');
		});

	erl_for_client('test2', 'Oreo', 'teste', 'send_rec_delay', cnodename, 

		async () => {
			const conn = cnode.connect('test2@' + hostname().split('.')[0]);
			cT.ok((conn > 0), 'Connection established');
			let { status, from, to, term } = await cnode.receive(conn);
			cT.same(term, {a: 'atomFromErl'}, 'Received atom');
			cnode.send(conn, from, {a: 'atomFromJS'});
			cnode.receiveCb(conn, (status, from, to, term) => {
				cT.same(term, {t: [{a: 'atomFromErl'}, 'StringFromErl', 42]}, 'Received tuple/3');
				cnode.send(conn, from, {t: [{a: 'atomFromJS'}, 'StringFromJS', 43]});
			});			
		},

		(res) => {
			cT.equal(res, null, 'Erlang node received correct terms');
		});

});
