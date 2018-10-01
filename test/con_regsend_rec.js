const tap = require('tap');
const erl_for_client = require('./helpers/start_erlang').erl_for_client;
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect RegSend Receive to one node', (cT) => {
	cT.plan(3);
	const cnodeName = 'testjs@' + hostname().split('.')[0];
	erl_for_client('teste2', 'Oreo', 'teste', 'reg_rec_send', undefined,

		async () => {
		const cnode = new cNode('Oreo', 'testjs');
		const conn = cnode.connect('teste2@' + hostname().split('.')[0]);
		cT.ok((conn > 0), 'Connection established');
		cnode.regSend(conn, 'testprocess', {a: 'atomFromJS2'});
		const { term } = await cnode.receive(conn)
		cT.same(term, {a: 'atomFromErl2'}, 'Received correct atom');
		},

		(res) => {
			cT.equal(res, null, 'Erlang node received correct atom');
			cT.end();
		})
});
