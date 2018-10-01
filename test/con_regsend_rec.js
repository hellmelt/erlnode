const tap = require('tap');
const erl_for_client = require('./helpers/start_erlang').erl_for_client;
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

const con_regsend_rec = async (cnode, enode, childTest, suffix) => {
  const conn = cnode.connect(enode + '@' + hostname().split('.')[0]);
  childTest.ok((conn > 0), 'Connection established');
  cnode.regSend(conn, 'testprocess' + suffix, {a: 'atomFromJS' + suffix});
  const { term } = await cnode.receive(conn);
  childTest.same(term, {a: 'atomFromErl2' + suffix}, 'Received correct atom');

}

tap.test('Connect RegSend Receive to one node', (cT) => {
	cT.plan(3);
	const cnodename = 'testjs@' + hostname().split('.')[0];
  const cnode = new cNode('Oreo', 'testjs');
  const suffix = '4';
	erl_for_client('teste2', 'Oreo', 'teste', 'reg_rec_send', cnodename + ' ' + suffix,
		() => con_regsend_rec(cnode, 'teste2', cT, suffix),
		(res) => {
			cT.equal(res, null, 'Erlang node received correct atom');
			cT.end();
		})
});
