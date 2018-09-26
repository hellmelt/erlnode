const tap = require('tap');
const { exec } = require('child_process');
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect RegSend Receive', (cT) => {
	cT.plan(3);
	const cnodeName = 'testjs@' + hostname().split('.')[0];
	const erlang = exec('cd test; erl -noshell -sname teste2 -setcookie Oreo -s teste reg_rec_send', 
		null, (res) => {
			cT.equal(res, null, 'Erlang node received correct atom');
			cT.end();
		});
	setTimeout(() => {
		const cnode = new cNode('Oreo', 'testjs');
		const conn = cnode.connect('teste2@' + hostname().split('.')[0]);
		cT.ok((conn > 0), 'Connection established');
		cnode.regSend(conn, 'testprocess', {a: 'atomFromJS2'});
		cnode.receive(conn).then((result) => {
			cT.same(result.term, {a: 'atomFromErl2'}, 'Received correct atom');
		})
	}, 1000);
})

