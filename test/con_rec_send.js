const tap = require('tap');
const { exec } = require('child_process');
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect Receive Send', (cT) => {
	cT.plan(4);
	const erlang = exec('cd test; erl -noshell -sname teste -setcookie Oreo -s teste send_rec', 
		null, (res) => {
			cT.equal(res, null, 'Erlang node received correct terms');
			cT.end();
		});
	setTimeout(() => {
		const cnode = new cNode('Oreo', 'testjs');
		const conn = cnode.connect('teste@' + hostname().split('.')[0]);
		cT.ok((conn > 0), 'Connection established');
		cnode.receive(conn).then(({ status, from, to, term }) => {
			cT.same(term, {a: 'atomFromErl'}, 'Received atom');
			cnode.send(conn, from, {a: 'atomFromJS'});
			cnode.receiveCb(conn, (status, from, to, term) => {
				cT.same(term, {t: [{a: 'atomFromErl'}, 'StringFromErl', 42]}, 'Received tuple/3');
				cnode.send(conn, from, {t: [{a: 'atomFromJS'}, 'StringFromJS', 43]});
			});
		});
	}, 1000);
})

