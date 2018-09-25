const tap = require('tap');
const { exec } = require('child_process');
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect Receive Send', (cT) => {
	cT.plan(4);
	const cnodeName = 'testjs@' + hostname().split('.')[0];
	const erlang = exec('cd test; erl -noshell -sname teste -setcookie Oreo -s teste send_rec', 
		null, (res) => {console.log('Erlang process finished: ', res),
			cT.equal(res, null, 'Erlang node received correct terms');
			cT.end();
		});
	console.log("Erlang node started");
	setTimeout(() => {
		const cnode = new cNode('Oreo', 'testjs');
		const conn = cnode.connect('teste@' + hostname().split('.')[0]);
		cT.ok((conn > 0), 'Connection established');
		cnode.receive(conn).then(({ from, to, term }) => {
			console.log("Received: ", from, to, term);
			cT.same(term, {a: 'atomFromErl'}, 'Received atom');
			cnode.send(conn, from, {a: 'atomFromJS'});
			cnode.receiveCb(conn, (from, to, term) => {
				console.log("Received: ", from, to, term);
				cT.same(term, {t: [{a: 'atomFromErl'}, 'StringFromErl', 42]}, 'Received tuple/3');
				cnode.send(conn, from, {t: [{a: 'atomFromJS'}, 'StringFromJS', 43]});
			});
		});
	}, 1000);
})

