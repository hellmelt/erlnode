const tap = require('tap');
const { exec } = require('child_process');
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect Receive Send', (cT) => {
	const cnodeName = 'testjs@' + hostname().split('.')[0];
	const erlang = exec('cd test; erl -noshell -sname teste -setcookie Oreo -s teste send_rec', 
		null, (res) => {console.log('Erlang process finished: ', res),
			cT.equal(res, null, 'Erlang node received correct atom');
			cT.end();
		});
	console.log("Erlang node started");
	setTimeout(() => {
		const cnode = new cNode('Oreo', 'testjs');
		const conn = cnode.connect('teste@' + hostname().split('.')[0]);
		cT.ok((conn > 0), 'Connection failed');
		cnode.receive(conn).then((result) => {
			console.log("Received: ", result);
			cT.same(result.term, {a: 'atomFromErl'}, 'Received wrong data');
		cnode.send(conn, result.from, {a: 'atomFromJS'});
		})
	}, 1000);
})

