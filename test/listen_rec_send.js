const tap = require('tap');
const { exec } = require('child_process');
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect Receive Send', (cT) => {
	cT.plan(4);

	const cnode = new cNode('Oreo', 'testjs');
	cnode.serverCb(0, (conn, nodename) => {
		console.log('Connected: ', conn, nodename);
			cT.ok((conn > 0), 'Connection established');
			cnode.receive(conn).then(({ status, from, to, term }) => {
				cT.same(term, {a: 'atomFromErl'}, 'Received atom');
				cnode.send(conn, from, {a: 'atomFromJS'});
				cnode.receiveCb(conn, (status, from, to, term) => {
					cT.same(term, {t: [{a: 'atomFromErl'}, 'StringFromErl', 42]}, 'Received tuple/3');
					cnode.send(conn, from, {t: [{a: 'atomFromJS'}, 'StringFromJS', 43]});
				});
		})
	});

	setTimeout(() => {
		let myNodeName = 'testjs@' + hostname().split('.')[0];
		myNodeName = myNodeName.toLowerCase();
		const erlang = exec('cd test; erl -noshell -sname teste -setcookie Oreo -s teste doSendRec ' + myNodeName, 
		null, (res) => {
			cT.equal(res, null, 'Erlang node received correct terms');
			cT.end();
		});
	}, 1000);

});
