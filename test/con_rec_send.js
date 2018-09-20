const tap = require('tap');
const { exec } = require('child_process');
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

tap.test('Connect Receive Send', (cT) => {
	const cnodeName = 'testjs@' + hostname().split('.')[0];
	const erlang = exec('erl -noshell -sname teste -setcookie Oreo -s teste send_rec ' + cnodeName, 
		null, (res) => {console.log(res)});
	setTimout(() => {
		const cnode = new cNode('Oreo', 'testjs');
		const conn = cnode.connect('teste@' + hostname().split('.')[0]);
		code.receive(conn).then((result) => {
			console.log("Received: ", result);
			cT.equal(result.term, {a: 'atomFromErl'});
		cnode.send(conn, result.from, {a: 'atomFromJS'});
		})
	}, 500);
	cT.end();
})

