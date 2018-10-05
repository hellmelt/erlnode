const tap = require('tap');
const erl_for_server = require('./helpers/start_erlang').erl_for_server;
const hostname = require('os').hostname;

const cNode = require('../api.js').cNode;

const server_rec_send = async (cnode, conn, childTest, suffix) => {
  let { status, from, to, term } = await cnode.receive(conn);
  childTest.same(term, {a: 'atomFromErl' + suffix}, 'Received atom');
  cnode.send(conn, from, {a: 'atomFromJS' + suffix});
  cnode.receiveCb(conn, (status, from, to, term) => {
    childTest.same(term, {t: [{a: 'atomFromErl' + suffix}, 'StringFromErl' + suffix, 42 + parseInt(suffix)]},
      'Received tuple/3');
    cnode.send(conn, from, {t: [{a: 'atomFromJS' + suffix}, 'StringFromJS' + suffix, 142 + parseInt(suffix)]});
  });
};

tap.test('Connect Receive Send one server one cnode', (cT) => {
  cT.plan(4);
  const suffix = '0';
  const cnode = new cNode('Oreo', 'testjs', 0, async (conn, nodename) => {
    console.log('Connected 0: ', conn, nodename);
    cT.ok((conn > 0), 'Connection established');
    server_rec_send(cnode, conn, cT, suffix);
  });
  let cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  erl_for_server('teste', 'Oreo', 'teste', 'send_rec', cnodename + ' ' + suffix,
    (result) => {
      cnode.unpublish();
      cT.equal(result, null, 'Erlang node received correct terms');
      cT.end();
    });
});

tap.test('Connect Receive Send one server one cnode two incoming connections', (cT) => {
  cT.plan(8);
  const suffix_1 = '0';
  const suffix_2 = '5';
  const cnode = new cNode('Oreo', 'testjs0', 0, async (conn, nodename) => {
    if (nodename.match('teste1')) {
      console.log('Connected 1: ', conn, nodename);
    	cT.ok((conn > 0), 'Connection established');
    	server_rec_send(cnode, conn, cT, suffix_1);
    } else if (nodename.match('teste2')) {
      console.log('Connected 2: ', conn, nodename);
      cT.ok((conn > 0), 'Connection established');
      server_rec_send(cnode, conn, cT, suffix_2);
		}
  });
  let cnodename = 'testjs0@' + hostname().toLowerCase().split('.')[0];
  erl_for_server('teste1', 'Oreo', 'teste', 'send_rec', cnodename + ' ' + suffix_1,
    (result) => {
      cT.equal(result, null, 'Erlang node received correct terms');
    });

  erl_for_server('teste2', 'Oreo', 'teste', 'send_rec', cnodename + ' ' + suffix_2,
    (result) => {
  		cnode.unpublish();
      cT.equal(result, null, 'Erlang node received correct terms');
    });
});

tap.test('Connect Receive Send two servers (two cnodes) two incoming connections', (cT) => {
  cT.plan(8);
  const suffix_1 = '0';
  const suffix_2 = '5';
  const cnode1 = new cNode('Oreo', 'testjs1', 0, async (conn, nodename) => {
    if (nodename.match('teste1')) {
      console.log('Connected 11: ', conn, nodename);
      cT.ok((conn > 0), 'Connection established');
      server_rec_send(cnode1, conn, cT, suffix_1);
    }
  });
  let cnodename1 = 'testjs1@' + hostname().toLowerCase().split('.')[0];
  erl_for_server('teste1', 'Oreo', 'teste', 'send_rec', cnodename1 + ' ' + suffix_1,
    (result) => {
  		cnode1.unpublish();
      cT.equal(result, null, 'Erlang node received correct terms');
    });

  const cnode2 = new cNode('Oreo', 'testjs2', 0, async (conn, nodename) => {
    if (nodename.match('teste2')) {
      console.log('Connected 22: ', conn, nodename);
      cT.ok((conn > 0), 'Connection established');
      server_rec_send(cnode2, conn, cT, suffix_2);
    }
  });
  let cnodename2 = 'testjs2@' + hostname().toLowerCase().split('.')[0];
  erl_for_server('teste2', 'Oreo', 'teste', 'send_rec', cnodename2 + ' ' + suffix_2,
    (result) => {
  		cnode2.unpublish();
      cT.equal(result, null, 'Erlang node received correct terms');
    });
});
