const tap = require('tap');
const hostname = require('os').hostname;
const erl_for_client = require('./helpers/start_erlang').erl_for_client;
const cNode = require('../api.js').cNode;

const con_rec_send = async (cnode, enode, childTest, suffix) => {
  const conn = cnode.connect(enode + '@' + hostname().toLowerCase().split('.')[0]);
  childTest.ok((conn > 0), 'Connection established');
  let { status, from, to, term } = await cnode.receive(conn);
  childTest.same(term, {a: 'atomFromErl' + suffix}, 'Received atom');
  cnode.send(conn, from, {a: 'atomFromJS' + suffix});
  cnode.receiveCb(conn, (status, from, to, term) => {
    childTest.same(term, {t: [{a: 'atomFromErl' + suffix}, 'StringFromErl' + suffix, 42 + parseInt(suffix)]}, 'Received tuple/3');
    cnode.send(conn, from, {t: [{a: 'atomFromJS' + suffix}, 'StringFromJS' + suffix, 142 + parseInt(suffix)]});
  });
};

tap.test('Connect Receive Send to one node one cnode', (cT) => {
  cT.plan(4);
  const cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const suffix = '0';
  const cnode = new cNode('Oreo', 'testjs');
  erl_for_client('teste', 'Oreo', 'teste', 'send_rec_delay', cnodename + ' ' + suffix,
    () => con_rec_send(cnode, 'teste', cT, 0),
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
      cT.end();
    });
});

tap.test('Connect Receive Send to two nodes one cnode', (cT) => {
  cT.plan(8);
  const cnode = new cNode('Oreo', 'testjs');
  const cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const suffix_1 = '1';
  erl_for_client('test1', 'Oreo', 'teste', 'send_rec_delay', cnodename + ' ' + suffix_1,
		() => con_rec_send(cnode, 'test1', cT, suffix_1),
    (res) => {
      cT.equal(res, null, 'Erlang node received correct terms');
    });

  const suffix_2 = 2;
  erl_for_client('test2', 'Oreo', 'teste', 'send_rec_delay', cnodename + ' ' + suffix_2,
		() => con_rec_send(cnode, 'test2', cT, suffix_2),
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
    });

});

tap.test('Connect Receive Send to two nodes two cnodes', (cT) => {
  cT.plan(8);
  const cnode_1 = new cNode('Oreo', 'testjs_1');
  const cnodename_1 = 'testjs_1@' + hostname().toLowerCase().split('.')[0];
  const suffix_1 = '1';
  erl_for_client('test1', 'Oreo', 'teste', 'send_rec_delay', cnodename_1 + ' ' + suffix_1,
    () => con_rec_send(cnode_1, 'test1', cT, suffix_1),
    (res) => {
      cnode_1.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
    });

  const cnode_2 = new cNode('Oreo', 'testjs_2');
  const cnodename_2 = 'testjs_2@' + hostname().toLowerCase().split('.')[0];
  const suffix_2 = 2;
  erl_for_client('test2', 'Oreo', 'teste', 'send_rec_delay', cnodename_2 + ' ' + suffix_2,
    () => con_rec_send(cnode_2, 'test2', cT, suffix_2),
    (res) => {
      cnode_2.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
    });

});
