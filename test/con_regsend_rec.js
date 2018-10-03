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
  const cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const cnode = new cNode('Oreo', 'testjs');
  const suffix = '4';
  erl_for_client('teste2', 'Oreo', 'teste', 'reg_rec_send', cnodename + ' ' + suffix,
    () => con_regsend_rec(cnode, 'teste2', cT, suffix),
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    })
});

tap.test('Connect RegSend Receive two nodes one cnode', (cT) => {
  cT.plan(6);
  const cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const cnode = new cNode('Oreo', 'testjs');
  const suffix_1 = '5';
  erl_for_client('teste5', 'Oreo', 'teste', 'reg_rec_send', cnodename + ' ' + suffix_1,
    () => con_regsend_rec(cnode, 'teste5', cT, suffix_1),
    (res) => {
      cT.equal(res, null, 'Erlang node received correct atom');
    });

  const suffix_2 = '6';
  erl_for_client('teste6', 'Oreo', 'teste', 'reg_rec_send', cnodename + ' ' + suffix_2,
    () => con_regsend_rec(cnode, 'teste6', cT, suffix_2),
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    });
});

tap.test('Connect RegSend Receive two nodes two cnodes', (cT) => {
  cT.plan(6);
  const cnodename_1 = 'testjs1@' + hostname().toLowerCase().split('.')[0];
  const cnode_1 = new cNode('Oreo', 'testjs1');
  const suffix_1 = '5';
  erl_for_client('teste5', 'Oreo', 'teste', 'reg_rec_send', cnodename_1 + ' ' + suffix_1,
    () => con_regsend_rec(cnode_1, 'teste5', cT, suffix_1),
    (res) => {
      cnode_1.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    });

  const cnodename_2 = 'testjs2@' + hostname().toLowerCase().split('.')[0];
  const cnode_2 = new cNode('Oreo', 'testjs2');
  const suffix_2 = '6';
  erl_for_client('teste6', 'Oreo', 'teste', 'reg_rec_send', cnodename_2 + ' ' + suffix_2,
    () => con_regsend_rec(cnode_2, 'teste6', cT, suffix_2),
    (res) => {
      cnode_2.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    });
});
