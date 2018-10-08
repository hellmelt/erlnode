/**
 * regsend_rec
 *
 * These tests start with sending a message to a process with a registered name in
 * a remote node. That means that the internal stuff will create a client (connect)
 * to the remote node.
 */

const tap = require('tap');
const erl_for_client = require('./helpers/start_erlang').erl_for_client;
const erl_node = require('./helpers/start_erlang').erl_node;
const hostname = require('os').hostname;
const cNode = require('../engine.js').cNode;

// Test sequence for each of the tests in this moudule.
//
// Send an atom to registered process in remote node, receive "your" atom.
const regsend_rec = async (cnode, enode, childTest, suffix) => {
  cnode.regSend('testprocess' + suffix, enode + '@' + hostname().toLowerCase().split('.')[0], {a: 'atomFromJS' + suffix});
  let to, term;
  do {
    ({ to, term } = await cnode.receive());
  } while (to !== suffix.toString());
  childTest.same(term, {a: 'atomFromErl2' + suffix}, 'Received correct atom');
};

tap.test('RegSend Receive to one node', (cT) => {
  cT.plan(2);
  const cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const cnode = new cNode('Oreo', 'testjs');
  const suffix = '4';
  erl_for_client('teste2', 'Oreo', 'teste', 'reg_rec_send', cnodename + ' ' + suffix,
    () => regsend_rec(cnode, 'teste2', cT, suffix),
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    });
});

tap.test('Connect RegSend Receive two nodes one cnode', (cT) => {
  cT.plan(4);
  const cnodename = 'testjs0@' + hostname().toLowerCase().split('.')[0];
  const cnode = new cNode('Oreo', 'testjs0');
  const suffix_1 = '5';
  erl_for_client('teste5', 'Oreo', 'teste', 'reg_rec_send', cnodename + ' ' + suffix_1,
    () => regsend_rec(cnode, 'teste5', cT, suffix_1),
    (res) => {
      cT.equal(res, null, 'Erlang node received correct atom');
    });

  const suffix_2 = '6';
  erl_for_client('teste6', 'Oreo', 'teste', 'reg_rec_send', cnodename + ' ' + suffix_2,
    () => regsend_rec(cnode, 'teste6', cT, suffix_2),
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    });
});

tap.test('Connect RegSend Receive two nodes two cnodes', (cT) => {
  cT.plan(4);
  const cnodename_1 = 'testjs1@' + hostname().toLowerCase().split('.')[0];
  const cnode_1 = new cNode('Oreo', 'testjs1');
  const suffix_1 = '5';
  erl_for_client('teste5', 'Oreo', 'teste', 'reg_rec_send', cnodename_1 + ' ' + suffix_1,
    () => regsend_rec(cnode_1, 'teste5', cT, suffix_1),
    (res) => {
      cnode_1.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    });

  const cnodename_2 = 'testjs2@' + hostname().toLowerCase().split('.')[0];
  const cnode_2 = new cNode('Oreo', 'testjs2');
  const suffix_2 = '6';
  erl_for_client('teste6', 'Oreo', 'teste', 'reg_rec_send', cnodename_2 + ' ' + suffix_2,
    () => regsend_rec(cnode_2, 'teste6', cT, suffix_2),
    (res) => {
      cnode_2.unpublish();
      cT.equal(res, null, 'Erlang node received correct atom');
    });
});
