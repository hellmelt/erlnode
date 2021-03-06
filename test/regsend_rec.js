/**
 * regsend_rec
 *
 * These tests start with sending a message to a process with a registered name in
 * a remote erlNode. That means that the internal stuff will create a client (connect)
 * to the remote erlNode.
 */

const tap = require('tap');
const erlang_node = require('./helpers/start_erlang').erlang_node;
const hostname = require('os').hostname;
const ErlNode = require('../index');

// Test sequence for each of the tests in this moudule.
//
// Send an atom to registered process in remote erlang node, receive "your" atom.
const regsend_rec = async (erlNode, erlangNodeName, childTest, suffix) => {
  erlNode.regSend('testprocess' + suffix, erlangNodeName + '@' + hostname().toLowerCase().split('.')[0], {a: 'atomFromJS' + suffix});
  let to, term;
  do {
    ({ to, term } = await erlNode.receive());
  } while (to !== suffix);
  childTest.same(term, {a: 'atomFromErl2' + suffix}, 'Received correct atom');
};

tap.test('RegSend Receive to one erlNode', (cT) => {
  cT.plan(2);
  const nodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const erlNode = new ErlNode('Oreo', 'testjs');
  const suffix = '4';
  erlang_node('teste2', 'Oreo', 'teste', 'reg_rec_send', nodename + ' ' + suffix,
    (res) => {
      erlNode.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct atom');
    });
  setTimeout(() => regsend_rec(erlNode, 'teste2', cT, suffix),
  200);  
});

tap.test('Connect RegSend Receive two nodes one erlNode', (cT) => {
  cT.plan(4);
  const nodename = 'testjs0@' + hostname().toLowerCase().split('.')[0];
  const erlNode = new ErlNode('Oreo', 'testjs0');
  const suffix_1 = '5';
  erlang_node('teste5', 'Oreo', 'teste', 'reg_rec_send', nodename + ' ' + suffix_1,
    (res) => {
      cT.equal(res, null, 'Erlang erlNode received correct atom');
    });
    setTimeout(() => regsend_rec(erlNode, 'teste5', cT, suffix_1),
      200);

  const suffix_2 = '6';
  erlang_node('teste6', 'Oreo', 'teste', 'reg_rec_send', nodename + ' ' + suffix_2,
    (res) => {
      erlNode.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct atom');
    });
    setTimeout(() => regsend_rec(erlNode, 'teste6', cT, suffix_2),
      200);
});

tap.test('Connect RegSend Receive two nodes two erlNodes', (cT) => {
  cT.plan(4);
  const nodename_1 = 'testjs1@' + hostname().toLowerCase().split('.')[0];
  const erlNode_1 = new ErlNode('Oreo', 'testjs1');
  const suffix_1 = '5';
  erlang_node('teste5', 'Oreo', 'teste', 'reg_rec_send', nodename_1 + ' ' + suffix_1,
    (res) => {
      erlNode_1.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct atom');
    });
  setTimeout(() => regsend_rec(erlNode_1, 'teste5', cT, suffix_1),
    200);

  const nodename_2 = 'testjs2@' + hostname().toLowerCase().split('.')[0];
  const erlNode_2 = new ErlNode('Oreo', 'testjs2');
  const suffix_2 = '6';
  erlang_node('teste6', 'Oreo', 'teste', 'reg_rec_send', nodename_2 + ' ' + suffix_2,
    (res) => {
      erlNode_2.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct atom');
    });
    setTimeout(() => regsend_rec(erlNode_2, 'teste6', cT, suffix_2),
      200);
});
