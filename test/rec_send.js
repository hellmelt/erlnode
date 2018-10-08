/**
 * rec_send
 *
 * These tests start with receiveing a message to a registered name (You cannot send to a pid
 * in a cnode, the process concept does not exist).
 * That means that the internal stuff will accept a connection on the server socket.
 */

const tap = require('tap');
const hostname = require('os').hostname;
const erl_node = require('./helpers/start_erlang').erl_node;
const cNode = require('../engine.js').cNode;

// Test sequence for each of the tests in this module.
//
// Receive "your" atom, send a reply. Receive term, send reply.
// Note: The second receive might fail, if a message from "wrong" node intercepts.
const rec_send = async (cnode, enode, childTest, suffix) => {
  let from, to, term;
  do {
    ({ from, to, term } = await cnode.receive());
  } while (to !== suffix.toString());
  childTest.same(term, {a: 'atomFromErl' + suffix}, 'Received atom');
  cnode.send(from, {a: 'atomFromJS' + suffix});
  cnode.receiveOnce((from, to, term) => {
    childTest.same(term, {t: [{a: 'atomFromErl' + suffix}, 'StringFromErl' + suffix, 42 + parseInt(suffix)]}, 'Received tuple/3');
    cnode.send(from, {t: [{a: 'atomFromJS' + suffix}, 'StringFromJS' + suffix, 142 + parseInt(suffix)]});
  });
};

tap.test('Receive Send to one node one cnode', (cT) => {
  cT.plan(3);
  const cnodename = 'test@' + hostname().toLowerCase().split('.')[0];
  const suffix = '0';
  const cnode = new cNode('Oreo', 'test');
  rec_send(cnode, 'teste', cT, suffix);
  erl_node('teste', 'Oreo', 'teste', 'send_rec_delay', cnodename + ' ' + suffix,
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
      cT.end();
    });
});

tap.test('Receive Send to two nodes one cnode', (cT) => {
  cT.plan(6);
  const cnode = new cNode('Oreo', 'testjs');
  const cnodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const suffix_1 = '1';
  rec_send(cnode, 'test1', cT, suffix_1);
  erl_node('test1', 'Oreo', 'teste', 'send_rec_delay', cnodename + ' ' + suffix_1,
    (res) => {
      cT.equal(res, null, 'Erlang node received correct terms');
    });

  const suffix_2 = '2';
  rec_send(cnode, 'test2', cT, suffix_2);
  erl_node('test2', 'Oreo', 'teste', 'send_rec_delay', cnodename + ' ' + suffix_2,
    (res) => {
      cnode.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
    });
});

tap.test('Receive Send to two nodes two cnodes', (cT) => {
  cT.plan(6);
  const cnode_1 = new cNode('Oreo', 'testjs1');
  const cnodename_1 = 'testjs1@' + hostname().toLowerCase().split('.')[0];
  const suffix_1 = '1';
  rec_send(cnode_1, 'test1', cT, suffix_1);
  erl_node('test1', 'Oreo', 'teste', 'send_rec_delay', cnodename_1 + ' ' + suffix_1,
    (res) => {
      cnode_1.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
    });

  const cnode_2 = new cNode('Oreo', 'testjs2');
  const cnodename_2 = 'testjs2@' + hostname().toLowerCase().split('.')[0];
  const suffix_2 = '2';
  rec_send(cnode_2, 'test2', cT, suffix_2);
  erl_node('test2', 'Oreo', 'teste', 'send_rec_delay', cnodename_2 + ' ' + suffix_2,
    (res) => {
      cnode_2.unpublish();
      cT.equal(res, null, 'Erlang node received correct terms');
    });
});
