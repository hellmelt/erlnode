/**
 * rec_send
 *
 * These tests start with receiveing a message to a registered name (You cannot send to a pid
 * in a erlNode, the process concept does not exist).
 * That means that the internal stuff will accept a connection on the server socket.
 */

const tap = require('tap');
const hostname = require('os').hostname;
const erl_node = require('./helpers/start_erlang').erl_node;
const ErlNode = require('../engine.js');

// Test sequence for each of the tests in this module.
//
// Receive "your" atom, send a reply. Receive term, send reply.
// Note: The second receive might fail, if a message from "wrong" node intercepts.
const rec_send = async (erlNode, erlangNodeName, childTest, suffix) => {
  let from, to, term;
  do {
    ({ from, to, term } = await erlNode.receive());
  } while (to !== suffix.toString());
  childTest.same(term, {a: 'atomFromErl' + suffix}, 'Received atom');
  erlNode.send(from, {a: 'atomFromJS' + suffix});
  erlNode.receiveOnce((from, to, term) => {
    childTest.same(term, {t: [{a: 'atomFromErl' + suffix}, 'StringFromErl' + suffix, 42 + parseInt(suffix)]}, 'Received tuple/3');
    erlNode.send(from, {t: [{a: 'atomFromJS' + suffix}, 'StringFromJS' + suffix, 142 + parseInt(suffix)]});
  });
};

tap.test('Receive Send to one erlNode one erlNode', (cT) => {
  cT.plan(3);
  const nodename = 'test@' + hostname().toLowerCase().split('.')[0];
  const suffix = '0';
  const erlNode = new ErlNode('Oreo', 'test');
  rec_send(erlNode, 'teste', cT, suffix);
  erl_node('teste', 'Oreo', 'teste', 'send_rec_delay', nodename + ' ' + suffix,
    (res) => {
      erlNode.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct terms');
      cT.end();
    });
});

tap.test('Receive Send to two nodes one erlNode', (cT) => {
  cT.plan(6);
  const erlNode = new ErlNode('Oreo', 'testjs');
  const nodename = 'testjs@' + hostname().toLowerCase().split('.')[0];
  const suffix_1 = '1';
  rec_send(erlNode, 'test1', cT, suffix_1);
  erl_node('test1', 'Oreo', 'teste', 'send_rec_delay', nodename + ' ' + suffix_1,
    (res) => {
      cT.equal(res, null, 'Erlang erlNode received correct terms');
    });

  const suffix_2 = '2';
  rec_send(erlNode, 'test2', cT, suffix_2);
  erl_node('test2', 'Oreo', 'teste', 'send_rec_delay', nodename + ' ' + suffix_2,
    (res) => {
      erlNode.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct terms');
    });
});

tap.test('Receive Send to two nodes two erlNodes', (cT) => {
  cT.plan(6);
  const erlNode_1 = new ErlNode('Oreo', 'testjs1');
  const nodename_1 = 'testjs1@' + hostname().toLowerCase().split('.')[0];
  const suffix_1 = '1';
  rec_send(erlNode_1, 'test1', cT, suffix_1);
  erl_node('test1', 'Oreo', 'teste', 'send_rec_delay', nodename_1 + ' ' + suffix_1,
    (res) => {
      erlNode_1.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct terms');
    });

  const erlNode_2 = new ErlNode('Oreo', 'testjs2');
  const nodename_2 = 'testjs2@' + hostname().toLowerCase().split('.')[0];
  const suffix_2 = '2';
  rec_send(erlNode_2, 'test2', cT, suffix_2);
  erl_node('test2', 'Oreo', 'teste', 'send_rec_delay', nodename_2 + ' ' + suffix_2,
    (res) => {
      erlNode_2.unpublish();
      cT.equal(res, null, 'Erlang erlNode received correct terms');
    });
});