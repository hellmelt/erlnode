const tap = require('tap');
const gen_server = require('../src/gen_server');
const ErlNode = require('../index');
const erlang_node = require('./helpers/start_erlang').erlang_node;
const hostname = require('os').hostname;
const { set_tuple, set_atom } = require('../../erlang.js/api');

const erlnode = new ErlNode('Oreo', 'js1', 0); // , (conn, node) => console.log('Connected: ', conn, node));
// erlnode.receiveCallback((from, to, data) => console.log('Received: ', from, to, data));
const nodename = 'js1@' + hostname().toLowerCase().split('.')[0];

class Accu extends gen_server {

  constructor(seed) {
    super();
    this.accumulator = seed;
  }
  handle_call_add (...data) {
    if (Array.isArray(data) && data.length === 1 && typeof data[0] === 'number') {
      this.accumulator += data[0];
      return this.accumulator;
    }
    return set_tuple([set_atom('error'), 'Bad format to call add']);
  }

  handle_call_subtract (...data) {
    if (Array.isArray(data) && data.length === 1 && typeof data[0] === 'number') {
      this.accumulator -= data[0];
      return this.accumulator;
    }
    return set_tuple([set_atom('error'), 'Bad format to call subtract']);
  }

  handle_cast_add (...data) {
    if (Array.isArray(data) && data.length === 1 && typeof data[0] === 'number') {
      this.accumulator += data[0];
    } else {
      console.log('Bad format to cast add');
    }
  }

  handle_cast_subtract (...data) {
    if (Array.isArray(data) && data.length === 1 && typeof data[0] === 'number') {
      this.accumulator -= data[0];
    } else {
    console.log('Bad format to cast subtract');
    }
  }
}

tap.test('Receive call and cast to js gen_server', (ct) => {
   ct.plan(1);
  const acc = new Accu(0);
  erlnode.register('accumulator', acc);
  erlang_node('e1', 'Oreo', 'test_gen_server', 'run_test', nodename,
    (res) => {
      erlnode.unpublish();
      ct.equal(res, null, 'Erlang gen_server exited as expected');
      ct.end();
    });
});
