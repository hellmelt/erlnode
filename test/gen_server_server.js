const tap = require('tap');
const gen_server = require('../src/gen_server');
const ErlNode = require('../src/engine.js');
const erlang_node = require('./helpers/start_erlang').erlang_node;
const hostname = require('os').hostname;

const erlnode = new ErlNode('Oreo', 'js1', 0); // , (conn, node) => console.log('Connected: ', conn, node));
// erlnode.receiveCallback((from, to, data) => console.log('Received: ', from, to, data));
const nodename = 'js1@' + hostname().toLowerCase().split('.')[0];

class Accu extends gen_server {

  constructor(seed) {
    super();
    this.accumulator = seed;
  }
  handle_call_add (data) {
    if (typeof data === 'number') {
      this.accumulator += data;
      return this.accumulator;
    }
    return set_tuple([set_atom('error'), 'Bad format to call add']);
  }

  handle_call_subtract (data) {
    if (typeof data === 'number') {
      return new Promise((res) => {
        setTimeout(() => {
          this.accumulator -= data;
          res(this.accumulator);
        }, 100);
      })
    }
    return set_tuple([set_atom('error'), 'Bad format to call subtract']);
  }

  handle_cast_add (data) {
    if (typeof data === 'number') {
      this.accumulator += data;
    }
  }

  handle_cast_subtract (data) {
    if (typeof data === 'number') {
      this.accumulator -= data;
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
      ct.equal(res, null, 'Erlang process exited as expected');
      ct.end();
    });
});
