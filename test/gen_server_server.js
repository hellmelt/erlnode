const tap = require('tap');
const gen_server = require('../src/gen_server');
const ErlNode = require('../src/engine.js');
const erlnode = new ErlNode('Oreo', 'js');

class Accu extends gen_server {

  constructor(seed) {
    super();
    this.accumulator = seed;
  }
  handle_call_add (...data) {
    console.log('add, data: ', data, this.accumulator);
    if (Array.isArray(data) && data.length === 1 && typeof data[0] === 'number') {
      this.accumulator += data[0];
      console.log(this.accumulator);
      return this.accumulator;
    }
    return set_tuple([set_atom('error'), "Bad format to call add"]);
  };
  handle_cast_someFunc (...data) {
    console.log(data);
  }
}

tap.test('Receive call and cast to js gen_server', (ct) => {
  // ct.plan(5);
  const acc = new Accu(0);
  erlnode.register('accumulator', acc);
});
