const gen_server = require('./src/gen_server');
const ErlNode = require('./src/engine.js');
const Lodash = require('./src/lodash');
const { set_tuple } = require('./src/types');

class JWT extends gen_server {
  constructor() {
    super();
  }
  handle_call_someFunc (...data) {
    return set_tuple(['Hello from JavaScript!', set_tuple(data)]);
  }
  handle_cast_someFunc (...data) {
    console.log(data);
  }
}

const erlNode = new ErlNode('Oreo', 'js');

const jwt = new JWT();
erlNode.register('jwt', jwt);

erlNode.register('lodash', new Lodash());
