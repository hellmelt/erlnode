const gen_server = require('./src/gen_server');
const ErlNode = require('./src/engine.js');

class JWT extends gen_server {
  constructor() {
    super();
  }
  handle_call_someFunc (data) {
    return 'Hello from JavaScript!';
  }
}

const erlNode = new ErlNode('Oreo', 'js');

const jwt = new JWT();
erlNode.register('jwt', jwt);
