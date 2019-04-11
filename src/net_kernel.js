const gen_server = require('./gen_server');
const { set_atom } = require('../../erlang.js/api');

class net_kernel extends gen_server {
    constructor() {
        super();
      };

      handle_call_is_auth (...data) {
        return set_atom('yes');
    }
};

module.exports = net_kernel;
