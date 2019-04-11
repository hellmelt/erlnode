const gen_server = require('./gen_server');
const { set_tuple, set_atom, get_atom } = require('../../erlang.js/api');

class Rex extends gen_server {
    constructor() {
        super();
      };

      handle_call_call(...data) {
          if (Array.isArray(data) && data.length === 4) {
            let [module, func, args, pid] = data;
            module = get_atom(module);
            func = get_atom(func);
            console.log(`Rex call module ${module} function ${func} args is array ${Array.isArray(args)}`);
        }
        return set_tuple([set_atom('badrpc'), set_atom('wrongNumberOfArgs')]);
    }
};

module.exports = Rex;
