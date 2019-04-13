const gen_server = require('./gen_server');
const { set_tuple, set_atom, get_atom } = require('../../erlang.js/api');
const path = require('path');

class Rex extends gen_server {
    constructor() {
        super();
      };

      async handle_call_call(...data) {
          if (Array.isArray(data) && data.length === 4) {
            let [module, func, args, pid] = data;
            module = get_atom(module);
            func = get_atom(func);
            try {
              const mod = require(path.join(process.cwd(), module));
              return await mod[func].apply(null, args);
            }
            catch(err) {
              return set_tuple([set_atom('badrpc'), set_tuple([err.name, err.message, process.cwd()])]);
            }
        }
        return set_tuple([set_atom('badrpc'), 'Wrong number of args, expected moudule, function, args, pid']);
    }

    handle_cast_cast(...data) {
      if (Array.isArray(data) && data.length === 4) {
        let [module, func, args, pid] = data;
        module = get_atom(module);
        func = get_atom(func);
        try {
          const mod = require(path.join(process.cwd(), module));
          mod[func].apply(null, args);
        } catch (err) {
          console.log([set_atom('badrpc'), set_tuple([err.name, err.message, process.cwd()])]);
        }
      }
  }
}

module.exports = Rex;
