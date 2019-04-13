const types = require('../../erlang.js/api');
const { is_tuple, get_tuple, set_tuple, is_atom, get_atom, set_atom, is_pid, is_reference } = types;

class gen_server {
  constructor() {

  }

  receive (erlNode, from, term) {
    if (!is_tuple(term)) {
      throw new Error('Bad form of message to gen_server');
    }

    const [request, ...rest] = get_tuple(term);

    if (!is_atom(request)) {
      throw new Error('Bad form of message to gen_server');
    }

    if (get_atom(request) === '$gen_call') {
      if (rest.length === 2) {
        this.handleCall(erlNode, rest);
      }
    } else if (get_atom(request) === '$gen_cast') {
      if (rest.length === 1) {
        this.handleCast(rest);
      }
    } else {
      this.handle_info(term);
    }
  }

  getFunc (data) {
    let func, args, aFunc;
    if (is_atom(data)) {
      func = get_atom(data);
    } else if (is_tuple(data)) {
      [aFunc, ...args] = get_tuple(data);
      if (is_atom(aFunc)) {
        func = get_atom(aFunc);
      }
    }
    return { func, args };
  }

  async handleCall (erlNode, [from, data]) {
    let reply;
    try {
      let { func, args } = this.getFunc(data);
      const funcname = 'handle_call_' + func;
      if (func && typeof this[funcname] === 'function') {
        reply = await this[funcname].apply(this, args);
      } else {
        reply = await this.handle_call(data);
      }
    }
    catch (err) {
      reply = set_tuple([set_atom('error'), set_tuple([err.name, err.message])]);
    }
    let tFrom;
    if (tFrom = get_tuple(from)) {
      const [pid, ref] = tFrom;
      if (is_pid(pid) && is_reference(ref)) {
        erlNode.send(pid, set_tuple([ref, reply]));
      }
    }
  }
  handle_call (data) {
    return set_tuple([set_atom('error'), 'Unhandled call, no matching function']);
  }

  handleCast ([data]) {
    try{
      let {func, args} = this.getFunc(data);
      const funcname = 'handle_cast_' + func;
      if (func && typeof this[funcname] === 'function') {
        this[funcname].apply(this, args);
      } else {
        this.handle_cast(data);
      }
    }
    catch (err) {
      console.log(`gen_server cast error: ${err.name}, ${err.message}`);
    }
  }
  handle_cast (data) {
    console.log('Unhandled cast: ', data);
  }

  handle_info (data) {
    console.log('Unexpected message: ', data);
  }
}

module.exports = gen_server;
