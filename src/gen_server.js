const types = require('./types');

const { is_tuple, get_tuple, tuple_length, set_tuple, is_atom, get_atom, set_atom, is_pid, is_reference } = types;

class gen_server {
  constructor() {

  }

  receive (erlNode, from, term) {
    if (!is_tuple(term)) {
      throw('Bad form of message to gen_server');
    }

    const [request, ...rest] = get_tuple(term);

    if (!is_atom(request)) {
      throw('Bad form of message to gen_server');
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

  handleCall (erlNode, [from, data]) {
    let { func, args } = this.getFunc(data);
    let reply;
    const funcname = 'handle_call_' + func;
    if (func && typeof this[funcname] === 'function') {
      reply = this[funcname].apply(null, args);
    } else {
      reply = this.handle_call(data);
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
    let {func, args} = this.getFunc(data);
    const funcname = 'handle_cast_' + func;
    if (func && typeof this[funcname] === 'function') {
      console.log('handleCast: ', funcname, args);
      this[funcname].apply(null, args);
    } else {
      this.handle_cast(data);
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
