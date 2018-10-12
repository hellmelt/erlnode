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
        this.call(erlNode, rest);
      }
    } else if (get_atom(request) === '$gen_cast') {
      if (rest.length === 1) {
        this.cast(rest);
      }
    } else {
      throw('Bad form of message to gen_server');
    }
  }

  call (erlNode, [from, data]) {
    let func, args;
    if (is_atom(data)) {
      func = get_atom(data);
    } else if (is_tuple(data)) {
      [funca, args] = get_tuple(data);
      if (is_atom(funca)) {
        func = get_atom(funca);
      }
    }

    let reply;
    const funcname = 'handle_call_' + func;
    if (func && typeof this[funcname] === 'function') {
      reply = this[funcname](args);
    } else {
      reply = this.handle_call(data);
    }

    let fromt;
    if (fromt = get_tuple(from)) {
      const [pid, ref] = fromt;
      if (is_pid(pid) && is_reference(ref)) {
        erlNode.send(pid, set_tuple([ref, reply]));
      }
    }
  }

  handle_call (data) {
    return set_tuple([set_atom('error'), 'No matching function']);
  }

}

module.exports = gen_server;
