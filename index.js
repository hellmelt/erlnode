const ENode = require('./src/engine');
const { set_tuple, set_atom } = require('../erlang.js/api');

class ErlNode {
  constructor(cookie, nodeName, port, acceptCallback) {
      this._enode = new ENode(cookie, nodeName, port, acceptCallback);

      // gen_server client call and cast
      this.gen_server = {};
      this.gen_server.call = (to, node, term) => {
        return this._enode.call(to, node, term);
      };
      this.gen_server.cast = (to, node, term) => {
          this._enode.cast(to, node, term);
      };

      // rpc client call and cast
      this.rpc = {};
      this.rpc.call = (node, module, func, args) => {
        return this.gen_server.call('rex', node, set_tuple([set_atom('call'), set_atom(module), set_atom(func), args, set_atom('user')]));
      };
      this.rpc.cast = (node, module, func, args) => {
        this.gen_server.cast('rex', node, set_tuple([set_atom('cast'), set_atom(module), set_atom(func), args, set_atom('user')]));
      };
  }
  unpublish () {
      this._enode.unpublish();
  }
  receiveOnce (callback) {
      this._enode.receiveOnce(callback);
  }
  receiveCallback (callback) {
      this._enode.receiveCallback(callback);
  }
  receive () {
      return this._enode.receive();
  }
  send (to, term) {
      this._enode.send(to, term);
  }
  regSend (to, node, term) {
      this._enode.regSend(to, node, term);
  }
  self () {
      return this._enode.self();
  }
  register (name, what) {
      this._enode.register(name, what);
  }
}

module.exports = ErlNode;
