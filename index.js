const enode = require('./src/engine');

class erlnode {
  constructor(cookie, nodeName, port, acceptCallback) {
      this._enode = new enode(cookie, nodeName, port, acceptCallback);
      this.gen_server = {};
      this.gen_server.call = (to, node, term) => {
        return this._enode.call(to, node, term);
      }
      this.gen_server.cast = (to, node, term) => {
          this._enode.cast(to, node, term);
      }
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

module.exports = erlnode;
