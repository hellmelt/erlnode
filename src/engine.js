const erlInterface = require('../build/Release/erlInterface.node');
const binary_to_term = require('../../erlang.js/api.js').binary_to_term;
const term_to_binary = require('../../erlang.js/api.js').term_to_binary;
const { is_tuple, get_tuple, set_tuple, set_atom, is_reference } = require('../../erlang.js/api.js');;

class ErlNode {
  constructor(cookie, nodeName, port, acceptCallback) {
    // Todo: The cnode should be a private property
    this._cnode = new erlInterface.CNode(
      {
        cookie: cookie,
        thisNodeName: nodeName
      });
    // Todo: These members should be private
    this._persistentReceiveCallbacks = [];
    this._receiveCallbacks = [];
    this._acceptCallback = acceptCallback;
    this._connections = {};
    this.registeredNames = {};
    this._Ref_ID = [0, 0, 0];
    this._references = new Map();

    this._cnode.server(port || 0);
    this._acceptLoop();
  }
  // Private method
  _connect (nodeName) {
    let connection;
    try {
      connection = this._cnode.connect(nodeName);
    }
    catch (err) {
      throw new Error('Connect failed, error: ', err);
    }
    this._receiveLoop(connection);
    this._connections[nodeName] = connection;
    return connection;
  }
  // If a connection is opened, ei_receive has to be listening for
  // ERL_TICK (alive polling). This functionality is here.
  // If no callbacks are registered, any messages vanishes in this method.
  //
  // Todo: Find a way to make this method private
  _receiveLoop (connection) {
    erlInterface.receive(connection, (status, from, to, buffer) => {
      if (status === 'ok') {
        if (from) {
          from = binary_to_term(from);
        }
        if (!(typeof to === 'string')) {
          to = binary_to_term(to);
        }
        const data = binary_to_term(buffer);
        if (is_tuple(data) && is_reference(get_tuple(data)[0])) {
          const ref = get_tuple(data)[0];
          const reply = get_tuple(data)[1];
          if (typeof this._references.get(this._get_ref_key(ref)) === 'function') {
            process.nextTick(this._references.get(this._get_ref_key(ref)), reply);
            this._references.delete(this._get_ref_key(ref));
          }
        }
        for (let i = 0; i < this._persistentReceiveCallbacks.length; i++) {
          if (typeof this._persistentReceiveCallbacks[i] === 'function') {
            process.nextTick(this._persistentReceiveCallbacks[i], from, to, data);
          } // Todo: Else remove item from array?
        }
        for (let i = 0; i < this._receiveCallbacks.length; i++) {
          if (typeof this._receiveCallbacks[i] === 'function') {
            process.nextTick(this._receiveCallbacks[i], from, to, data);
          }
        }
        this._receiveCallbacks = [];
        this._receiveLoop(connection);

        if (typeof this.registeredNames[to] === 'function') {
          process.nextTick(this.registeredNames[to], this, from, data);
        } else if (typeof this.registeredNames[to] === 'object' && typeof this.registeredNames[to].receive === 'function') {
          process.nextTick(this.registeredNames[to].receive.bind(this.registeredNames[to]), this, from, data);
        }

      } else if (status === 'closed') {
        for (let node in this._connections) {
          if (this._connections[node] === connection) {
            delete this._connections[node];
          }
        }
      }
    })
  }
  // Private
  _acceptLoop () {
    this._cnode.accept((connection, nodename) => {
      // Timeout
      if (connection === -5) {  // ETIMEDOUT
        this._acceptLoop();
      } else if (connection > 0) {
        if (typeof this._acceptCallback === 'function') {
          this._acceptCallback(connection, nodename);
        }
        this._connections[nodename] = connection;
        this._receiveLoop(connection);
        this._acceptLoop();
      }
    });
  }
  unpublish () {
    this._cnode.unpublish();
  }
  // Private
  _disconnect (connection) {
    erlInterface.disconnect(connection);
  }
  receiveOnce (callback) {
      this._receiveCallbacks.push(callback);
  }
  receiveCallback (callback) {
      this._persistentReceiveCallbacks.push(callback);
  }
  receive () {
    return new Promise((resolve, reject) => {
      this.receiveOnce((from, to, term) => {
        resolve({ from, to, term });
      });
    });
  }
  send (to, term) {
    if (to && to.p && to.p.node && to.p.node.a) {
      const node = to.p.node.a;
      let connection;
      if (this._connections[node]) {
        connection = this._connections[node]
      } else {
        connection = this._connect(node);
      }
      erlInterface.send(connection, term_to_binary(to), term_to_binary(term));
    } else {
      throw new Error('Invalid pid, node missing');
    }
  };
  regSend (to, node, term) {
    let connection;
    if (this._connections[node]) {
      connection = this._connections[node]
    } else {
      connection = this._connect(node);
    }
    this._cnode.regSend(connection, to, term_to_binary(term));
  }
  self () {
    return binary_to_term(this._cnode.self());
  }

  _creation () {
    return this.self().p.creation;
  }

  register (name, what) {
    this.registeredNames[name] = what;
    if (typeof what === 'object') {
      what.erlNode = this;
    }
  }

  _make_ref () {
    const new_ref = {n: {node: this.self().p.node, creation: this._creation(), 
    ID: this._Ref_ID.slice()}};
    if (++this._Ref_ID[0] > (Math.pow(2, 18) - 1)) {
      this._Ref_ID[0] = 0;
      if (++this._Ref_ID[1] > (Math.pow(2, 18) - 1)) {
        this._Ref_ID[1] = 0;
        if (++this._Ref_ID[2] > (Math.pow(2, 18) - 1)) {
          this._Ref_ID[2] = 0;
        }
      }
    }
    return new_ref;
  }

  _get_ref_key (ref) {
    let str = ref.n.node.a + ref.n.creation.toString();
    for (let i = 0; i < 3; i++) {
      str += ref.n.ID[i].toString();
    }
    return str;
  }

  call (to, node, term) {
    return new Promise((resolve, reject) => {
      const ref = this._make_ref();
      const data = set_tuple([set_atom('$gen_call'), set_tuple([this.self(), ref]), term]);
      this.regSend(to, node, data);
      this._references.set(this._get_ref_key(ref), resolve);
    }); 
  }

  cast (to, node, term) {
    const data = set_tuple([set_atom('$gen_cast'), term]);
    this.regSend(to, node, data);
  }
}

module.exports = ErlNode;
