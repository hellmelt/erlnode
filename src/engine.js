const erlInterface = require('../build/Release/erlInterface.node');
const binary_to_term = require('../../erlang.js/api.js').binary_to_term;
const term_to_binary = require('../../erlang.js/api.js').term_to_binary;
const { is_tuple, get_tuple, tuple_length, set_tuple, is_atom, get_atom, set_atom, is_pid, is_reference } = require('./types');;

class ErlNode {
  constructor(cookie, nodeName, port, acceptCallback) {
    // Todo: The cnode should be a private property
    this.cnode = new erlInterface.CNode(
      {
        cookie: cookie,
        thisNodeName: nodeName
      });
    // Todo: These members should be private
    this.persistentReceiveCallbacks = [];
    this.receiveCallbacks = [];
    this.acceptCallback = acceptCallback;
    this.connections = {};
    this.registeredNames = {};
    this.Ref_ID = [0, 0, 0];
    this.references = new Map();

    this.cnode.server(port || 0);
    this.acceptLoop();
  }
  // Private method
  connect (nodeName) {
    let connection;
    try {
    connection = this.cnode.connect(nodeName);
    }
    catch (err) {
      throw new Error('Connect failed, error: ', err);
    }
    this.receiveLoop(connection);
    this.connections[nodeName] = connection;
    return connection;
  }
  // If a connection is opened, ei_receive has to be listening for
  // ERL_TICK (alive polling). This functionality is here.
  // If no callbacks are registered, any messages vanishes in this method.
  //
  // Todo: Find a way to make this method private
  receiveLoop (connection) {
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
          if (typeof this.references.get(this.get_ref_key(ref)) === 'function') {
            this.references.get(this.get_ref_key(ref))(reply);
            this.references.delete(this.get_ref_key(ref));
          }
        }
        for (let i = 0; i < this.persistentReceiveCallbacks.length; i++) {
          if (typeof this.persistentReceiveCallbacks[i] === 'function') {
            this.persistentReceiveCallbacks[i](from, to, data);
          } // Todo: Else remove item from array?
        }
        for (let i = 0; i < this.receiveCallbacks.length; i++) {
          if (typeof this.receiveCallbacks[i] === 'function') {
            this.receiveCallbacks[i](from, to, data);
          }
        }
        this.receiveCallbacks = [];
        this.receiveLoop(connection);

        if (typeof this.registeredNames[to] === 'function') {
          this.registeredNames[to](this, from, data);
        } else if (typeof this.registeredNames[to] === 'object' && typeof this.registeredNames[to].receive === 'function') {
          this.registeredNames[to].receive(this, from, data);
        }

      } else if (status === 'closed') {
        for (let node in this.connections) {
          if (this.connections[node] === connection) {
            delete this.connections[node];
          }
        }
      }
    })
  }
  // Private
  acceptLoop () {
    this.cnode.accept((connection, nodename) => {
      // Timeout
      if (connection === -5) {  // ETIMEDOUT
        this.acceptLoop();
      } else if (connection > 0) {
        if (typeof this.acceptCallback === 'function') {
          this.acceptCallback(connection, nodename);
        }
        this.connections[nodename] = connection;
        this.receiveLoop(connection);
        this.acceptLoop();
      }
    });
  }
  unpublish () {
    this.cnode.unpublish();
  }
  // Private
  disconnect (connection) {
    erlInterface.disconnect(connection);
  }
  receiveOnce (callback) {
      this.receiveCallbacks.push(callback);
  }
  receiveCallback (callback) {
      this.persistentReceiveCallbacks.push(callback);
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
      if (this.connections[node]) {
        connection = this.connections[node]
      } else {
        connection = this.connect(node);
      }
      erlInterface.send(connection, term_to_binary(to), term_to_binary(term));
    } else {
      throw new Error('Invalid pid, node missing');
    }
  };
  regSend (to, node, term) {
    let connection;
    if (this.connections[node]) {
      connection = this.connections[node]
    } else {
      connection = this.connect(node);
    }
    this.cnode.regSend(connection, to, term_to_binary(term));
  }
  self () {
    return binary_to_term(this.cnode.self());
  }

  creation () {
    return this.self().p.creation;
  }

  register (name, what) {
    this.registeredNames[name] = what;
    if (typeof what === 'object') {
      what.erlNode = this;
    }
  }

  make_ref () {
    const new_ref = {n: {node: this.self().p.node, creation: this.creation(), 
    ID: this.Ref_ID.slice()}};
    if (++this.Ref_ID[0] > (Math.pow(2, 18) - 1)) {
      this.Ref_ID[0] = 0;
      if (++this.Ref_ID[1] > (Math.pow(2, 18) - 1)) {
        this.Ref_ID[1] = 0;
        if (++this.Ref_ID[2] > (Math.pow(2, 18) - 1)) {
          this.Ref_ID[2] = 0;
        }
      }
    }
    return new_ref;
  }

  get_ref_key (ref) {
    let str = ref.n.node.a + ref.n.creation.toString();
    for (let i = 0; i < 3; i++) {
      str += ref.n.ID[i].toString();
    }
    return str;
  }

  call (to, node, term) {
    return new Promise((resolve, reject) => {
      const ref = this.make_ref();
      const data = set_tuple([set_atom('$gen_call'), set_tuple([this.self(), ref]), term]);
      this.regSend(to, node, data);
      this.references.set(this.get_ref_key(ref), resolve);
    }); 
  }

  cast (to, node, term) {
    const data = set_tuple([set_atom('$gen_cast'), term]);
    this.regSend(to, node, data);
  }
}

module.exports = ErlNode;
