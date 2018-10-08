// Should be someting like erlInterface = ....
const erlInterface = require('./build/release/erlInterface.node');
const binary_to_term = require('../erlang.js').binary_to_term;
const term_to_binary = require('../erlang.js').term_to_binary;

// Sould be class ErlNode
class ErlNode {
  constructor(cookie, nodeName, port, acceptCallback) {
    // Todo: The cnode should be a private property
    // Should be someting like this.cnode = new erlInterface.CNode
    this.cnode = new erlInterface.CNode(
      {
        cookie: cookie,
        thisNodeName: nodeName
      });
    // Todo: These members should be private
    this.persistentReceiveCallback = [];
    this.receiveCallback = [];
    this.acceptCallback = acceptCallback;
    this.connections = {};

    this.cnode.server(port || 0);
    this.acceptLoop();
  }
  // Private method
  connect (nodeName) {
    const connection = this.cnode.connect(nodeName);
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
        for (let i = 0; i < this.persistentReceiveCallback.length; i++) {
          if (typeof this.persistentReceiveCallback[i] === 'function') {
            this.persistentReceiveCallback[i](binary_to_term(from), to, binary_to_term(buffer));
          } // Todo: Else remove item from array?
        }
        for (let i = 0; i < this.receiveCallback.length; i++) {
          if (typeof this.receiveCallback[i] === 'function') {
            this.receiveCallback[i](binary_to_term(from), to, binary_to_term(buffer));
          }
        }
        this.receiveCallback = [];

        this.receiveLoop(connection);
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
      this.receiveCallback.push(callback);
  }
  receiveCallback (callback) {
      this.persistentReceiveCallback.push(callback);
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
      throw('Invalid pid, node missing');
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
}

module.exports = ErlNode;
