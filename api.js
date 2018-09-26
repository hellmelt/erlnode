const erlNode = require('./build/release/erlnode.node');
const binary_to_term = require('../erlang.js').binary_to_term;
const term_to_binary = require('../erlang.js').term_to_binary;

class cNode {
	constructor(cookie, nodeName) {
		// Todo: This erlNode should be private
		this.node = new erlNode.ErlNode(
    {
      cookie: cookie,
      thisNodeName: nodeName
    });
	  // Todo: These members should be private
	  this.persistentReceiveCallback = {};
	  this.receiveCallback = {};
	}
	connect (nodeName, callback) {
		const connection = this.node.connect(nodeName);
		this.persistentReceiveCallback.connection = callback;
		this.receiveLoop(connection);
		return connection;
	}
	// If a connection is opened, ei_receive has to be listening for
	// ERL_TICK (alive polling). This functionality is here.
	// If no callbacks are registered, any messages vanishes in this method.
	//
	// Todo: Find a way to make this method private
	receiveLoop (connection) {
		erlNode.receive(connection, (status, from, to, buffer) => {
			if (status === 'ok') {
				if (typeof this.persistentReceiveCallback.connection === 'function') {
					this.persistentReceiveCallback.connection(status, binary_to_term(from), to, binary_to_term(buffer));
				}
				if (typeof this.receiveCallback.connection === 'function') {
					this.receiveCallback.connection(status, binary_to_term(from), to, binary_to_term(buffer));
					this.receiveCallback.connection = undefined;
				}
				this.receiveLoop(connection);
			} else if (status === 'closed') {
				this.persistentReceiveCallback.connection = undefined;
			}
		})
	}
	serverCb (port, callback) {
		return this.node.server(port, (connection, nodename) => {
			this.receiveLoop(connection);
			callback(connection, nodename)});
	}
	server (port) {
		const promise = new Promise((resolve, reject) => {
			this.node.server(port, (connection, nodename) => {
				this.receiveLoop(connection);
				resolve({ connection, nodename });
			})
		})
	}
	receiveCb (connection, callback) {
		this.receiveCallback.connection = callback;
	}
	receive (connection) {
		const promise = new Promise((resolve, reject) => {
			this.receiveCallback.connection = (status, from, to, term) => {
				resolve({ status, from, to, term });
			}
		});
		return promise;
	}
	send (connection, to, term) {
		erlNode.send(connection, term_to_binary(to), term_to_binary(term));
	}
	regSend (connection, to, term) {
		this.node.regSend(connection, to, term_to_binary(term));
	}

}

module.exports.cNode = cNode;
