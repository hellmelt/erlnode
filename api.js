const erlNode = require('./build/release/erlnode.node');
const binary_to_term = require('../erlang.js').binary_to_term;
const term_to_binary = require('../erlang.js').term_to_binary;

class cNode {
	constructor(cookie, nodeName) {
		this.node = new erlNode.ErlNode(
    {
      cookie: cookie,
      thisNodeName: nodeName
    });
	}
	connect (nodeName) {
		return this.node.connect(nodeName);
	}
	receiveCb (connection, callback) {
		return erlNode.receive(connection, (from, to, buffer) => {
			callback(from, to, binary_to_term(buffer))});
	}
	receive (connection) {
		const promise = new Promise((resolve, reject) => {
			erlNode.receive(connection, (from, to, buffer) => {
				resolve({from, to, term: binary_to_term(buffer)});
			})
		})
		return promise;
	}
	send (connection, to, term) {
		erlNode.send(connection, to, term_to_binary(term));
	}
	regSend (connection, to, term) {
		this.node.regSend(connection, to, term_to_binary(term));
	}

}

module.exports.cNode = cNode;
