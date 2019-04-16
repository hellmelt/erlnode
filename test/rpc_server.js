const tap = require('tap');
const ErlNode = require('../index');
const erlang_node = require('./helpers/start_erlang').erlang_node;
const hostname = require('os').hostname;

const erlnode = new ErlNode('Oreo', 'js7', 0);
const nodename = 'js7@' + hostname().toLowerCase().split('.')[0];
const erlang_nodename = 'e7@' + hostname().split('.')[0];

tap.test('Receive rpc call and cast', (ct) => {
  ct.plan(2);
  erlang_node('e7', 'Oreo', 'test_rpc_server', 'run_test', nodename,
    (res) => {
      erlnode.unpublish();
      ct.equal(res, null, 'Erlang process exited as expected');
      ct.end();
    });

  setTimeout(() => {ct.resolveMatch(
    erlnode.rpc.call(erlang_nodename, 'net_adm', 'ping', [{a: nodename}]), {a: 'pong'},
    'RPC client call returned expected value');},
    1000);
});
