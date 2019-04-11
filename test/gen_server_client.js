const tap = require('tap');
const hostname = require('os').hostname;
const erlang_node = require('./helpers/start_erlang').erlang_node;
const ErlNode = require('../index');

const erlName = 'e1@' + hostname().toLowerCase().split('.')[0]
const erlnode = new ErlNode('Oreo', 'js');

tap.test('Send call and cast to erlang gen_server', (ct) => {
    ct.plan(3);
    erlang_node('e1', 'Oreo', 'accu', 'start_link', 0,
    (res) => {
      erlnode.unpublish();
      ct.equal(res, null, 'Erlang gen_server exited as expected');
      ct.end();
    });
   setTimeout(async () => {
        const r1 = await erlnode.gen_server.call('accu', erlName, {t: [{a: 'add'}, 13]});
        ct.equal(r1, 13, 'call add is ok');
        erlnode.gen_server.cast('accu', erlName, {t: [{a: 'subtract'}, 3]});
        const r2 = await erlnode.gen_server.call('accu', erlName, {t: [{a: 'add'}, 12]});
        ct.equal(r2, 22, 'cast subtract is ok');
        erlnode.gen_server.cast('accu', erlName, {a: 'stop'});
    }, 200);
});
