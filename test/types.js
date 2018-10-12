const tap = require('tap');
const { is_atom, get_atom, is_tuple, get_tuple, tuple_length, is_pid, is_reference } = require('../src/types');

tap.test('atom type', (t) => {
  const atom = {a: 'atomText'};
  const notAtom = {t: ['t1', 't2']};

  t.ok(is_atom(atom), 'is_atom true for an atom');
  t.notOk(is_atom(notAtom), 'is_atom false for other object');
  t.notOk(is_atom('atomText'), 'is_atom false for string');

  t.equal(get_atom(atom), 'atomText', 'Extracts correct atom contents');
  t.notEqual(get_atom('atomText'), 'atomText', 'Does not extract atom from string');
  t.notEqual(get_atom(notAtom), 't1', 'Does not extract atom from other type');

  t.end();
});

tap.test('tuple type', (t) => {
  const tuple = {t: ['t1', {a: 'atomText'}, {t: ['t11', 't12']}, [5, 4, 3, 2, 1]]};
  const notTuple = {a: 'atomText'};

  t.ok(is_tuple(tuple), 'is_tuple is true for a tuple');
  t.notOk(is_tuple(notTuple), 'is_tuple is false for e.g. an atom');

  t.equal(4, tuple_length(tuple), 'tuple_length returns correct length');

  t.end();
});

tap.test('pid type', (t) => {
  const pid = { p: {
      node: { a: 'anders@dhcp-184-235' },
      ID: 169,
      serial: 0,
      creation: 3 } };

  t.ok(is_pid(pid), 'is_pid is true for a pid object');

  t.end();
});

tap.test('new reference type', (t) => {
  const ref =  { n:
      { node: { a: 'anders@dhcp-184-235' },
        creation: 3,
        ID: [ 139494, 3817603080, 1276906264 ] } };

  t.ok(is_reference(ref), 'is_reference returns true for a new reference object');

  t.end();
});