const is_boolean = (term) => {
  return (typeof term === 'boolean');
};

const is_number = (term) => {
  return (typeof term === 'number');
};

const is_string = (term) => {
  return (typeof term === 'string');
};

const is_undefined = (term) => {
  return (typeof term === 'undefined');
};

const is_array = (term) => {
  return (Array.isArray(term));
};

const is_list = (term) => {
  return is_array(term);
};

const is_atom = (term) => {
  return (typeof term === 'object' && (term.hasOwnProperty('a') || term.hasOwnProperty('atom')));
};

const is_binary = (term) => {
  return (typeof term === 'object' && (term.hasOwnProperty('b') || term.hasOwnProperty('binary')));
};

const is_tuple = (term) => {
  return (typeof term === 'object' && ((term.hasOwnProperty('t') && is_array(term.t)) || (term.hasOwnProperty('tuple') && is_array(term.tuple))));
};

const is_pid = (term) => {
  return (typeof term === 'object' && (term.hasOwnProperty('p') || term.hasOwnProperty('pid')));
};

const is_reference = (term) => {
  return (typeof term === 'object' && (term.hasOwnProperty('n') || term.hasOwnProperty('reference')));
};

const get_atom = (atom) => {
  return is_atom(atom) ? (atom.a ? atom.a : atom.atom) : undefined;
};

const get_binary = (binary) => {
  return is_binary(binary) ? (binary.b ? binary.b : binary.binary) : undefined;
};

const get_tuple = (tuple) => {
  return is_tuple(tuple) ? (tuple.t ? tuple.t : tuple.tuple) : undefined;
};

const get_pid = (pid) => {
  return is_pid(pid) ? (pid.p ? pid.p : pid.pid) : undefined;
};

const get_reference = (reference) => {
  return is_reference(reference) ? (reference.r ? reference.r : reference.reference) : undefined;
};

const tuple_length = (tuple) => {
  return is_tuple(tuple) ? get_tuple(tuple).length : undefined;
};

const set_tuple = (array) => {
  return {t: array};
};

const set_atom = (atomText) => {
  return {a: atomText};
};

module.exports = {
  is_boolean,
  is_number,
  is_string,
  is_undefined,
  is_array,
  is_list,
  is_atom,
  is_binary,
  is_tuple,
  is_pid,
  is_reference,
  get_atom,
  get_binary,
  get_tuple,
  get_pid,
  get_reference,
  tuple_length,
  set_tuple,
  set_atom
};
