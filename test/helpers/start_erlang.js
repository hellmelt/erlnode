const { exec } = require('child_process');

const erlang_node = (sname, cookie, module, func, args, finishedcb) => {
  let cmd = `cd test; erl -noshell -sname ${sname} -setcookie ${cookie} -run ${module} ${func}`;
  cmd += args ? ` ${args}` : '';
  const erlang = exec(cmd, null, finishedcb);
};

module.exports.erlang_node = erlang_node;
