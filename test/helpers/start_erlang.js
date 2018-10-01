const { exec } = require('child_process');

const erl_for_client = (sname, cookie, module, func, args, startcb, finishedcb) => {
	let cmd = `cd test; erl -noshell -sname ${sname} -setcookie ${cookie} -s ${module} ${func}`
	cmd += args ? ` ${args}` : '';
	console.log(cmd);
	const erlang = exec(cmd, null, finishedcb);

	setTimeout(startcb, 500);
}

module.exports.erl_for_client = erl_for_client;

const erl_for_server = (sname, cookie, module, func, args, finishedcb) => {
	setTimeout(() => {
		let cmd = `cd test; erl -noshell -sname ${sname} -setcookie ${cookie} -s ${module} ${func}`
		cmd += args ? ` ${args}` : '';
		console.log(cmd);
		const erlang = exec(cmd, null, finishedcb);
	}, 500);
};

module.exports.erl_for_server = erl_for_server;
