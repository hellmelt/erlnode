const erlInterface= require('./build/release/erlnode.node');

console.log(erlInterface.hej());

console.log(erlInterface.erlConnectInit(1, 'Oreo', 0));

console.log(erlInterface.erlConnect('anders@dhcp-184-203'));
