const gen_server = require('./gen_server');
const _ = require('lodash');

class Lodash extends gen_server {
    constructor() {
        super();
    }

    handle_call (data) {
        console.log('lodash handle_call: ', data);
        let { func, args } = this.getFunc(data);
        if (func && typeof _[func] === 'function') {
            console.log('Data: ', data, 'Func: ', func, 'Args: ', args);
            return _[func](...args);
        }
        throw new Error('Call to undefined lodash function: ', data);
    }
}

module.exports = Lodash;
