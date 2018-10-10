class gen_server {

    receive (from, term) {
        if (term && term.t && term.t.length > 0) {
            if (term.t[0].a === '$gen_call') {
                call(term.t.slice(1));
            } else if (term.t[0].a === '$gen_cast') {
                cast(term.t.slice(1));
            } else {
                throw('Bad message to a gen_server');
            }
        } else {
            throw('Bad message to a gen_server');
        }
    }

    call (term) {
        if (term.length === 2) {
            from = term[0];
            data = term[1];
            let funcname;
            if (data.a) {
                funcname = data.a;
            } else if (data.t) {
                funcname = data.t[0].a;
            }
            if (funcname) {
                funcname = 'handle_call_' + funcname;
                if (typeof this[funcname] === 'function') {
                    this[funcname](data.t[1]);
                }
            }
        }
    }
}