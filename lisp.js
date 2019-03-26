function(con, arg) { // i:"("
	
	// SYNTAX
	// (+ 1 2) - Sequence; generally denotes a method invocation
	// [+ 1 2] - Synonymous with the above
	// {'name 'Foo 'age 42} - Hash-map
	// "hello world" - String
	// r"^.*$" - Regex (compiled at read-time); flags go after the r
	// 'foo - Expands to (QUOTE foo)
	// @foo - Expands to (DEREF foo)
	// ^foo - Expands to (QUASIQUOTE foo)
	// ~foo - Expands to (UNQUOTE foo)
	// ~@foo - Expands to (SPLICE-UNQUOTE foo)
	// Numbers are interpreted as in Javascript
	// Symbols may consist of alphanumerics, as well as: +-*/$%!?<>=
	//
	//
	// STRUCTURES
	// A few structures require special handling:
	// * Atoms are objects tagged A
	// * Macros are functions tagged M
	// * Symbols are just strings; literal strings are quoted
	// * Anything with Object as its constructor is a hash-map

	// Monstrous regex to enumerate language tokens.
	let reg = _ => (/~@|(\w+)?"((\\.|[^\\"])*)"|([(){}[\]^'~@#]|[\w%+*\/$&!?<>=._-]+)/g),
	
	// Objects are tagged by simply setting a specific field on them to true.
	// This allows us to check for these types in a few characters, while
	// remaining relatively expressive.
	tag = (t, v)  => Object.assign(v, {['__' + t]: true}),
	
	// Shorthands for frequently-used functionality
	def = n => n != void 0,
	len = s => s.length,
	arr = Array.isArray,
	obj = a => a.constructor == Object,
	str = a => a instanceof String || typeof a == 'string',
	
	// Check for Clojure-ish truthiness
	tru = p => p !== false && p != null,
	
	// Partition an array
	prt = a => {
		let c = [];
		for(let i = 0; i < len(a); i += 2)
			c.push([a[i], a[i + 1]]);
		return c;
	},
	
	// Returns a new context with the specified bindings applied. If e is
	// truthy, values are (sequentially) evaluated; otherwise, they're applied
	// as-is.
	bind = (p, r = {}, e = 0) => {
		let s = Object.create(r);
		prt(p).map(([i, j]) => s[i] = e ? eval_(j, s) : j);
		return s;
	},
	
	// Output buffer
	out = [],
	
	// Read an expression into the AST
	eof = {},
	read_exp = (prog, reg, m = reg.exec(prog)) =>
		m === null            ? eof
		: m[0] == "'"         ? ['quote', read_exp(prog, reg)]
		: m[0] == '^'         ? ['quasiquote', read_exp(prog, reg)]
		: m[0] == '~'         ? ['unquote', read_exp(prog, reg)]
		: m[0] == '~@'        ? ['splice-unquote', read_exp(prog, reg)]
		: m[0] == '@'         ? ['deref', read_exp(prog, reg)]
		: m[0] == '('         ? read_seq(prog, reg, ')')
		: m[0] == '['         ? read_seq(prog, reg, ']')
		: m[0] == '{'         ? ['hash-map', ...read_seq(prog, reg, '}')]
		: m[0] == 'true'      ? true
		: m[0] == 'false'     ? false
		: m[0] == 'nil'       ? void 0
		: m[2] !== void 0     ? (m[1] && m[1][0] == 'r'
		                        ? new RegExp(m[2], m[1].slice(1))
		                        : ['quote', m[2]])
		: !isNaN(m[0])        ? +m[0]
		: m[0],
	
	// Read a list into the AST
	read_seq = (prog, reg, end) => {
		let ast = [], m;
		for(;;) {
			m = read_exp(prog, reg)
			if([')',']','}',eof].includes(m))
				if(m == end)
					break;
				else
					throw m != eof ? 'Unmatched ' + m : 'Unexpected EOF';
			ast.push(m);
		}
		
		return ast;
	},
	
	// Read a single object from a string
	read = p => read_exp(p, reg()),
	
	// Read all objects from a string
	read_all = p => read_seq(p, reg(), eof),

	// Expand a macro
	mexp = (ctx, a) => {
		while(arr(a) && len(a) && a[0] in ctx && ctx[a[0]].__M)
			a = ctx[a[0]](...a.slice(1));
		return a;
	},
	
	apply = (e, i) =>
		def(e.call) ? e.call(0, ...i)
		: str(e) ? i[0][e]
		: (obj(e) && str(i[0])) || (arr(e) && !isNaN(i[0])) ? e[i[0]]
		: null[9],

	// Evaluate an expression	
	eval_ = (a, ctx) => {
		
		// Perform macro expansions
		a = mexp(ctx, a);
		
		// Resolve a leaf
		if(!arr(a))
			return !def(a)
			  ? void 0
			: str(a)
			  ? a in ctx ? ctx[a] : null[a]
			  : a;
		
		// Return an empty list
		if(!len(a))
			return a;
		
		// Apply special form
		if(str(a[0]) && spe[a[0]])
			return spe[a[0]](ctx, ...a.slice(1));
		
		// Apply regular function
		let [e, ...i] = a.map(i => eval_(i, ctx));
		return apply(e, i);
	},
		
	// Returns a printable description of the object. Needs work.
	prn = (a, p = 1) =>
	    !def(a) ? 'nil'
		: arr(a) ? `(${a.map(e => prn(e,p)).join(' ')})`
		: str(a) ? (p ? a : `\"${a}\"`)
		: obj(a)
		  ? `{${Object.keys(a).map(k => [prn(k,p), prn(a[k],p)].join(' ')).join(' ')}}`
//		: a.constructor == RegExp
//		  ? `r\"${a.toString().slice(1, -1)}\"`
		: def(a.valueOf)
		  ? a.valueOf()
		: a,
		  
	// Retrieve a list of stored scripts in the database
	lib = #db.f({ type:'lisp_scr' }).array().reduce((d, i) => (d[i.name] = i.body, d), {}),
	
	// Special forms
	_do = (ctx, ...f) => f.map(a => eval_(a, ctx)).slice(-1)[0],
	qq = (ctx, a) =>
		!arr(a) || !len(a)
		  ? [a]
		: a[0] == 'unquote'
		  ? [eval_(a[1], ctx)]
		: a[0] == 'splice-unquote'
		  ? eval_(a[1], ctx)
		: [[].concat(...a.map(i => qq(ctx, i)))],
	
	spe = {
		def: (ctx, k, v) => cor[k] = eval_(v, ctx),
		'do': _do,
		'let': (ctx, b, ...f) => _do(bind(b, ctx, 1), ...f),
		loop: (ctx, b, ...f) => {
			let ntx = bind(b, ctx, 1), v;
			for(;;) {
				v = _do(ntx, ...f);
				if(def(v) && v.__R)
					for(let i = 0; i < len(v); i++)
						ntx[b[i * 2]] = v[i];
				else
					break;
			}
			return v;
		},
		fn: (ctx, b, ...f) => (...a) => {
			let stx = Object.create(ctx);
			b.some((j, i) => j == '&' ? stx[b[i + 1]] = a.slice(i) : (stx[j] = a[i], 0));
			return _do(stx, ...f);
		},
		defmacro: (ctx, k, b, ...f) => spe.def(ctx, k, tag('M', spe.fn(ctx, b, ...f))),
		'if': (ctx, c, t, f) => tru(eval_(c, ctx)) ? eval_(t, ctx) : def(f) ? eval_(f, ctx) : void 0,
		quote: (_, a) => a,
		quasiquote: (ctx, a) => qq(ctx, a)[0],
		macroexpand: mexp,
		'eval': (ctx, a) => eval_(eval_(a, ctx), ctx),
		load: (ctx, n) => arr(n)
		                  ? n.map(p => spe.load(0, p))
		                  : eval_(['do', ...read_all(lib[n])], cor)
	},
	
	// Core context
	cor = {		
		// Pending MAL implementation
		split: (s, a) => a.split(s),
		join:  (s, a) => def(a) ? a.join(s) : s.join(' '),
		
		// Collection operations
		'seq?': arr,
		'push!': (a, ...b) => (a.push(...b), a),  
		range:   (a, b) => [...Array(def(b) ? b : a).keys()].slice(def(b) ? a : 0),
		list:    (...a) => a, 
		cons:    (i, a) => [i, ...a],
		concat:  (...a) => [].concat(...a),
		apply: apply,
		
		// Hashmap functions
		'hash-map': (...a)        => bind(a),
		
		// Arithmetic and boolean operators
		'+':    (a, b) => a + b,
		'-':    (a, b) => a - b,
		'*':    (a, b) => a * b,
		'/':    (a, b) => a / b,
		number: a => +a,
		mod:    (a, b) => a % b,
		$eq:    (a, b) => a == b,
		$exact: (a, b) => a === b,
		$gt:    (a, b) => a > b,
		$lt:    (a, b) => a < b,
		not:     a     => !tru(a),
		
		// Atom functions
		atom:      a     => tag('A', { _v:a }),
		deref:     a     => a._v,
		'reset!': (a, v) => a._v = v,
		
		// I/O
		prn: a => (out.push(prn(a,0)), void 0),
		print: a => (out.push(prn(a)), void 0),
		'read-string': read,
		'read-all': read_all,
		
		// Type queries
		'atom?':   a => def(a) && a.__A,
		'macro?':  a => def(a) && a.__M,
		
		// Interop
		$Array:    Array,
		$Boolean:  Boolean,
		$Date:     Date,
		$Function: Function,
		$JSON:     JSON,
		$Map:      Map,
		$Math:     Math,
		$Number:   Number,
		$Object:   Object,
		$RegExp:   RegExp,
		$String:   String,
		$Set:      Set,
		$val:     a           => def(a) && def(a.valueOf) ? a.valueOf() : a,
		$new:    (c, ...a)    => new c(...a),
		$delete: (o, n)       => delete o[n],
		$typeof:  o           => typeof o,
		$isa:    (o, t)       => o instanceof t,
		$in:     (k, m)       => k in m,
		$get:    (o, n)       => o[n],
		$set:    (o, n, v)    => o[n] = v,
		$call:   (o, n, ...a) => o[n](...a),
		throw:   m           => {throw m},
		
		// Misc
		recur: (...a) => tag('R', a),
		$ST: _ST,
		$ctx: con,
//		$db: {
//			i:   q     => #db.i(q),
//			r:   q     => #db.r(q),
//			f:  (q, p) => #db.f(q, p).array(),
//			u:  (q, c) => #db.u(q, c),
//			us: (q, c) => #db.us(q, c)
//		}
	};
	cor.$env = cor;
	
	let rep = (a, p) => {
		cor['$args'] = a;
		
		let c = {ok:true};
		try {
			eval_(['do', ...read_all(p)], cor);
			c.msg = out.join('\n');
		}
		catch(e) {
			c.ok = false;
			c.msg = e.message || e;
		}
		
		if(arg.time)
			c.time = new Date() - _ST;
		out.length = 0;
		return c;
	};
	
	// Load core libs and evaluate from args
	spe.load(cor, ['_core']);
		
	return con.calling_script || con.is_scriptor ? rep : rep(arg, arg.i);
}
