function(cnt, arg) { // i:"(-> $env keys sort print)"
	
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
	// Symbols may consist of alphanumerics, as well as: +-*/$%#!?<>=
	//
	//
	// STRUCTURES
	// A few structures require special handling:
	// * Atoms are objects tagged A
	// * Macros are functions tagged M
	// * Symbols are just strings; literal strings are quoted
	// * Anything with Object as its constructor is a hash-map

	// Monstrous regex to enumerate language tokens.
	let reg = _ => (/~@|(\w+)?"((\\.|[^\\"])*)"|([(){}[\]^'~@]|[\w%+*\/#$&!?<>=._-]+)|;[^\n]*/g),
	
	// Objects are tagged by simply setting a specific field on them to true.
	// This allows us to check for these types in a few characters, while
	// remaining relatively expressive.
	tag = (t, v) => (v['__' + t] = true, v), 
	
	// Shorthands for frequently-used functionality
	def = n => n != void 0,
	len = s => s.length,
	arr = Array.isArray,
	con = a => a.constructor,
	obj = a => con(a) == Object,
	set = a => con(a) == Set,
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
	
	// Destructure b into symbol tree of in a
	des = (ctx, a, b) => (arr(a) ? a.some((x, i) => (x == '&' ? (ctx[a[i + 1]] = b.slice(i), 1) : des(ctx, x, b[i]))) : ctx[a] = b, 0),
	
	// Returns a new context with the specified bindings applied. If e is
	// truthy, values are (sequentially) evaluated; otherwise, they're applied
	// as-is.
	bind = (p, r = {}, e = 0) => {
		let s = Object.create(r);
		prt(p).map(([i, j]) => des(s, i, e ? eval_(j, s) : j));
		return s;
	},
	
	// Output buffer
	out = [],
	
	// Read an expression into the AST
	eof = {},
	nop = {},
	read_exp = (prog, reg, [m,r,s] = reg.exec(prog) || [eof]) =>
		m[0] == ';'    ? nop
		: m == "'"     ? ['quote', read_exp(prog, reg)]
		: m == '^'     ? ['quasiquote', read_exp(prog, reg)]
		: m == '~'     ? ['unquote', read_exp(prog, reg)]
		: m == '~@'    ? ['splice-unquote', read_exp(prog, reg)]
		: m == '@'     ? ['deref', read_exp(prog, reg)]
		: m == '('     ? read_seq(prog, reg, ')')
		: m == '['     ? read_seq(prog, reg, ']')
		: m == '{'     ? ['hash-map', ...read_seq(prog, reg, '}')]
		: m == 'true'  ? true
		: m == 'false' ? false
		: m == 'nil'   ? void 0
		: s !== void 0 ? (r && r[0] == 'r'
		                 ? new RegExp(s, r.slice(1))
		               : ['quote', s])
		: !isNaN(m)    ? +m
		: m,
	
	// Read a list into the AST
	read_seq = (prog, reg, end) => {
		let ast = [], m;
		for(;;) {
			m = read_exp(prog, reg)
			if([')',']','}',eof].includes(m)) {
				if(m == end)
					return ast;
				throw 'Unexpected ' + (m != eof ? m : 'EOF');
			}
			if(m != nop)
				ast.push(m);
		}
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
		: set(e) ? e.has(i[0]) && i[0]
		: (obj(e) && str(i[0])) || (arr(e) && !isNaN(i[0])) ? e[i[0]]
		: null[typeof e],

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
		!def(a)
		  ? 'nil'
		: arr(a)
		  ? `(${a.map(e => prn(e,p)).join(' ')})`
		: set(a)
		  ? `#{${[...a].map(e => prn(e,p)).join(' ')}}`
		: str(a)
		  ? (p ? a : `\"${a}\"`)
		: obj(a)
		  ? `{${Object.keys(a).map(k => [prn(k,p), prn(a[k],p)].join(' ')).join(' ')}}`
		: def(a.valueOf)
		  ? a.valueOf()
		: a,
		  
	// Retrieve a list of stored scripts in the database
	lib = #db.f({ type:'lisp_scr' }).array().reduce((d, i) => (d[i.name] = i.body, d), {}),
	
	// Special forms
	_do = (ctx, ...f) => f.map(a => eval_(a, ctx)).slice(-1)[0],
	
	qqn = 0,
	qq = (ctx, a) => {
		let stx = '__s' in ctx ? ctx : bind(['__s', qqn++], ctx),
		    [n,p] = str(a) ? a.split(/(?=#$)/) : [];
		
		return p
			  ? [n + '__auto_' + stx.__s]
			: !arr(a) || !len(a)
			  ? [a]
			: a[0] == 'unquote'
			  ? [eval_(a[1], stx)]
			: a[0] == 'splice-unquote'
			  ? eval_(a[1], stx)
			: [[].concat(...a.map(i => qq(stx, i)))]
	},
	
	spe = {
		def: (ctx, k, v) => cor[k] = eval_(v, ctx),
		'do': _do,
		'let': (ctx, b, ...f) => _do(bind(b, ctx, 1), ...f),
		loop: (ctx, b, ...f) => {
			let v, e = 1;
			for(;;) {
				ctx = bind(b, ctx, e);
				v = _do(ctx, ...f);
				if(def(v) && v.__R)
					for(let i = 0; i < len(v); i++) {
						b = b.slice();
						b[i * 2 + 1] = v[i];
						e = 0;
					}
				else
					return v;
			}
		},
		fn: (ctx, b, ...f) => (...a) => {
			let stx = bind([], ctx);
			des(stx, b, a);
			return _do(stx, ...f);
		},
		defmacro: (ctx, k, b, ...f) => spe.def(ctx, k, tag('M', spe.fn(ctx, b, ...f))),
		'if': (ctx, c, t, f) => tru(eval_(c, ctx)) ? eval_(t, ctx) : def(f) ? eval_(f, ctx) : void 0,
		quote: (_, a) => a,
		quasiquote: (ctx, a) => qq(ctx, a)[0],
		macroexpand: mexp,
		'eval': (ctx, a) => eval_(eval_(a, ctx), ctx),
		load: (ctx, n) => arr(n)
		                  ? n.map(p => spe.load(ctx, p))
		                  : eval_(['do', ...read_all(lib[n])], ctx)
	},
	
	// Core context
	cor = {		
		// Collection operations
		'$push!': (a, ...b) => (a.push(...b), a),  
		range:   (a, b)    => [...Array(def(b) ? b : a).keys()].slice(def(b) ? a : 0),
		list:    (...a)    => a, 
		cons:    (i, a)    => [i, ...a],
		concat:  (...a)    => [].concat(...a),
		apply: apply,
		
		// Hashmap functions
		'hash-map': (...a) => bind(a),
		set:         a     => new Set(a),
		
		// Arithmetic and boolean operators
		'+':    (a, b) => a + b,
		'-':    (a, b) => a - b,
		'*':    (a, b) => a * b,
		'/':    (a, b) => a / b,
		mod:    (a, b) => a % b,
		number:  a     => +a,
		$eq:    (a, b) => a == b,
		$exact: (a, b) => a === b,
		$gt:    (a, b) => a > b,
		$lt:    (a, b) => a < b,
		not:     a     => !tru(a),
		
		// I/O
		'prn-str':   a => prn(a,0),
		'print-str': a => prn(a),
		'read-string': read,
		'read-all': read_all,
		
		// Interop
		$val:        a           => def(a) && def(a.valueOf) ? a.valueOf() : a,
		$new:       (c, ...a)    => new c(...a),
		'$delete!': (o, n)       => delete o[n],
		$typeof:     o           => typeof o,
		$isa:       (o, t)       => o instanceof t,
		$in:        (k, m)       => k in m,
		$get:       (o, n)       => o[n],
		'$set!':    (o, n, v)    => o[n] = v,
		$call:      (o, n, ...a) => o[n](...a),
		throw:       m           => {throw m},
		
		$Array: Array,
		$Boolean: Boolean,
		$Date: Date,
		$Function: Function,
		$JSON: JSON,
		$Map: Map,
		$Math: Math,
		$Number: Number,
		$Object: Object,
		$RegExp: RegExp,
		$Set: Set,
		$String: String,
		
		// Misc
		atom: a => tag('A', {__a:a}),
		$ST: _ST,
		$ctx: cnt,
		$out: out,
		$dbg: a => #D(a),
	};
	
	let rep = (p, a = {}, s = new Date()) => {
		cor.$args = a;
		
		let c = {ok:true};
		try {
			eval_(['do', ...read_all(p)], cor);
		}
		catch(e) {
			c.ok = false;
			out.unshift(e.message || e, "`D#######`", "\n");
		}
		
		// Calculate execution time if requested
		if(a.time)
			c.time = new Date() - s;

		c.msg = out.join('');
		out.length = 0;
		
		return c;
	};
	rep.env = cor.$env = cor;
	
	// Load privileged libraries
	spe.load(Object.assign(bind([], cor), {
		$db: {
			i:   q     => #db.i(q),
			r:   q     => #db.r(q),
			f:  (q, p) => #db.f(q, p).array(),
			u:  (q, c) => #db.u(q, c),
			us: (q, c) => #db.us(q, c)
		}

	}), ['_core']);
	//}), []);
	
	// Run script from args, or return evaluator
	return arg ? rep(arg.i, arg, _ST) : rep;
}
