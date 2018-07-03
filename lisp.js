function(con, arg) { // i:"(-> %root keys prn)"
	
	// SYNTAX
	// (+ 1 2 3) - Sequence; generally denotes a method invocation
	// [1 2 3 4] - Group [see below]
	// {'name "Foo" 'age 42} - Hashmap
	// "hello world" - String
	// r"^.*$" - Regular expression (compiled at read-time)
	// 'foo - Expands to (QUOTE foo)
	// @foo - Expands to (DEREF foo)
	// ^foo - Expands to (QUASIQUOTE foo) [see NOTES below]
	// ~foo - Expands to (UNQUOTE foo)
	// ~@foo - Expands to (SPLICE-UNQUOTE foo)
	// Numbers are interpreted as in Javascript
	// Symbols may consist of alphanumerics, as well as: +-*/$%!?<>=
	//
	//
	// STRUCTURES
	// A few structures require special handling:
	// * Atoms are objects tagged ATM
	// * Symbols are strings tagged SYM
	// * Macros are functions tagged MAC
	// * Any array is a sequence
	// * Any sequence tagged GRP is a group
	// * Anything with Object as its constructor is a hash-map
	//
	// Anything else is simply considered self-evaluating.
	//
	//
	// OTHER NOTES
	// * "Groups" are just sequences with an evaluator-hint attached to prevent
	//   them from being treated as a function invocation. When a group is
	//   processed by the evaluator, its contents are evaluated and returned as
	//   a plain sequence. There is no notion of a vector here; "sequences" are
	//   backed by variable-length arrays anyway, so the distinction would mean
	//   little.
	// * The quasiquote/syntax-quote symbol in this implementation is the
	//   circumflex accent (^) rather than the backtick (`), as otherwise macros
	//   become very cumbersome to type inside of multiline strings (as is the
	//   usual case in Hackmud). Meta information, if/when it's added, will
	//   likely not have a reader macro associated with it.
	// * This implementation has no notion of comments. The Hackmud preprocessor
	//   automatically strips anything that looks like a JS comment without
	//   tallying it towards the script's character count, so the expectation is
	//   that those will be used instead.


	// Big ugly/beautiful regex for matching the language grammar. Notably, a
	// program with an unmatched opening parenthesis will run until the
	// interpreter times out. This sucks for debugging, but saves characters.
	let reg = _ => (/~@|(\w+)?"((\\.|[^\\"])*)"|([(){}[\]^'~@#]|:?[\w%+*\/$&!?<>=._-]+)/g),
	
	// Objects are tagged by simply setting a specific field on them to true.
	// This allows us to check for these types in a few characters, while
	// remaining relatively expressive.
	tag = (t, v)  => Object.assign(v, {['_' + t]: true}),
	sym =  a      => tag('SYM', a),
	
	// Shorthands for frequently-used functionality
	def = n => n != void 0,
	len = s => s.length,
	arr = Array.isArray,
	
	// Check for Clojure-ish truthiness
	tru = p => p !== false && p != null,
	
	// Pair off elements in an array
	// Example: [0 1 2 3 4 5] becomes [[0 1] [2 3] [4 5]]
	fold = a => {
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
		fold(p).map(([i, j]) => s[i] = e ? eval_(j, s) : j);
		return s;
	},
	
	// Output buffer
	out = [],
	
	// Read an expression into the AST
	read_exp = (prog, reg, m = reg.exec(prog)) =>
		m[0] == "'"           ? [sym('quote'), read_exp(prog, reg)]
		: m[0] == '^'         ? [sym('quasiquote'), read_exp(prog, reg)]
		: m[0] == '~'         ? [sym('unquote'), read_exp(prog, reg)]
		: m[0] == '~@'        ? [sym('splice-unquote'), read_exp(prog, reg)]
		: m[0] == '@'         ? [sym('deref'), read_exp(prog, reg)]
		: m[0] == '('         ? read_list(prog, reg, ')')
		: m[0] == '['         ? tag('GRP', read_list(prog, reg, ']'))
		: m[0] == '{'         ? [sym('hash-map'), ...read_list(prog, reg, '}')]
		: m[0] == 'true'      ? true
		: m[0] == 'false'     ? false
		: m[0] == 'nil'       ? void 0
		: m[0][0] == ':'      ? m[0].slice(1)
		: m[2] !== void 0     ? (m[1] && m[1][0] == 'r' ? new RegExp(m[2], m[1].slice(1)) : m[2])
		: !isNaN(m[0])        ? Number.parseFloat(m[0])
		: sym(m[0]),
	
	// Read a list into the AST
	read_list = (prog, reg, end) => {
		var ast = [];
		for(let m; (m = reg.exec(prog));) {
			if(m[0] == end)
				break;
			ast.push(read_exp(prog, reg, m));
		}
		return ast;
	},
	
	// Convert a string into an AST
	read = prog => [sym('do'), ...read_list(prog, reg())],

	// Expand a macro
	mexp = (ctx, a) => {
		while(arr(a) && len(a) && a[0]._SYM && def(ctx[a[0]]) && ctx[a[0]]._MAC)
			a = ctx[a[0]](...a.slice(1));
		return a;
	},

	// Evaluate an expression	
	eval_ = (a, ctx) => {
		
		// Perform macro expansions
		a = mexp(ctx, a);
		
		// Resolve a leaf
		if(!arr(a))
			return !def(a)
			  ? void 0
			: a._SYM
			  ? ctx[a]
			  : a;
		
		// Return an empty list
		if(!len(a))
			return a;
		
		// Apply special form
		if(a[0]._SYM && spe[a[0]])
			return spe[a[0]](ctx, ...a.slice(1));
		
		// Apply regular function
		let e = a.map(i => eval_(i, ctx)),
		     i = e.slice(1);
		return a._GRP ? e // Evaluated contents are returned
			: def(e[0]) && def(e[0].call) ? e[0].call(0, ...i) // Execute an object's call member
			: null[0]; // Spit out an error
	},
		
	// Returns a printable description of the object. Needs work.
	print = a =>
	    !def(a) ? 'nil'
		: arr(a) ? `( ${a.map(e => print(e)).join(' ')} )`
		: a.constructor == Object
		  ? `{ ${Object.keys(a).map(k => [k, a[k]].join(' ')).join(', ')} }`
		: def(a.valueOf)
		  ? a.valueOf()
		: a,
		  
	// Retrieve a list of stored scripts in the database
	lib = #db.f({ type:'lisp_scr' }).array().reduce((d, i) => (d[i.name] = i.scr, d), {}),
	
	// Special forms
	_do = (ctx, ...f) => f.map(a => eval_(a, ctx)).slice(-1)[0],
	qq = (ctx, a) =>
		!arr(a) || !len(a)
		  ? [a]
		: a[0] == 'unquote' && a[0]._SYM
		  ? [eval_(a[1], ctx)]
		: a[0] == 'splice-unquote' && a[0]._SYM
		  ? eval_(a[1], ctx)
		: [[].concat(...a.map(i => qq(ctx, i)))],
	
	spe = {
		def: (ctx, k, v) => ctx[k] = eval_(v, ctx),
		'do': _do,
		'let': (ctx, p, ...f) => _do(bind(p, ctx, 1), ...f),
		fn: (ctx, b, ...f) => (...a) => {
			let stx = Object.create(ctx);
			b.some((j, i) => j == '&' ? stx[b[i + 1]] = a.slice(i) : (stx[j] = a[i], 0));
			return _do(stx, ...f);
		},
		defmacro: (ctx, k, b, ...f) => spe.def(ctx, k, tag('MAC', spe.fn(ctx, b, ...f))),
		'if': (ctx, c, t, f) => tru(eval_(c, ctx)) ? eval_(t, ctx) : def(f) ? eval_(f, ctx) : void 0,
		quote: (_, a) => a,
		quasiquote: (ctx, a) => qq(ctx, a)[0],
		macroexpand: mexp,
		'eval': (ctx, a) => eval_(a, cor),
		'while': (ctx, p, ...f) => {
			while(eval_(p, ctx))
				_do(ctx, ...f);
		},
		load: (ctx, n) => (arr(n) ? n : [n]).forEach(p => eval_(read(lib[p]), ctx))
	},
	
	// Core context
	cor = {		
		// Pending MAL implementation
		split:  (s, a)    => a.split(s),
		join:   (s, a)    => def(a) ? a.join(s) : s.join(' '),
		
		// Collection operations
		range:    (a, b)    => [...Array(b).keys()].slice(a),
		cons:     (i, a)    => [i, ...a],
		conj:     (a, ...i) => [...a, ...i],
		concat:   (...a)    => [].concat(...a),
		apply:    (f, ...a) => f(...[].concat(...a)),
		
		// Hashmap functions
		'hash-map': (...a)        => bind(a),
		
		// Arithmetic and boolean operators
		$add:   (a, b) => a + b,
		$sub:   (a, b) => a - b,
		$mul:   (a, b) => a * b,
		$div:   (a, b) => a / b,
		mod:    (a, b) => a % b,
		$eq:    (a, b) => a == b,
		$exact: (a, b) => a === b,
		$gt:    (a, b) => a > b,
		$lt:    (a, b) => a < b,
		not:     a     => !tru(a),
		
		// Atom functions
		atom:      a     => tag('ATM', { _v:a }),
		deref:     a     => a._v,
		'reset!': (a, v) => a._v = v,
		
		// I/O
		prn: a => (out.push(print(a)), void 0),
		log: a => #D(print(a)),
		'read-string': read,
		
		// Type queries
		'atom?':   a => def(a) && a._ATM,
		'symbol?': a => def(a) && a._SYM,
		'group?':  a => def(a) && a._GRP,
		'macro?':  a => def(a) && a._MAC,
		
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
		$valueOf: a           => def(a) && def(a.valueOf) ? a.valueOf() : a,
		$new:    (c, ...a)    => new c(...a),
		$delete: (o, n)       => delete o[n],
		$typeof:  o           => typeof o,
		$isa:    (o, t)       => o instanceof t,
		$in:     (k, m)       => k in m,
		$get:    (o, n)       => o[n],
		$set:    (o, n, v)    => o[n] = v,
		$call:   (o, n, ...a) => o[n](...a),
		$throw:   m           => {throw m},
		
		// Misc
		symbol: sym,
		keyword: a => def(a._KWD),
		'_ST': _ST,
		'context': con,
		'db': {
			i:   q     => #db.i(q),
			r:   q     => #db.r(q),
			f:  (q, p) => #db.f(q, p).array(),
			u:  (q, c) => #db.u(q, c),
			us: (q, c) => #db.us(q, c)
		},
		'%tag': tag,
		'%lib': lib
	};
	cor['%root'] = cor;
	
	let lisp = {
		rep: (a, p) => {
			cor['args'] = a;
			eval_(read(p), cor);
			let c = out.join('\n');
			out.length = 0;
			return c;
		},
		
		save: (n, p) => {
			if(con.caller == 'hg') {
				let o = { type:'lisp_scr', name:n };
				#db.us(o, { $setOnInsert:o, $set:{ scr:p } });
			}
		}
	};
	
	// Return utility object
	if(con.calling_script || con.is_scriptor)
		return lisp;
	
	// Load core libs and evaluate from args
	spe.load(cor, 'core');
	return lisp.rep(arg, arg.i);
}
