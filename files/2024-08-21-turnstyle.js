"use strict";

class EvalError extends Error {
    constructor(msg) {
        super(msg);
        this.name = "EvalError";
    }
}

// Simple implementation of arbitrary-precision rational numbers.
class Rational {
    constructor(numerator, denominator = 1n) {
        const gcd = Rational._gcd(numerator, denominator);
        this._num = numerator / gcd;
        this._denom = denominator / gcd;
    }

    add(that) {
        return new Rational(
            this._num * that._denom + that._num * this._denom,
            this._denom * that._denom,
        );
    }

    sub(that) {
        return new Rational(
            this._num * that._denom - that._num * this._denom,
            this._denom * that._denom,
        );
    }

    mul(that) {
        return new Rational(this._num * that._num, this._denom * that._denom);
    }

    div(that) {
        return new Rational(this._num * that._denom, this._denom * that._num);
    }

    mod(that) {
        const lhs = this.toBigInt();
        const rhs = that.toBigInt();
        if (lhs === null || rhs === null) {
            throw new Error("Rational: modulo arguments must be integrals");
        }
        return new Rational(lhs % rhs);
    }

    eq(that)  { return this._num === that._num && this._denom === that._denom; }
    lt(that)  { return this._num * that._denom <  that._num * this._denom; }
    gt(that)  { return this._num * that._denom >  that._num * this._denom; }
    lte(that) { return this._num * that._denom <= that._num * this._denom; }
    gte(that) { return this._num * that._denom >= that._num * this._denom; }

    toNumber() { return Number(this._num / this._denom) }

    toBigInt() {
        if (this._denom === 1n) return this._num;
        return null;
    }

    toString() {
        if (this._denom === 1n) return this._num.toString();
        return `${this._num}/${this._denom}`;
    }

    static _gcd(a, b) {
        while (b !== 0n) {
            const tmp = a % b;
            a = b;
            b = tmp;
        }
        return a;
    }
}

// Stores a value either as a Rational (exact) or as a Number (inexact).
class Num {
    constructor(value) {
        this._value = value;
    }

    get _exact() { return this._value instanceof Rational; }

    _binop(that, exact, inexact) {
        if (this._exact && that._exact) {
            return exact(this._value, that._value);
        } else {
            const lhs = this._exact ? this._value.toNumber() : this._value;
            const rhs = that._exact ? that._value.toNumber() : that._value;
            return inexact(lhs, rhs);
        }
    }

    add(that) { return new Num(this._binop(that, (x, y) => x.add(y), (x, y) => x + y)); }
    sub(that) { return new Num(this._binop(that, (x, y) => x.sub(y), (x, y) => x - y)); }
    mul(that) { return new Num(this._binop(that, (x, y) => x.mul(y), (x, y) => x * y)); }
    div(that) { return new Num(this._binop(that, (x, y) => x.div(y), (x, y) => x / y)); }

    mod(that) {
        if (!this._exact || !that._exact) {
            throw new Error("Number: modulo arguments must be integrals");
        }
        return new Num(this._value.mod(that._value));
    }

    eq(that)  { return this._binop(that, (x, y) => x.eq(y),  (x, y) => x == y); }
    lt(that)  { return this._binop(that, (x, y) => x.lt(y),  (x, y) => x <  y); }
    gt(that)  { return this._binop(that, (x, y) => x.gt(y),  (x, y) => x >  y); }
    lte(that) { return this._binop(that, (x, y) => x.lte(y), (x, y) => x <= y); }
    gte(that) { return this._binop(that, (x, y) => x.gte(y), (x, y) => x >= y); }

    toNumber() { return this._exact ? this._value.toNumber() : this._value; }
    toBigInt() { return this._exact ? this._value.toBigInt() : null; }
    toString() { return this._value.toString(); }
}

class Position {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    right() { return new Position(this.x + 1, this.y); }
    down()  { return new Position(this.x, this.y + 1); }
    left()  { return new Position(this.x - 1, this.y); }
    up()    { return new Position(this.x, this.y - 1); }

    neighbors() { return [this.right(), this.down(), this.left(), this.up()]; }

    toString() { return `${this.x},${this.y}`; }
}

const Direction = {
    RIGHT: "RIGHT",
    DOWN:  "DOWN",
    LEFT:  "LEFT",
    UP:    "UP",
    turnLeft: (dir) => {
        switch (dir) {
            case Direction.RIGHT: return Direction.UP;
            case Direction.DOWN:  return Direction.RIGHT;
            case Direction.LEFT:  return Direction.DOWN;
            case Direction.UP:    return Direction.LEFT;
        }
    },
    turnRight: (dir) => {
        switch (dir) {
            case Direction.RIGHT: return Direction.DOWN;
            case Direction.DOWN:  return Direction.LEFT;
            case Direction.LEFT:  return Direction.UP;
            case Direction.UP:    return Direction.RIGHT;
        }
    },
};

const Pattern = {
    AAAA: "AAAA",
    AAAB: "AAAB",
    AABA: "AABA",
    AABB: "AABB",
    AABC: "AABC",
    ABAA: "ABAA",
    ABAB: "ABAB",
    ABAC: "ABAC",
    ABBA: "ABBA",
    ABBB: "ABBB",
    ABBC: "ABBC",
    ABCA: "ABCA",
    ABCB: "ABCB",
    ABCC: "ABCC",
    ABCD: "ABCD",
    parse: (a, x, y, z) => {
        if (x === a) {
            if (y === a) {
                if (z === a)      return Pattern.AAAA;
                else              return Pattern.AAAB;
            } else {
                if (z === a)      return Pattern.AABA;
                else if (z === y) return Pattern.AABB;
                else              return Pattern.AABC;
            }
        } else {
            if (y === a) {
                if (z === a)      return Pattern.ABAA;
                else if (z === x) return Pattern.ABAB;
                else              return Pattern.ABAC;
            } else if (y === x) {
                if (z === a)      return Pattern.ABBA;
                else if (z === x) return Pattern.ABBB;
                else              return Pattern.ABBC;
            } else {
                if (z === a)      return Pattern.ABCA;
                else if (z === x) return Pattern.ABCB;
                else if (z === y) return Pattern.ABCC;
                else              return Pattern.ABCD;
            }
        }
    },
};

class Source {
    hex(x, y) {
        const rgba = this.rgba(x, y);
        const hr = rgba[0].toString(16).padStart(2, "0");
        const hg = rgba[1].toString(16).padStart(2, "0");
        const hb = rgba[2].toString(16).padStart(2, "0");
        const ha = rgba[3].toString(16).padStart(2, "0");
        return `#${hr}${hg}${hb}${ha}`;
    }
}

// Firefox private windows (and possibly other browsers with strict security
// settings) will apply a random noise to the data returned from
// `canvas.getImageData()`, to prevent browser fingerprinting.  This prevents
// Turnstyle programs from parsing, because colors must be exactly the same.
// This is a workaround to find similar-enough colors.
class DenoiseSource extends Source {
    constructor(src) {
        super();
        this._limit = 32;
        this._width = src.width;
        this._height = src.height;
        this._rgba = new Array(src.width * src.height);

        const palette = {};
        const cache = {};

        const sse = (as, bs) => {
            let result = 0;
            for (let i = 0; i < as.length && i < bs.length; i++) {
                result += (as[i] - bs[i]) * (as[i] - bs[i]);
            }
            return result;
        }

        const similar = (rgba) => {
            for (const k of Object.keys(palette)) {
                if (sse(rgba, palette[k]) < this._limit) return palette[k];
            }
            return null;
        }

        for (let y = 0; y < src.height; y++) {
            for (let x = 0; x < src.width; x++) {
                const hex = src.hex(x, y);
                const rgba = src.rgba(x, y);
                let out;
                if (hex in cache) {
                    out = cache[hex];
                } else {
                    out = similar(rgba);
                    if (out) cache[hex] = out;
                    else cache[hex] = palette[hex] = out = rgba;
                }
                this._rgba[y * src.width + x] = out;
            }
        }
    }

    get width()  { return this._width;  }
    get height() { return this._height; }

    rgba(x, y) { return this._rgba[y * this.width + x]; }
}

class ImageLoadingError extends Error {
    constructor() {
        super("Could not load image file");
        this.name = "ImageLoadingError";
    }
}

class ImageDataSource extends Source {
    constructor(imageData) {
        super();
        this._imageData = imageData;
    }

    get width()  { return this._imageData.width;  }
    get height() { return this._imageData.height; }

    rgba(x, y) {
        const o = (y * this._imageData.width + x) * 4;
        return [
            this._imageData.data[o],
            this._imageData.data[o + 1],
            this._imageData.data[o + 2],
            this._imageData.data[o + 3],
        ];
    }

    static load(doc, src) {
        return new Promise((resolve, reject) => {
            const img = new Image();
            img.onload = () => {
                const canvas = doc.createElement("canvas");
                canvas.width = img.width;
                canvas.height = img.height;
                const ctx = canvas.getContext("2d");
                ctx.drawImage(img, 0, 0);
                const imageData =
                    ctx.getImageData(0, 0, canvas.width, canvas.height);
                resolve(new ImageDataSource(imageData));
            }
            // Sadly the `img.onerror` error contains no useful information.
            img.onerror = (err) => reject(new ImageLoadingError());
            img.src = src;
        });
    }
}

class ParserError extends Error {
    constructor(pos, dir, msg) {
        super(`${pos.toString()},${dir}: ${msg}`);
        this.name = "ParserError";
    }
}

class Parser {
    constructor(src, pos, dir) {
        this._src = src;
        this._pos = pos ? pos : new Position(0, Math.floor(src.height / 2));
        this._dir = dir ? dir : Direction.RIGHT;
    }

    _loc() { return {position: this._pos, direction: this._dir}; }

    _error(msg) { throw new ParserError(this._pos, this._dir, msg); }

    _pixelL() {
        switch (this._dir) {
            case Direction.RIGHT: return this._pos.up();
            case Direction.DOWN:  return this._pos.right();
            case Direction.LEFT:  return this._pos.down();
            case Direction.UP:    return this._pos.left();
        }
    }

    _pixelC() { return this._pos; }

    _pixelF() {
        switch (this._dir) {
            case Direction.RIGHT: return this._pos.right();
            case Direction.DOWN:  return this._pos.down();
            case Direction.LEFT:  return this._pos.left();
            case Direction.UP:    return this._pos.up();
        }
    }

    _pixelR() {
        switch (this._dir) {
            case Direction.RIGHT: return this._pos.down();
            case Direction.DOWN:  return this._pos.left();
            case Direction.LEFT:  return this._pos.up();
            case Direction.UP:    return this._pos.right();
        }
    }

    parse() {
        const pattern = Pattern.parse(
            this._colorL(), this._colorC(), this._colorF(), this._colorR(),
        );
        switch (pattern) {
            case Pattern.ABAC:
                return new AppExpr(this._parseL(), this._parseF(), this._loc());
            case Pattern.ABCA:
                return new AppExpr(this._parseL(), this._parseR(), this._loc());
            case Pattern.ABCC:
                return new AppExpr(this._parseF(), this._parseR(), this._loc());
            case Pattern.AABC:
                return new LamExpr(this._colorR(), this._parseL(), this._loc());
            case Pattern.ABBC:
                return new LamExpr(this._colorC(), this._parseF(), this._loc());
            case Pattern.ABCB:
                return new LamExpr(this._colorL(), this._parseR(), this._loc());
            case Pattern.AAAB:
                return new VarExpr(this._colorR(), this._loc());
            case Pattern.AABA:
                return new VarExpr(this._colorF(), this._loc());
            case Pattern.ABAA:
                return new VarExpr(this._colorC(), this._loc());
            case Pattern.ABBB:
                return new VarExpr(this._colorL(), this._loc());
            case Pattern.ABCD:
                const left  = this._area(this._pixelL());
                const front = this._area(this._pixelF());
                const right = this._area(this._pixelR());
                if (left === 1) {
                    const n = BigInt(front) ** BigInt(right);
                    return new LitExpr(new Num(new Rational(n), this._loc()));
                } else if (left === 2) {
                    if (Primitives[front] && Primitives[front][right]) {
                        const primitive = Primitives[front][right];
                        return new PrimExpr(primitive, [], this._loc());
                    }
                    this._error(`Unknown Prim(${front},${right})`);
                }
                this._error(`Unhandled symbol: ${left}`);
            case Pattern.AAAA:
                return new IdExpr(this._parseF(), this._loc());
            case Pattern.AABB:
                return new IdExpr(this._parseL(), this._loc());
            case Pattern.ABAB:
                return new IdExpr(this._parseR(), this._loc());
            case Pattern.ABBA:
                return new IdExpr(this._parseF(), this._loc());
            default:
                this._error(`Unhandled pattern: ${pattern.toString()}`);
        }
    }

    _parseL() {
        return () => new Parser(
            this._src,
            this._pixelL(),
            Direction.turnLeft(this._dir),
        ).parse();
    }

    _parseF() {
        return () => new Parser(this._src, this._pixelF(), this._dir).parse();
    }

    _parseR() {
        return () => new Parser(
            this._src,
            this._pixelR(),
            Direction.turnRight(this._dir),
        ).parse();
    }

    _color(pos) { return this._src.hex(pos.x, pos.y); }

    _colorL() { return this._color(this._pixelL()); }
    _colorC() { return this._color(this._pixelC()); }
    _colorF() { return this._color(this._pixelF()); }
    _colorR() { return this._color(this._pixelR()); }

    _has(pos) {
        return pos.x >= 0 && pos.x < this._src.width &&
            pos.y >= 0 && pos.y < this._src.height;
    }

    _area(pos) {
        const visited = new Set();
        const color = this._color(pos);
        let frontier = [pos];
        while (frontier.length > 0) {
            const next = [];
            for (const p of frontier) {
                if (!visited.has(p.toString())) {
                    for (const n of p.neighbors()) {
                        if (this._has(n) && !visited.has(n.toString()) &&
                                this._color(n) === color) {
                            next.push(n);
                        }
                    }
                }
                visited.add(p.toString());
            }
            frontier = next;
        }
        return visited.size;
    }
}

class Expr {
    constructor(loc) {
        this._loc = loc;
    }

    get location() { return this._loc; }

    async whnf(ctx) {
        if (!this._whnf) {
            await ctx.onWhnf(this);
            this._whnf = this;
        }
        return this._whnf;
    }

    async apply(ctx, arg) {
        return new AppExpr(() => this, () => arg, this._loc);
    }

    freeVars() { return new Set(); }
    allVars()  { return new Set(); }

    subst(x, s) { return this; }

    value() { return null; }
}

class AppExpr extends Expr {
    constructor(lhsf, rhsf, loc) {
        super(loc);
        this._lhsf = lhsf;
        this._rhsf = rhsf;
    }

    get lhs() {
        if (!this._lhs) this._lhs = this._lhsf();
        return this._lhs;
    }

    get rhs() {
        if (!this._rhs) this._rhs = this._rhsf();
        return this._rhs;
    }

    toString() {
        const lhs = this.lhs.toString();
        const rhs = this.rhs.toString();
        return `(${lhs} ${rhs})`;
    }

    async whnf(ctx) {
        if (!this._whnf) {
            await ctx.onWhnf(this);
            const lhs = await this.lhs.whnf(ctx);
            this._whnf = await lhs.apply(ctx, this.rhs);
        }
        return this._whnf;
    }

    freeVars() { return this.lhs.freeVars().union(this.rhs.freeVars()); }
    allVars()  { return this.lhs.allVars().union(this.rhs.allVars());   }

    subst(x, s) {
        return new AppExpr(
            () => this.lhs.subst(x, s),
            () => this.rhs.subst(x, s),
            this.location,
        );
    }
}

class LamExpr extends Expr {
    constructor(variable, bodyf, loc) {
        super(loc);
        this._variable = variable;
        this._bodyf = bodyf;
    }

    get body() {
        if (!this._body) this._body = this._bodyf()
        return this._body;
    }

    toString() {
        const body = this.body.toString();
        return `(\\${this._variable} -> ${body})`;
    }

    async apply(ctx, arg) {
        return this.body.subst(this._variable, arg).whnf(ctx);
    }

    freeVars() {
        const fvs = this.body.freeVars();
        fvs.delete(this._variable);
        return fvs;
    }

    allVars() {
        const avs = this.body.allVars();
        avs.add(this._variable);
        return avs;
    }

    subst(x, s) {
        // This lambda binds more tightly, no need to change
        if (x === this._variable) return this;

        const fvs = s.freeVars();
        if (fvs.has(this._variable)) {
            // Avoid capturing `this._variable`
            const body = this.body;
            const avs = fvs.union(body.allVars());
            let fresh = 0;
            while (avs.has(`fresh_${fresh}`)) fresh++
            const variable = `fresh_${fresh}`;
            return new LamExpr(
                variable,
                () => body.
                    subst(this._variable, new VarExpr(variable)).
                    subst(x, s),
                this.location,
            )
        }

        // Continue substitution
        return new LamExpr(
            this._variable,
            () => this.body.subst(x, s),
            this.location,
        );
    }
}

class VarExpr extends Expr {
    constructor(variable, loc) {
        super(loc);
        this._variable = variable;
    }

    toString() { return this._variable; }

    freeVars() { return new Set([this._variable]); }
    allVars()  { return new Set([this._variable]); }

    subst(x, e) {
        if (x === this._variable) return e;
        return this;
    }
}

class PrimExpr extends Expr {
    constructor(primitive, args, loc) {
        super(loc);
        this._primitive = primitive;
        this._args = args ? args : [];
    }

    toString() {
        if (this._args.length === 0) return this._primitive.name;
        return this._primitive.name + "(" +
            this._args.map((a) => a.toString()).join(", ") + ")";
    }

    async apply(ctx, arg) {
        const args = [...this._args, arg]
        if (args.length === this._primitive.arity) {
            return this._primitive.implementation(ctx, args);
        }
        return new PrimExpr(this._primitive, args, this.location);
    }
}

class LitExpr extends Expr {
    constructor(value, loc) {
        super(loc);
        this._value = value;
    }

    toString() { return this._value.toString(); }

    value() { return this._value; }
}

class IdExpr extends Expr {
    constructor(exprf, loc) {
        super(loc);
        this._exprf = exprf;
    }

    get expr() {
        if (!this._expr) this._expr = this._exprf();
        return this._expr;
    }

    toString() { return this.expr.toString(); }

    async whnf(ctx) {
        if (!this._whnf) {
            await ctx.onWhnf(this);
            this._whnf = await this.expr.whnf(ctx);
        }
        return this._whnf;
    }

    freeVars() { return this.expr.freeVars(); }
    allVars()  { return this.expr.allVars();  }

    subst(x, s) {
        return new IdExpr(() => this.expr.subst(x, s), this.location);
    }
}

const Primitives = {
    1: {
        1: {
            name: "in_num",
            arity: 2,
            implementation: async (ctx, args) => {
                const input = await ctx.inputNumber();
                const lit = new LitExpr(new Num(new Rational(BigInt(input))));
                const applied = await args[0].apply(ctx, lit);
                return applied.whnf(ctx);
            },
        },
        2: {
            name: "in_char",
            arity: 2,
            implementation: async (ctx, args) => {
                const input = await ctx.inputCharacter();
                const codePoint = input.codePointAt(0);
                const lit = new LitExpr(new Num(new Rational(BigInt(codePoint))));
                const applied = await args[0].apply(ctx, lit);
                return applied.whnf(ctx);
            },
        },
    },
    2: {
        1: {
            name: "out_num",
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                await ctx.outputNumber(lhs.value().toString());
                return args[1].whnf(ctx);
            },
        },
        2: {
            name: "out_char",
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                await ctx.outputCharacter(String.fromCodePoint(lhs.value()));
                return args[1].whnf(ctx);
            },
        },
    },
    3: (() => {
        const binop = (name, f) => ({
            name,
            arity: 2,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                const rhs = await args[1].whnf(ctx);
                return new LitExpr(f(lhs.value(), rhs.value()));
            },
        });
        return {
            1: binop("num_add", (lhs, rhs) => lhs.add(rhs)),
            2: binop("num_sub", (lhs, rhs) => lhs.sub(rhs)),
            3: binop("num_mul", (lhs, rhs) => lhs.mul(rhs)),
            4: binop("num_div", (lhs, rhs) => lhs.div(rhs)),
            5: binop("num_mod", (lhs, rhs) => lhs.mod(rhs)),
        };
    })(),
    4: (() => {
        const cmp = (name, p) => ({
            name,
            arity: 4,
            implementation: async (ctx, args) => {
                const lhs = await args[0].whnf(ctx);
                const rhs = await args[1].whnf(ctx);
                return args[p(lhs.value(), rhs.value()) ? 2 : 3].whnf(ctx);
            },
        });
        return {
            1: cmp("cmp_eq",  (lhs, rhs) => lhs.eq(rhs)),
            2: cmp("cmp_lt",  (lhs, rhs) => lhs.lt(rhs)),
            3: cmp("cmp_gt",  (lhs, rhs) => lhs.gt(rhs)),
            4: cmp("cmp_lte", (lhs, rhs) => lhs.lte(rhs)),
            5: cmp("cmp_gte", (lhs, rhs) => lhs.gte(rhs)),
        };
    })(),
    5: {
        1: {
            name: "inexact_sqrt",
            arity: 1,
            implementation: async (ctx, args) => {
                const x = await args[0].whnf(ctx);
                return new LitExpr(new Num(Math.sqrt(x.value().toNumber())));
            },
        },
    },
};

class AnnotatedView {
    constructor(doc, src) {
        this._doc = doc;
        this._ns = "http://www.w3.org/2000/svg";
        this._src = src;
        this._factor = 20;
        this._padding = 0.1;
        this._focus = null;
        this._svg = doc.createElementNS(this._ns, "svg");
        const width = src.width + this._padding * 2;
        const height = src.height + this._padding * 2;
        this._svg.setAttribute("width", this._factor * width);
        this._svg.setAttribute("height", this._factor * height);
        this._svg.setAttribute(
            "viewBox",
            `-${this._padding} -${this._padding} ${width} ${height}`,
        );

        for (let y = 0; y < this._src.height; y++) {
            for (let x = 0; x < this._src.width; x++) {
                const alpha = this._src.rgba(x, y)[3];
                if (alpha > 0) {
                    const rect = this._doc.createElementNS(this._ns, "rect");
                    rect.setAttribute("x", x);
                    rect.setAttribute("y", y);
                    rect.setAttribute("width", "1");
                    rect.setAttribute("height", "1");
                    rect.setAttribute("fill", this._src.hex(x, y));
                    rect.setAttribute("stroke", "none");
                    this._svg.appendChild(rect);
                }
            }
        }
    }

    get element() { return this._svg; }

    focus(pos, dir) {
        if (this._focus) this._svg.removeChild(this._focus);

        const points = [
            [0, -1], [1, -1], [1, 0], [2, 0],
            [2, 1], [1, 1], [1, 2], [0, 2], [0, -1],
        ].map(([x, y]) => {
            switch (dir) {
                case Direction.RIGHT: return [x, y];
                case Direction.DOWN:  return [y, x];
                case Direction.LEFT:  return [1 - x, y];
                case Direction.UP:    return [1 - y, 1 - x];
            }
        }).map((xy) => xy.join(",")).join(" ");

        const rect = this._doc.createElementNS(this._ns, "polyline");
        rect.setAttribute("fill", "none");
        rect.setAttribute("stroke", "black");
        rect.setAttribute("stroke-width", "0.1");
        rect.setAttribute("transform", `translate(${pos.x} ${pos.y})`);
        rect.setAttribute("points", points);
        this._focus = rect;
        this._svg.appendChild(this._focus);
    }
}

class Terminal {
    constructor(doc) {
        this._queue = [];
        this._consumers = [];

        this._code = doc.createElement("code");
        this._code.setAttribute("class", "terminal");
        this._pre = doc.createElement("pre");
        this._code.appendChild(this._pre);
        this._output = doc.createElement("span");
        this._pre.appendChild(this._output);
        this._input = doc.createElement("textarea");
        this._input.oninput = (event) => {
            const str = this._input.value;
            this._output.innerText += str;
            for (const c of str) this._push(c);
            this._input.value = "";
        };
        this._pre.appendChild(this._input);
        this._code.onclick = (event) => this._input.focus();
        this._cursor = doc.createElement("span");
        this._cursor.setAttribute("class", "cursor");
        this._pre.appendChild(this._cursor);
    }

    _push(c) {
        if (this._consumers.length > 0) {
            const consumer = this._consumers.shift();
            consumer(c);
        } else {
            this._queue.push(c);
        }
    }

    _pop() {
        return new Promise((resolve, reject) => {
            if (this._queue.length > 0) {
                const x = this._queue.shift();
                resolve(x);
            } else {
                this._consumers.push(resolve);
            }
        });
    }

    print(str) {
        this._output.innerText += str;
        this._code.scrollTo(0, this._code.scrollHeight);
    }

    async inputNumber() {
        this._input.focus();
        let str = "";
        let c = await this._pop();
        while (c >= "0" && c <= "9") {
            str += c;
            c = await this._pop();
        }
        if (str === "") throw new EvalError(`expected number, got: ${c}`);
        const n = Number(str);
        if (isNaN(n)) throw new EvalError(`not a number: ${str}`);
        if (!/\s/.test(c)) throw new EvalError(`expected space, got: ${c}`);
        return Number(n);
    }

    async inputCharacter() {
        this._input.focus();
        return this._pop();
    }

    get element() { return this._code; }
}

class Interpreter {
    constructor(doc, src) {
        this.delay = 50;
        this._doc = doc;
        this._src = src;
        this._div = doc.createElement("div");
        this._div.setAttribute("class", "interpreter");
        this._terminal = new Terminal(this._doc);
        this._div.appendChild(this._terminal.element);
        this._paused = null;
        this._unpause = () => {};
    }

    _output (line) { this._terminal.print(line + "\n") };

    get element() { return this._div; }

    pause() {
        this._paused = new Promise((resolve, reject) => {
            this._unpause = () => {
                resolve();
                this._paused = null;
            };
        });
    }

    unpause() { this._unpause(); }

    async load() {
        try {
            const noisy = await ImageDataSource.load(this._doc, this._src);
            const source = new DenoiseSource(noisy);
            this._view = new AnnotatedView(this._doc, source);
            this._div.insertBefore(this._view.element, this._div.firstChild);

            this._view.element.onclick = (event) => {
                event.preventDefault();
                if (!this._paused) {
                    this.pause();
                } else {
                    this.unpause();
                }
            };
            this._output("Starting interpreter...");
            this._parser = new Parser(source);
        } catch (e) {
            this._output(`Interpreter failed to load: ${e}`);
        }
    }

    async run() {
        const evalCtx = {
            inputNumber: async () => this._terminal.inputNumber(),
            inputCharacter: async () => this._terminal.inputCharacter(),
            outputNumber: (num) => this._output(num.toString()),
            outputCharacter: (char) => this._terminal.print(char),
            onWhnf: async (expr) => {
                if (this._paused) await this._paused;
                if (expr.location) {
                    this._view.focus(expr.location.position, expr.location.direction);
                }
                await new Promise(r => setTimeout(r, this.delay));
            }
        }

        try {
            const whnf = await this._parser.parse().whnf(evalCtx);
            if (whnf.value() === null) {
                this._output("Interpreter exited with expression:");
                this._output(whnf.toString());
            } else {
                this._output(`Interpreter exited with code ${whnf.value()}`);
            }
        } catch(e) {
            this._output(`Interpreter crashed: ${e}`);
        }
    }
}
