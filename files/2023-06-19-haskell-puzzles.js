const tryHaskell = "https://tryhaskell.org";
// const tryHaskell = "https://www.haskellmooc.co.uk";

class JSONPDispatcher {
    constructor() {
        this.listeners = {};
        this.versions = {};
        this.version = 0;
    }

    listen(bucket, listener) {
        this.listeners[bucket] = listener;
    }

    dispatch(bucket, version, data) {
        if (version >= this.versions[bucket]) {
            this.listeners[bucket](data);
        }
    }

    request(bucket, url) {
        const version = this.version;
        this.version += 1;
        const functionName = "dispatch_" + bucket + "_" + version;
        const definition = document.createElement("script");
        definition.type = "text/JavaScript";
        definition.innerText = "function " + functionName + "(data) { " +
                "dispatch(\"" + bucket + "\", " + version + ", data) }";
        document.body.appendChild(definition);

        const script = document.createElement("script");
        script.type = "text/JavaScript";
        script.src = url + "&callback=" + functionName;
        document.body.appendChild(script);
        this.versions[bucket] = version;
    }
}

const dispatcher = new JSONPDispatcher();
function dispatch(bucket, version, data) {
    dispatcher.dispatch(bucket, version, data);
}

class Token {
    static textFont = "15px monospace";
    static hintFont = "8px monospace";
    static background = "#438";

    constructor(ctx, text, hint) {
        this.ctx = ctx;
        if (hint) {
            this.hint = hint;
        } else {
            this.hint = "";
        }

        this.ctx.font = Token.textFont;
        const textWidth = ctx.measureText(text).width;
        let hintWidth = 0;
        if (hint) {
            this.ctx.font = Token.hintFont;
            hintWidth = ctx.measureText(hint).width;
        }
        this.w = 10 + Math.max(textWidth, hintWidth) * 1.1;
        this.h = 42;
        this.text = text;
    }

    draw() {
        this.ctx.fillStyle = Token.background;
        this.ctx.fillRect(this.x, this.y, this.w, this.h);
        this.ctx.textAlign = "center";
        this.ctx.textBaseline = "top";
        this.ctx.fillStyle = "#fec";
        this.ctx.font = Token.textFont;
        this.ctx.fillText(this.text, this.x + this.w / 2, this.y + 6);
        if (this.hint || true) {
            this.ctx.fillStyle = "#aaa";
            this.ctx.font = Token.hintFont;
            this.ctx.fillText(this.hint, this.x + this.w / 2, this.y + 27);
        }
    }

    center(x, y) {
        this.x = x - this.w / 2;
        this.y = y - this.h / 2;
    }

    contains(x, y) {
        return x >= this.x && x < this.x + this.w &&
               y >= this.y && y < this.y + this.h;
    }

    verticalOverlap(other) {
        return this.y <= other.y + other.h && this.y + this.h >= other.y;
    }
}

class Sandbox {
    static width = 300;
    static height = 200;

    constructor(canvas, spec) {
        this.canvas = canvas;
        this.ctx = canvas.getContext("2d");
        this.tokens = [];
        for (const t of spec) {
            const token = new Token(this.ctx, t.text, t.hint);
            token.center(t.x, t.y);
            this.tokens.push(token);
        }
        this.selected = -1;
        this.text = "";
        this.textChangeListeners = [];

        canvas.addEventListener("mousedown", e => {
            let pos = this.position(canvas, e);
            this.select(pos.x, pos.y);
        });

        canvas.addEventListener("mousemove", e => {
            let pos = this.position(canvas, e);
            this.moveSelection(pos.x, pos.y);
        });

        canvas.addEventListener("mouseup", e => {
            this.unselect();
        });

        canvas.addEventListener("touchstart", e => {
            let touched = false;
            for (let touch of e.changedTouches) {
                let pos = this.position(canvas, touch);
                touched = touched || this.select(pos.x, pos.y);
            }
            if (touched) {
                e.preventDefault();
            }
        });

        canvas.addEventListener("touchmove", e => {
            let moved = false;
            for (let touch of e.changedTouches) {
                let pos = this.position(canvas, touch);
                moved = moved || this.moveSelection(pos.x, pos.y);
            }
            if (moved) {
                e.preventDefault();
            }
        });

        canvas.addEventListener("touchend", e => {
            if (this.unselect()) {
                e.preventDefault();
            }
        });

        this.update();
    }

    addTextChangeListener(f) {
        this.textChangeListeners.push(f);
    }

    select(x, y) {
        this.tokens.forEach((e, i) => {
            if (e.contains(x, y)) {
                this.selected = i;
            }
        });
        this.update();
        return this.selected >= 0;
    }

    moveSelection(x, y) {
        if (this.selected < 0 ||
                x < 0 || x >= Sandbox.width || y < 0 || y >= Sandbox.height) {
            return false;
        }
        this.tokens[this.selected].center(x, y);
        this.update();
        return true;
    }

    unselect() {
        if (this.selected >= 0) {
            this.selected = -1;
            this.update();
            return true;
        }
        return false;
    }

    position(canvas, thing) {
        let bounding = this.canvas.getBoundingClientRect();
        const scale = bounding.width / Sandbox.width;
        return {
            x: (thing.clientX - bounding.left) / scale,
            y: (thing.clientY - bounding.top) / scale
        };
    }

    lines() {
        const lines = [];
        let remaining = [...this.tokens];
        while (remaining.length > 0) {
            // Select topmost remaining item
            let topmostIdx = -1;
            let topmost = null;
            remaining.forEach((item, i) => {
                if (topmostIdx < 0 || item.y < topmost.y) {
                    topmost = item;
                    topmostIdx = i;
                }
            })

            // Add overlapping tokens to line.
            let line = [remaining[topmostIdx]];
            remaining.splice(topmostIdx, 1);
            line = line.concat(remaining.filter(i => {
                return i.verticalOverlap(topmost);
            }));
            remaining = remaining.filter(i => {
                return !i.verticalOverlap(topmost);
            });
            line.sort((a, b) => a.x - b.x);
            lines.push(line);
        }
        return lines;
    }

    update() {
        const text = this.lines().flatMap(l => l.map(i => i.text)).join(" ");
        if (text != this.text) {
            this.text = text;
            for (const f of this.textChangeListeners) {
                f(text);
            }
        }
        this.draw();
    }

    draw() {
        if (this.selected >= 0) {
            this.canvas.classList.add("moving");
        } else {
            this.canvas.classList.remove("moving");
        }

        const rect = this.canvas.getBoundingClientRect();
        this.canvas.width = rect.width * window.devicePixelRatio;
        this.canvas.height = rect.width *
                (Sandbox.height / Sandbox.width) * window.devicePixelRatio;
        const scale = (rect.width / Sandbox.width) * window.devicePixelRatio;
        this.ctx.scale(scale, scale);

        this.ctx.fillStyle = "#324";
        this.ctx.fillRect(0, 0, Sandbox.width, Sandbox.height);

        for (const line of this.lines()) {
            const avgY = line.reduce((acc, item) => {
                return acc + item.y + item.h / 2;
            }, 0) / line.length;
            this.ctx.lineWidth = 2;
            this.ctx.strokeStyle = Token.background;
            this.ctx.beginPath();
            this.ctx.moveTo(0, avgY);
            this.ctx.lineTo(Sandbox.width, avgY);
            this.ctx.stroke();
        }
        this.tokens.forEach(e => {
            e.draw(this.ctx);
        })
    }
}

const css = document.createElement("style");
css.innerText = "\
.puzzle {\
    font-family: monospace;\
    font-size: 20px;\
    color: #eee;\
    background-color: #222;\
}\
.puzzle .goal, .puzzle .code, .puzzle .result {\
    padding: 12px;\
}\
.puzzle .goal {\
    text-align: center;\
}\
.puzzle .result {\
    overflow-x: scroll;\
    margin: 0px;\
}\
.puzzle .correct {\
    background-color: #282;\
}\
.puzzle .error, .puzzle .incorrect {\
    background-color: #822;\
}\
.puzzle canvas {\
    cursor: grab;\
}\
.puzzle canvas.moving {\
    cursor: grabbing;\
}\
";
document.body.appendChild(css);

for (const element of document.getElementsByClassName("puzzle")) {
    const id = element.getAttribute("id");
    const json = element.getAttribute("data-puzzle");
    const spec = JSON.parse(json);

    const goal = document.createElement("div");
    goal.classList.add("goal");
    goal.innerText = "goal: " + spec.goal;
    element.appendChild(goal);

    const canvas = document.createElement("canvas");
    canvas.style = "width: 100%;"
    element.appendChild(canvas);
    const sandbox = new Sandbox(canvas, spec.tokens);

    const code = document.createElement("div");
    code.classList.add("code");
    code.innerText = "> " + sandbox.text;
    element.appendChild(code);

    const result = document.createElement("pre");
    result.classList.add("result");
    element.appendChild(result);

    dispatcher.listen(id, (data) => {
        result.classList.remove("error");
        result.classList.remove("correct");
        result.classList.remove("incorrect");
        if (data.error) {
            let error = data.error;
            if (error.startsWith("<hint>")) {
                error = error.slice("<hint>".length);
            }
            result.innerText = error;
            result.classList.add("error");
        } else if (data.success) {
            result.innerText = data.success.value;
            if (data.success.value == spec.goal) {
                result.classList.add("correct");
            } else {
                result.classList.add("incorrect");
            }
        }
    });
    sandbox.addTextChangeListener(text => {
        code.innerText = "> " + text;
        const url = tryHaskell + "/eval?" + "exp=" + encodeURIComponent(text);
        dispatcher.request(id, url);
        result.innerText = "Evaluating...";
    });
}
