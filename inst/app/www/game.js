/*
 * Maze Muncher: client-side game engine.
 *
 * R/Shiny sends one config per level (maze rows, speeds, timings). This file
 * runs the real-time loop on a <canvas> and reports `level_cleared` and
 * `game_over` back to the server through Shiny.setInputValue().
 */
(function () {
  'use strict';

  var DIRS = {
    up: { x: 0, y: -1 },
    left: { x: -1, y: 0 },
    down: { x: 0, y: 1 },
    right: { x: 1, y: 0 }
  };
  var DIR_ORDER = ['up', 'left', 'down', 'right'];
  var OPPOSITE = { up: 'down', down: 'up', left: 'right', right: 'left' };
  var KEYMAP = {
    ArrowUp: 'up', ArrowDown: 'down', ArrowLeft: 'left', ArrowRight: 'right',
    KeyW: 'up', KeyS: 'down', KeyA: 'left', KeyD: 'right'
  };
  var STEP = 1 / 120;
  var COLORS = {
    bg: '#140b24',
    wallFill: '#16303f',
    wallEdge: '#2de2c4',
    wallFlash: '#ffffff',
    gate: '#ff5fa2',
    seed: '#ffd9a8',
    star: '#ffe66d',
    player: '#7dfc9a',
    playerDark: '#2f9e57',
    leaf: '#c6ff6b',
    frightened: '#c9d2ff',
    frightFlash: '#ffffff',
    gem: '#b98bff',
    text: '#ece8ff'
  };
  var SENTRY_TYPES = [
    { name: 'Hunter', color: '#ff4fa3', corner: 'tr' },
    { name: 'Cutter', color: '#ff9f43', corner: 'tl' },
    { name: 'Pincer', color: '#38d1ff', corner: 'br' },
    { name: 'Drifter', color: '#b6f23f', corner: 'bl' }
  ];
  var POINTS = { seed: 10, star: 50, sentry: 200 };
  var EXTRA_LIFE_AT = 10000;
  var DEFAULT_SIZE = { w: 23, h: 21 };

  var games = {};
  var activeGame = null;

  function toArray(x) {
    if (x === undefined || x === null) return [];
    return Array.isArray(x) ? x : [x];
  }

  /* ------------------------------------------------------------------ */
  /* Level model                                                         */
  /* ------------------------------------------------------------------ */

  function Level(config) {
    var rows = toArray(config.rows);
    this.h = rows.length;
    this.w = rows[0].length;
    this.grid = rows.map(function (r) { return r.split(''); });
    this.pellets = new Uint8Array(this.w * this.h);
    this.remaining = 0;
    this.den = [];
    this.open = [];
    for (var y = 0; y < this.h; y++) {
      for (var x = 0; x < this.w; x++) {
        var ch = this.grid[y][x];
        if (ch === '.') { this.pellets[y * this.w + x] = 1; this.remaining++; }
        else if (ch === '*') { this.pellets[y * this.w + x] = 2; this.remaining++; }
        else if (ch === 'P') this.player = { x: x, y: y };
        else if (ch === 'S') this.den.push({ x: x, y: y });
        else if (ch === '=') this.gate = { x: x, y: y };
        if (ch !== '#' && ch !== 'S' && ch !== '=') this.open.push({ x: x, y: y });
      }
    }
    this.total = this.remaining;
    this.den.sort(function (a, b) { return a.x - b.x; });
    this.denCenter = this.den[Math.floor(this.den.length / 2)];
    this.exit = { x: this.gate.x, y: this.gate.y - 1 };
    this.distExit = this.bfs(this.exit);
    this.distDen = this.bfs(this.denCenter);
  }

  Level.prototype.wrapX = function (x) {
    return ((x % this.w) + this.w) % this.w;
  };

  Level.prototype.tile = function (x, y) {
    if (y < 0 || y >= this.h) return '#';
    return this.grid[y][this.wrapX(x)];
  };

  Level.prototype.playerCan = function (x, y) {
    var ch = this.tile(x, y);
    return ch !== '#' && ch !== '=' && ch !== 'S';
  };

  Level.prototype.sentryCan = function (x, y, home) {
    var ch = this.tile(x, y);
    if (ch === '#') return false;
    if (ch === '=' || ch === 'S') return !!home;
    return true;
  };

  // Distances to `target` over every tile sentries may use (den included).
  Level.prototype.bfs = function (target) {
    var dist = new Int16Array(this.w * this.h).fill(-1);
    var queue = [target];
    dist[target.y * this.w + target.x] = 0;
    for (var i = 0; i < queue.length; i++) {
      var cur = queue[i];
      for (var k = 0; k < 4; k++) {
        var d = DIRS[DIR_ORDER[k]];
        var nx = this.wrapX(cur.x + d.x), ny = cur.y + d.y;
        if (!this.sentryCan(nx, ny, true)) continue;
        var idx = ny * this.w + nx;
        if (dist[idx] !== -1) continue;
        dist[idx] = dist[cur.y * this.w + cur.x] + 1;
        queue.push({ x: nx, y: ny });
      }
    }
    return dist;
  };

  /* ------------------------------------------------------------------ */
  /* Game                                                                */
  /* ------------------------------------------------------------------ */

  function Game(canvas) {
    this.canvas = canvas;
    this.ctx = canvas.getContext('2d');
    this.phase = 'idle';
    this.level = null;
    this.score = 0;
    this.lives = 0;
    this.anim = 0;
    this.acc = 0;
    this.last = null;
    this.paused = false;
    this.popups = [];
    this.particles = [];
    this.hud = {};
    this.resize();
    var self = this;
    window.addEventListener('resize', function () { self.resize(); });
    this.bindTouch();
    requestAnimationFrame(function (ts) { self.frame(ts); });
  }

  /* ---------- lifecycle (driven by the server) ---------- */

  Game.prototype.newGame = function (msg) {
    this.inputId = msg.input;
    this.hud = msg.hud || {};
    this.score = 0;
    this.lives = msg.lives || 3;
    this.extraLifeGiven = false;
    this.paused = false;
    activeGame = this;
    this.loadLevel(msg.config);
  };

  Game.prototype.loadLevel = function (config) {
    this.config = config;
    this.levelNumber = config.level;
    this.level = new Level(config);
    this.eaten = 0;
    this.gemThresholds = [Math.floor(this.level.total * 0.3), Math.floor(this.level.total * 0.7)];
    this.gem = null;
    this.popups = [];
    this.particles = [];
    this.resize();
    this.resetActors();
    this.setPhase('ready', 2.0);
    this.updateHud();
  };

  Game.prototype.stop = function () {
    this.setPhase('idle', 0);
    this.level = null;
    this.resize();
  };

  Game.prototype.resetActors = function () {
    var L = this.level, cfg = this.config;
    this.player = {
      x: L.player.x, y: L.player.y, dir: null, want: null, face: 'left',
      progress: 0, moving: false
    };
    var n = Math.max(1, Math.min(4, cfg.sentries || 4));
    var release = toArray(cfg.release);
    var denSlots = [L.denCenter, L.den[0], L.den[L.den.length - 1]];
    this.sentries = [];
    for (var i = 0; i < n; i++) {
      var type = SENTRY_TYPES[i];
      var start = i === 0 ? L.exit : denSlots[(i - 1) % denSlots.length];
      this.sentries.push({
        kind: i,
        color: type.color,
        corner: this.cornerTarget(type.corner),
        x: start.x, y: start.y, dir: 'left', progress: 0, moving: false,
        state: i === 0 ? 'active' : 'den',
        releaseAt: release[i] !== undefined ? release[i] : 2 * i,
        frightened: false,
        reverse: false,
        wander: null,
        wanderUntil: 0
      });
    }
    this.levelTime = 0;
    this.modeIndex = 0;
    this.modeTimer = 0;
    this.scatter = toArray(cfg.schedule).length > 0;
    this.frightTimer = 0;
    this.chain = 0;
    this.freeze = 0;
  };

  Game.prototype.cornerTarget = function (corner) {
    var L = this.level;
    return {
      x: corner.charAt(1) === 'r' ? L.w + 1 : -2,
      y: corner.charAt(0) === 'b' ? L.h + 1 : -2
    };
  };

  Game.prototype.setPhase = function (phase, duration) {
    this.phase = phase;
    this.phaseTimer = duration || 0;
  };

  Game.prototype.emit = function (type) {
    if (!this.inputId || !window.Shiny || !window.Shiny.setInputValue) return;
    window.Shiny.setInputValue(this.inputId, {
      type: type,
      level: this.levelNumber,
      score: this.score,
      lives: this.lives,
      nonce: Date.now() + Math.random()
    }, { priority: 'event' });
  };

  Game.prototype.updateHud = function () {
    var set = function (id, text) {
      var el = id && document.getElementById(id);
      if (el) el.textContent = text;
    };
    set(this.hud.score, this.score.toLocaleString());
    set(this.hud.level, this.levelNumber ? String(this.levelNumber) : '-');
    set(this.hud.lives, this.lives > 0 ? '\u2665'.repeat(this.lives) : '-');
    set(this.hud.maze, this.config ? this.config.name : '');
  };

  Game.prototype.addScore = function (pts, x, y) {
    this.score += pts;
    if (x !== undefined) this.popups.push({ x: x, y: y, text: String(pts), t: 0 });
    if (!this.extraLifeGiven && this.score >= EXTRA_LIFE_AT) {
      this.extraLifeGiven = true;
      this.lives++;
      this.popups.push({ x: this.player.x, y: this.player.y - 1, text: '1UP', t: 0 });
    }
    this.updateHud();
  };

  /* ---------- input ---------- */

  Game.prototype.setWant = function (dir) {
    if (!this.player) return;
    this.player.want = dir;
  };

  Game.prototype.togglePause = function () {
    if (this.phase === 'play' || this.phase === 'ready') this.paused = !this.paused;
  };

  Game.prototype.isLive = function () {
    return this.phase !== 'idle' && this.phase !== 'over';
  };

  Game.prototype.bindTouch = function () {
    var self = this, start = null;
    this.canvas.addEventListener('touchstart', function (e) {
      var t = e.changedTouches[0];
      start = { x: t.clientX, y: t.clientY };
    }, { passive: true });
    this.canvas.addEventListener('touchmove', function (e) {
      if (self.isLive()) e.preventDefault();
    }, { passive: false });
    this.canvas.addEventListener('touchend', function (e) {
      if (!start) return;
      var t = e.changedTouches[0];
      var dx = t.clientX - start.x, dy = t.clientY - start.y;
      start = null;
      if (Math.max(Math.abs(dx), Math.abs(dy)) < 20) return;
      if (Math.abs(dx) > Math.abs(dy)) self.setWant(dx > 0 ? 'right' : 'left');
      else self.setWant(dy > 0 ? 'down' : 'up');
    }, { passive: true });
  };

  /* ---------- main loop ---------- */

  Game.prototype.frame = function (ts) {
    var self = this;
    if (this.last === null) this.last = ts;
    var dt = Math.min((ts - this.last) / 1000, 0.1);
    this.last = ts;
    if (!this.paused) {
      this.anim += dt;
      this.acc += dt;
      while (this.acc >= STEP) {
        this.update(STEP);
        this.acc -= STEP;
      }
    }
    this.render();
    requestAnimationFrame(function (t) { self.frame(t); });
  };

  Game.prototype.update = function (dt) {
    var i;
    for (i = this.popups.length - 1; i >= 0; i--) {
      this.popups[i].t += dt;
      if (this.popups[i].t > 1.2) this.popups.splice(i, 1);
    }
    for (i = this.particles.length - 1; i >= 0; i--) {
      var p = this.particles[i];
      p.t += dt; p.x += p.vx * dt; p.y += p.vy * dt;
      if (p.t > p.life) this.particles.splice(i, 1);
    }

    if (this.phaseTimer > 0) this.phaseTimer -= dt;

    switch (this.phase) {
      case 'ready':
        if (this.phaseTimer <= 0) this.setPhase('play', 0);
        break;
      case 'play':
        this.updatePlay(dt);
        break;
      case 'dying':
        if (this.phaseTimer <= 0) {
          this.lives--;
          this.updateHud();
          if (this.lives <= 0) {
            this.setPhase('over', 0);
            this.emit('game_over');
          } else {
            this.resetActors();
            this.setPhase('ready', 1.5);
          }
        }
        break;
      case 'cleared':
        if (this.phaseTimer <= 0) {
          this.setPhase('waiting', 0);
          this.emit('level_cleared');
        }
        break;
      default:
        break;
    }
  };

  Game.prototype.updatePlay = function (dt) {
    if (this.freeze > 0) { this.freeze -= dt; return; }
    var cfg = this.config;
    this.levelTime += dt;

    // Scatter/chase schedule pauses while sentries are frightened.
    if (this.frightTimer > 0) {
      this.frightTimer -= dt;
      if (this.frightTimer <= 0) {
        this.sentries.forEach(function (s) { s.frightened = false; });
      }
    } else {
      var schedule = toArray(cfg.schedule);
      if (this.modeIndex < schedule.length) {
        this.modeTimer += dt;
        if (this.modeTimer >= schedule[this.modeIndex]) {
          this.modeTimer = 0;
          this.modeIndex++;
          this.scatter = this.modeIndex < schedule.length && this.modeIndex % 2 === 0;
          this.sentries.forEach(function (s) { if (s.state === 'active') s.reverse = true; });
        }
      }
    }

    // Gem pickup lifetime.
    if (this.gem && this.levelTime > this.gem.until) this.gem = null;

    this.movePlayer(dt);
    if (this.phase !== 'play') return;
    if (this.checkCollisions()) return;

    var self = this;
    this.sentries.forEach(function (s) { self.moveSentry(s, dt); });
    this.checkCollisions();
  };

  /* ---------- movement ---------- */

  // Moves `m` `dist` tiles along its direction. At every tile centre it calls
  // `decide(m)`, which sets m.dir and returns false if m must stop there.
  Game.prototype.advance = function (m, dist, decide, onArrive) {
    var L = this.level, guard = 0;
    while (dist > 1e-9 && guard++ < 8) {
      if (!m.moving) {
        if (!decide.call(this, m)) return;
        m.moving = true;
        m.progress = 0;
      }
      var need = 1 - m.progress;
      if (dist < need) { m.progress += dist; return; }
      dist -= need;
      var d = DIRS[m.dir];
      m.x = L.wrapX(m.x + d.x);
      m.y += d.y;
      m.progress = 0;
      m.moving = false;
      if (onArrive) onArrive.call(this, m);
      if (this.phase !== 'play') return;
    }
  };

  Game.prototype.movePlayer = function (dt) {
    var p = this.player, L = this.level;
    // Instant reversal mid-corridor.
    if (p.moving && p.want && p.want === OPPOSITE[p.dir]) {
      var d = DIRS[p.dir];
      p.x = L.wrapX(p.x + d.x);
      p.y += d.y;
      p.dir = p.want;
      p.face = p.dir;
      p.progress = 1 - p.progress;
    }
    this.advance(p, this.config.player_speed * dt, this.decidePlayer, this.playerArrived);
  };

  Game.prototype.decidePlayer = function (p) {
    var L = this.level;
    if (p.want) {
      var w = DIRS[p.want];
      if (L.playerCan(p.x + w.x, p.y + w.y)) p.dir = p.want;
    }
    if (!p.dir) return false;
    var d = DIRS[p.dir];
    if (!L.playerCan(p.x + d.x, p.y + d.y)) return false;
    p.face = p.dir;
    return true;
  };

  Game.prototype.playerArrived = function (p) {
    var L = this.level, idx = p.y * L.w + p.x, kind = L.pellets[idx];
    if (!kind) return;
    L.pellets[idx] = 0;
    L.remaining--;
    this.eaten++;
    if (kind === 1) {
      this.addScore(POINTS.seed);
    } else {
      this.addScore(POINTS.star, p.x, p.y);
      this.startFright();
    }
    if (this.gemThresholds.length && this.eaten >= this.gemThresholds[0]) {
      this.gemThresholds.shift();
      this.gem = { x: L.player.x, y: L.player.y, until: this.levelTime + 9 };
    }
    if (L.remaining <= 0) this.setPhase('cleared', 2.2);
  };

  Game.prototype.startFright = function () {
    this.frightTimer = this.config.frightened;
    this.chain = 0;
    this.sentries.forEach(function (s) {
      if (s.state === 'eaten') return;
      s.frightened = true;
      if (s.state === 'active') s.reverse = true;
    });
  };

  Game.prototype.sentrySpeed = function (s) {
    var cfg = this.config, base = cfg.sentry_speed;
    if (s.state === 'eaten') return base * 2.2;
    if (s.state === 'leaving') return base * 0.6;
    var f = 1;
    if (s.frightened) f *= cfg.frightened_factor || 0.55;
    if (this.level.tile(s.x, s.y) === '~') f *= cfg.tunnel_factor || 0.5;
    return base * f;
  };

  Game.prototype.moveSentry = function (s, dt) {
    if (s.state === 'den') {
      if (this.levelTime >= s.releaseAt) s.state = 'leaving';
      else return;
    }
    this.advance(s, this.sentrySpeed(s) * dt, this.decideSentry, null);
  };

  Game.prototype.stepToward = function (s, dist) {
    var L = this.level, here = dist[s.y * L.w + s.x], best = null, bestD = here;
    for (var k = 0; k < 4; k++) {
      var name = DIR_ORDER[k], d = DIRS[name];
      var nx = L.wrapX(s.x + d.x), ny = s.y + d.y;
      if (!L.sentryCan(nx, ny, true)) continue;
      var nd = dist[ny * L.w + nx];
      if (nd >= 0 && nd < bestD) { bestD = nd; best = name; }
    }
    if (!best) return false;
    s.dir = best;
    return true;
  };

  Game.prototype.decideSentry = function (s) {
    var L = this.level;
    if (s.state === 'eaten') {
      if (s.x === L.denCenter.x && s.y === L.denCenter.y) {
        s.state = 'leaving';
      } else {
        return this.stepToward(s, L.distDen);
      }
    }
    if (s.state === 'leaving') {
      if (s.x === L.exit.x && s.y === L.exit.y) {
        s.state = 'active';
        s.dir = Math.random() < 0.5 ? 'left' : 'right';
      } else {
        return this.stepToward(s, L.distExit);
      }
    }

    // Active: never reverse unless forced, pick the turn closest to target.
    if (s.reverse) {
      s.reverse = false;
      var r = OPPOSITE[s.dir], rd = DIRS[r];
      if (L.sentryCan(s.x + rd.x, s.y + rd.y, false)) { s.dir = r; return true; }
    }
    var options = [];
    for (var k = 0; k < 4; k++) {
      var name = DIR_ORDER[k], d = DIRS[name];
      if (name === OPPOSITE[s.dir]) continue;
      if (L.sentryCan(s.x + d.x, s.y + d.y, false)) options.push(name);
    }
    if (!options.length) {
      var back = OPPOSITE[s.dir], bd = DIRS[back];
      if (!L.sentryCan(s.x + bd.x, s.y + bd.y, false)) return false;
      s.dir = back;
      return true;
    }
    if (s.frightened) {
      s.dir = options[Math.floor(Math.random() * options.length)];
      return true;
    }
    var target = this.targetFor(s), best = options[0], bestD = Infinity;
    options.forEach(function (name) {
      var d = DIRS[name];
      var dx = s.x + d.x - target.x, dy = s.y + d.y - target.y;
      var dd = dx * dx + dy * dy;
      if (dd < bestD) { bestD = dd; best = name; }
    });
    s.dir = best;
    return true;
  };

  Game.prototype.targetFor = function (s) {
    if (this.scatter) return s.corner;
    var p = this.player, pd = DIRS[p.face];
    switch (s.kind) {
      case 0: // Hunter: straight at the blob
        return { x: p.x, y: p.y };
      case 1: // Cutter: three tiles ahead of the blob
        return { x: p.x + 3 * pd.x, y: p.y + 3 * pd.y };
      case 2: { // Pincer: the Hunter's position mirrored through the blob
        var h = this.sentries[0];
        return { x: 2 * p.x - h.x, y: 2 * p.y - h.y };
      }
      default: { // Drifter: a random spot, re-picked every few seconds
        if (!s.wander || this.levelTime >= s.wanderUntil ||
            (s.x === s.wander.x && s.y === s.wander.y)) {
          var open = this.level.open;
          s.wander = open[Math.floor(Math.random() * open.length)];
          s.wanderUntil = this.levelTime + 3;
        }
        return s.wander;
      }
    }
  };

  /* ---------- collisions ---------- */

  Game.prototype.posOf = function (m) {
    if (!m.moving || !m.dir) return { x: m.x, y: m.y };
    var d = DIRS[m.dir];
    return { x: m.x + d.x * m.progress, y: m.y + d.y * m.progress };
  };

  Game.prototype.checkCollisions = function () {
    var pp = this.posOf(this.player);
    if (this.gem && Math.abs(pp.x - this.gem.x) + Math.abs(pp.y - this.gem.y) < 0.6) {
      this.addScore(this.config.gem_value || 100, this.gem.x, this.gem.y);
      this.gem = null;
    }
    for (var i = 0; i < this.sentries.length; i++) {
      var s = this.sentries[i];
      if (s.state === 'den' || s.state === 'eaten') continue;
      var sp = this.posOf(s);
      if (Math.abs(sp.x - pp.x) + Math.abs(sp.y - pp.y) >= 0.7) continue;
      if (s.frightened) {
        s.frightened = false;
        s.state = 'eaten';
        s.moving = false;
        s.progress = 0;
        this.chain++;
        this.addScore(POINTS.sentry * Math.pow(2, this.chain - 1), sp.x, sp.y);
        this.burst(sp.x, sp.y, s.color, 10);
        this.freeze = 0.35;
      } else {
        this.burst(pp.x, pp.y, COLORS.player, 22);
        this.setPhase('dying', 1.6);
        return true;
      }
    }
    return false;
  };

  Game.prototype.burst = function (x, y, color, n) {
    for (var i = 0; i < n; i++) {
      var a = (i / n) * Math.PI * 2 + Math.random() * 0.4;
      var v = 2 + Math.random() * 3;
      this.particles.push({
        x: x, y: y, vx: Math.cos(a) * v, vy: Math.sin(a) * v,
        t: 0, life: 0.6 + Math.random() * 0.5, color: color
      });
    }
  };

  /* ------------------------------------------------------------------ */
  /* Rendering                                                           */
  /* ------------------------------------------------------------------ */

  Game.prototype.resize = function () {
    var w = this.level ? this.level.w : DEFAULT_SIZE.w;
    var h = this.level ? this.level.h : DEFAULT_SIZE.h;
    var parent = this.canvas.parentElement;
    var avail = parent ? parent.clientWidth : 600;
    var maxH = Math.max(240, window.innerHeight - 190);
    var ts = Math.max(10, Math.floor(Math.min(avail / w, maxH / h, 34)));
    var dpr = window.devicePixelRatio || 1;
    this.ts = ts;
    this.dpr = dpr;
    this.canvas.style.width = (w * ts) + 'px';
    this.canvas.style.height = (h * ts) + 'px';
    this.canvas.width = Math.round(w * ts * dpr);
    this.canvas.height = Math.round(h * ts * dpr);
    this.wallCache = null;
  };

  // Pre-render walls once per level/size; two versions for the clear flash.
  Game.prototype.buildWalls = function () {
    var L = this.level, ts = this.ts, dpr = this.dpr;
    var make = function (edgeColor) {
      var c = document.createElement('canvas');
      c.width = Math.round(L.w * ts * dpr);
      c.height = Math.round(L.h * ts * dpr);
      var g = c.getContext('2d');
      g.setTransform(ts * dpr, 0, 0, ts * dpr, 0, 0);
      var wall = function (x, y) {
        if (x < 0 || y < 0 || x >= L.w || y >= L.h) return true;
        return L.grid[y][x] === '#';
      };
      var I = 0.2, edges = [];
      g.fillStyle = COLORS.wallFill;
      for (var y = 0; y < L.h; y++) {
        for (var x = 0; x < L.w; x++) {
          if (!wall(x, y)) continue;
          var oL = !wall(x - 1, y), oR = !wall(x + 1, y), oT = !wall(x, y - 1), oB = !wall(x, y + 1);
          var l = x + (oL ? I : 0), r = x + 1 - (oR ? I : 0);
          var t = y + (oT ? I : 0), b = y + 1 - (oB ? I : 0);
          g.fillRect(l, t, r - l, b - t);
          if (oL) edges.push([l, t, l, b]);
          if (oR) edges.push([r, t, r, b]);
          if (oT) edges.push([l, t, r, t]);
          if (oB) edges.push([l, b, r, b]);
          // Concave corners: cut a notch and outline it.
          [[-1, -1], [1, -1], [-1, 1], [1, 1]].forEach(function (dd) {
            var dx = dd[0], dy = dd[1];
            if (!(wall(x + dx, y) && wall(x, y + dy) && !wall(x + dx, y + dy))) return;
            var cx = dx < 0 ? x : x + 1 - I, cy = dy < 0 ? y : y + 1 - I;
            g.clearRect(cx, cy, I, I);
            var ix = dx < 0 ? x + I : x + 1 - I, iy = dy < 0 ? y + I : y + 1 - I;
            var ex = dx < 0 ? x : x + 1, ey = dy < 0 ? y : y + 1;
            edges.push([ix, ey, ix, iy]);
            edges.push([ix, iy, ex, iy]);
          });
        }
      }
      g.strokeStyle = edgeColor;
      g.lineWidth = 0.09;
      g.lineCap = 'round';
      g.lineJoin = 'round';
      g.shadowColor = edgeColor;
      g.shadowBlur = ts * 0.45 * dpr;
      g.beginPath();
      edges.forEach(function (e) { g.moveTo(e[0], e[1]); g.lineTo(e[2], e[3]); });
      g.stroke();
      // Den gate.
      g.shadowBlur = 0;
      g.strokeStyle = COLORS.gate;
      g.lineWidth = 0.14;
      g.beginPath();
      g.moveTo(L.gate.x + 0.05, L.gate.y + 0.5);
      g.lineTo(L.gate.x + 0.95, L.gate.y + 0.5);
      g.stroke();
      return c;
    };
    this.wallCache = { normal: make(COLORS.wallEdge), flash: make(COLORS.wallFlash) };
  };

  Game.prototype.render = function () {
    var ctx = this.ctx, ts = this.ts, dpr = this.dpr;
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.fillStyle = COLORS.bg;
    ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);

    if (!this.level) { this.renderTitle(); return; }
    if (!this.wallCache) this.buildWalls();

    var flash = this.phase === 'cleared' && Math.floor(this.anim * 6) % 2 === 0;
    ctx.drawImage(flash ? this.wallCache.flash : this.wallCache.normal, 0, 0);

    ctx.setTransform(ts * dpr, 0, 0, ts * dpr, 0, 0);
    this.drawPellets(ctx);
    if (this.gem) this.drawGem(ctx, this.gem.x + 0.5, this.gem.y + 0.5);
    var self = this;
    if (this.phase !== 'cleared' && this.phase !== 'waiting') {
      this.sentries.forEach(function (s) {
        if (self.phase === 'dying' && self.phaseTimer < 1.2) return;
        self.drawSentry(ctx, s);
      });
    }
    this.drawPlayer(ctx);
    this.drawParticles(ctx);
    this.drawPopups(ctx);

    if (this.paused) this.banner('PAUSED', COLORS.text);
    else if (this.phase === 'ready') this.banner('READY', COLORS.star);
    else if (this.phase === 'cleared' || this.phase === 'waiting') this.banner('MAZE CLEAR', COLORS.wallEdge);
    else if (this.phase === 'over') this.banner('GAME OVER', COLORS.gate);
  };

  Game.prototype.drawPellets = function (ctx) {
    var L = this.level, pulse = 1 + 0.15 * Math.sin(this.anim * 6);
    ctx.fillStyle = COLORS.seed;
    for (var y = 0; y < L.h; y++) {
      for (var x = 0; x < L.w; x++) {
        if (L.pellets[y * L.w + x] !== 1) continue;
        ctx.beginPath();
        ctx.moveTo(x + 0.5, y + 0.38);
        ctx.lineTo(x + 0.6, y + 0.5);
        ctx.lineTo(x + 0.5, y + 0.62);
        ctx.lineTo(x + 0.4, y + 0.5);
        ctx.closePath();
        ctx.fill();
      }
    }
    ctx.save();
    ctx.fillStyle = COLORS.star;
    ctx.shadowColor = COLORS.star;
    ctx.shadowBlur = this.ts * 0.6 * this.dpr;
    for (var yy = 0; yy < L.h; yy++) {
      for (var xx = 0; xx < L.w; xx++) {
        if (L.pellets[yy * L.w + xx] !== 2) continue;
        this.starPath(ctx, xx + 0.5, yy + 0.5, 0.36 * pulse, 0.16 * pulse, this.anim);
        ctx.fill();
      }
    }
    ctx.restore();
  };

  Game.prototype.starPath = function (ctx, cx, cy, outer, inner, rot) {
    ctx.beginPath();
    for (var i = 0; i < 10; i++) {
      var r = i % 2 === 0 ? outer : inner;
      var a = rot + (i * Math.PI) / 5 - Math.PI / 2;
      var px = cx + Math.cos(a) * r, py = cy + Math.sin(a) * r;
      if (i === 0) ctx.moveTo(px, py); else ctx.lineTo(px, py);
    }
    ctx.closePath();
  };

  Game.prototype.drawGem = function (ctx, cx, cy) {
    var sx = Math.abs(Math.cos(this.anim * 3)) * 0.9 + 0.1;
    ctx.save();
    ctx.translate(cx, cy);
    ctx.scale(sx, 1);
    ctx.fillStyle = COLORS.gem;
    ctx.shadowColor = COLORS.gem;
    ctx.shadowBlur = this.ts * 0.5 * this.dpr;
    ctx.beginPath();
    ctx.moveTo(0, -0.38); ctx.lineTo(0.3, -0.1); ctx.lineTo(0, 0.38); ctx.lineTo(-0.3, -0.1);
    ctx.closePath();
    ctx.fill();
    ctx.fillStyle = 'rgba(255,255,255,0.55)';
    ctx.beginPath();
    ctx.moveTo(0, -0.38); ctx.lineTo(0.12, -0.1); ctx.lineTo(-0.12, -0.1);
    ctx.closePath();
    ctx.fill();
    ctx.restore();
  };

  Game.prototype.drawBlob = function (ctx, cx, cy, face, squash, scale) {
    var d = DIRS[face] || DIRS.left;
    var horiz = d.x !== 0;
    ctx.save();
    ctx.translate(cx, cy);
    ctx.scale(scale * (1 + (horiz ? squash : -squash)), scale * (1 + (horiz ? -squash : squash)));
    // Sprout on top.
    ctx.fillStyle = COLORS.leaf;
    ctx.beginPath();
    ctx.ellipse(0.1, -0.44, 0.13, 0.06, -0.6, 0, Math.PI * 2);
    ctx.fill();
    // Body.
    var grad = ctx.createRadialGradient(-0.14, -0.16, 0.05, 0, 0, 0.45);
    grad.addColorStop(0, '#d8ffe1');
    grad.addColorStop(0.35, COLORS.player);
    grad.addColorStop(1, COLORS.playerDark);
    ctx.fillStyle = grad;
    ctx.beginPath();
    ctx.arc(0, 0, 0.42, 0, Math.PI * 2);
    ctx.fill();
    // Eyes look where the blob is heading.
    var ox = d.x * 0.1, oy = d.y * 0.1 - 0.05;
    [-0.14, 0.14].forEach(function (ex) {
      ctx.fillStyle = '#ffffff';
      ctx.beginPath();
      ctx.arc(ex + ox, oy, 0.11, 0, Math.PI * 2);
      ctx.fill();
      ctx.fillStyle = '#1b1030';
      ctx.beginPath();
      ctx.arc(ex + ox + d.x * 0.04, oy + d.y * 0.04, 0.055, 0, Math.PI * 2);
      ctx.fill();
    });
    ctx.restore();
  };

  Game.prototype.drawPlayer = function (ctx) {
    var p = this.player, pos = this.posOf(p);
    var cx = pos.x + 0.5, cy = pos.y + 0.5;
    if (this.phase === 'dying') {
      var k = 1 - Math.max(0, this.phaseTimer) / 1.6;
      if (k > 0.75) return;
      this.drawBlob(ctx, cx, cy, p.face, 0.25 * Math.sin(k * 30), 1 - k / 0.75);
      return;
    }
    var moving = p.moving && this.phase === 'play' && !this.paused;
    var squash = moving ? Math.sin(this.anim * 18) * 0.09 : Math.sin(this.anim * 3) * 0.03;
    this.drawBlob(ctx, cx, cy, p.face, squash, 1);
    // Wrap-around: draw a ghost copy on the far edge while in a tunnel.
    var L = this.level;
    if (pos.x < 0) this.drawBlob(ctx, cx + L.w, cy, p.face, squash, 1);
    else if (pos.x > L.w - 1) this.drawBlob(ctx, cx - L.w, cy, p.face, squash, 1);
  };

  Game.prototype.hexPath = function (ctx, r) {
    ctx.beginPath();
    for (var i = 0; i < 6; i++) {
      var a = (i * Math.PI) / 3;
      if (i === 0) ctx.moveTo(Math.cos(a) * r, Math.sin(a) * r);
      else ctx.lineTo(Math.cos(a) * r, Math.sin(a) * r);
    }
    ctx.closePath();
  };

  Game.prototype.drawSentry = function (ctx, s) {
    var pos = this.posOf(s), cx = pos.x + 0.5, cy = pos.y + 0.5;
    if (s.state === 'den') cy += Math.sin(this.anim * 5 + s.kind * 1.7) * 0.12;
    var pp = this.posOf(this.player);
    var vx = pp.x - pos.x, vy = pp.y - pos.y, vl = Math.sqrt(vx * vx + vy * vy) || 1;
    if (s.state === 'eaten') {
      var ed = DIRS[s.dir] || DIRS.left;
      vx = ed.x; vy = ed.y; vl = 1;
    }
    ctx.save();
    ctx.translate(cx, cy);

    if (s.state !== 'eaten') {
      var color = s.color;
      if (s.frightened) {
        var blink = this.frightTimer < 2 && Math.floor(this.anim * 8) % 2 === 0;
        color = blink ? COLORS.frightFlash : COLORS.frightened;
        ctx.rotate(Math.sin(this.anim * 12 + s.kind) * 0.18);
      } else {
        ctx.rotate(Math.sin(this.anim * 2 + s.kind) * 0.06);
      }
      ctx.fillStyle = color;
      ctx.shadowColor = color;
      ctx.shadowBlur = this.ts * 0.5 * this.dpr;
      this.hexPath(ctx, 0.45);
      ctx.fill();
      ctx.shadowBlur = 0;
      ctx.strokeStyle = 'rgba(20, 11, 36, 0.45)';
      ctx.lineWidth = 0.05;
      this.hexPath(ctx, 0.33);
      ctx.stroke();
      // Antenna.
      ctx.strokeStyle = color;
      ctx.lineWidth = 0.05;
      ctx.beginPath();
      ctx.moveTo(0, -0.38); ctx.lineTo(0, -0.52);
      ctx.stroke();
      ctx.fillStyle = color;
      ctx.beginPath();
      ctx.arc(0, -0.54, 0.05, 0, Math.PI * 2);
      ctx.fill();
    }

    if (s.frightened && s.state !== 'eaten') {
      // Dizzy squiggle instead of an eye.
      ctx.strokeStyle = '#3a2a6b';
      ctx.lineWidth = 0.06;
      ctx.beginPath();
      for (var i = 0; i <= 8; i++) {
        var x = -0.2 + i * 0.05, y = Math.sin(i * 1.6 + this.anim * 10) * 0.05;
        if (i === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
      }
      ctx.stroke();
    } else {
      ctx.fillStyle = '#ffffff';
      ctx.beginPath();
      ctx.arc(0, 0, 0.17, 0, Math.PI * 2);
      ctx.fill();
      ctx.fillStyle = '#1b1030';
      ctx.beginPath();
      ctx.arc((vx / vl) * 0.07, (vy / vl) * 0.07, 0.08, 0, Math.PI * 2);
      ctx.fill();
    }
    ctx.restore();
  };

  Game.prototype.drawParticles = function (ctx) {
    this.particles.forEach(function (p) {
      ctx.globalAlpha = Math.max(0, 1 - p.t / p.life);
      ctx.fillStyle = p.color;
      ctx.beginPath();
      ctx.arc(p.x + 0.5, p.y + 0.5, 0.07, 0, Math.PI * 2);
      ctx.fill();
    });
    ctx.globalAlpha = 1;
  };

  // Text is drawn in pixel space; tiny fractional font sizes are unreliable.
  Game.prototype.drawPopups = function (ctx) {
    var ts = this.ts, dpr = this.dpr;
    ctx.save();
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.font = 'bold ' + Math.round(ts * 0.55) + 'px system-ui, sans-serif';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    this.popups.forEach(function (p) {
      ctx.globalAlpha = Math.max(0, 1 - p.t / 1.2);
      ctx.fillStyle = COLORS.star;
      ctx.fillText(p.text, (p.x + 0.5) * ts, (p.y + 0.5 - p.t * 0.8) * ts);
    });
    ctx.restore();
  };

  Game.prototype.banner = function (text, color) {
    var ctx = this.ctx, ts = this.ts, dpr = this.dpr, L = this.level;
    var w = (L ? L.w : DEFAULT_SIZE.w) * ts;
    var y = L ? (L.denCenter.y + 2.5) * ts : 0.5 * DEFAULT_SIZE.h * ts;
    ctx.save();
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.fillStyle = 'rgba(20, 11, 36, 0.78)';
    ctx.fillRect(0, y - ts * 0.75, w, ts * 1.5);
    ctx.font = '800 ' + Math.round(ts * 0.95) + 'px system-ui, sans-serif';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillStyle = color;
    ctx.shadowColor = color;
    ctx.shadowBlur = ts * 0.6;
    ctx.fillText(text, w / 2, y);
    ctx.restore();
  };

  Game.prototype.renderTitle = function () {
    var ctx = this.ctx, ts = this.ts, dpr = this.dpr, W = DEFAULT_SIZE.w, H = DEFAULT_SIZE.h;
    ctx.save();
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillStyle = COLORS.wallEdge;
    ctx.shadowColor = COLORS.wallEdge;
    ctx.shadowBlur = ts;
    ctx.font = '900 ' + Math.round(ts * 2) + 'px system-ui, sans-serif';
    ctx.fillText('MAZE MUNCHER', (W * ts) / 2, H * ts * 0.3);
    ctx.shadowBlur = 0;
    ctx.fillStyle = COLORS.text;
    ctx.font = '500 ' + Math.round(ts * 0.7) + 'px system-ui, sans-serif';
    ctx.fillText('Pick your settings and press Start game', (W * ts) / 2, H * ts * 0.72);
    ctx.fillStyle = '#8f88b3';
    ctx.fillText('Munch every seed \u00b7 grab a star to stun the sentries', (W * ts) / 2, H * ts * 0.8);
    ctx.restore();

    ctx.setTransform(ts * dpr * 1.6, 0, 0, ts * dpr * 1.6, 0, 0);
    var row = (H * 0.5) / 1.6, startX = (W / 1.6) / 2 - 3.2;
    var bob = Math.sin(this.anim * 6) * 0.08;
    this.drawBlob(ctx, startX, row + bob, 'right', Math.sin(this.anim * 18) * 0.08, 1);
    var self = this;
    var fakePlayer = { x: startX - 0.5, y: row - 0.5, moving: false };
    var saved = this.player;
    this.player = fakePlayer;
    SENTRY_TYPES.forEach(function (t, i) {
      var s = { kind: i, color: t.color, x: startX + 1.6 + i * 1.3 - 0.5, y: row - 0.5, state: 'den', moving: false, frightened: false };
      self.drawSentry(ctx, s);
    });
    this.player = saved;
  };

  /* ------------------------------------------------------------------ */
  /* Wiring: canvases, keyboard, Shiny message handlers                  */
  /* ------------------------------------------------------------------ */

  function gameFor(id) {
    if (games[id]) return games[id];
    var canvas = document.getElementById(id);
    if (!canvas) return null;
    games[id] = new Game(canvas);
    return games[id];
  }

  function isTyping(el) {
    if (!el) return false;
    var tag = el.tagName;
    return tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT' || el.isContentEditable;
  }

  document.addEventListener('keydown', function (e) {
    var g = activeGame;
    if (!g || isTyping(e.target)) return;
    var dir = KEYMAP[e.code];
    if (dir) {
      if (g.isLive()) {
        e.preventDefault();
        g.setWant(dir);
      }
    } else if (e.code === 'KeyP' || e.code === 'Escape') {
      if (g.isLive()) {
        e.preventDefault();
        g.togglePause();
      }
    }
  });

  document.addEventListener('visibilitychange', function () {
    if (document.hidden && activeGame && activeGame.phase === 'play') activeGame.paused = true;
  });

  function init() {
    var canvases = document.querySelectorAll('canvas.mm-canvas');
    for (var i = 0; i < canvases.length; i++) gameFor(canvases[i].id);

    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) return;
    window.Shiny.addCustomMessageHandler('mazemuncher:start', function (msg) {
      var g = gameFor(msg.canvas);
      if (!g) return;
      if (document.activeElement && document.activeElement.blur) document.activeElement.blur();
      g.newGame(msg);
      g.canvas.focus({ preventScroll: true });
    });
    window.Shiny.addCustomMessageHandler('mazemuncher:level', function (msg) {
      var g = gameFor(msg.canvas);
      if (g) g.loadLevel(msg.config);
    });
    window.Shiny.addCustomMessageHandler('mazemuncher:stop', function (msg) {
      var g = gameFor(msg.canvas);
      if (g) g.stop();
    });
  }

  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();

  // Exposed for debugging in the browser console.
  window.MazeMuncher = { games: games };
})();
