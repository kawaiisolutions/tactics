import {
	load,
	Prolog,
	Atom,
	Compound,
	fromJSON
} from "https://esm.sh/trealla@0.21.13?keep-names";

let b4load = Date.now();
await load();

const FILES = ["tactics", "unit", "world"];

let pl = null;
let state = null;
let jstate = null;
let focus = null;
let turns = 0;
let moveRadius = [];
let attackRadius = [];
let matchStatus = null;
let menu = [];

const keybinds = {
	m: view.bind(globalThis, "move"),
	a: view.bind(globalThis, "attack"),
	t: endTurn,
	Escape: view.bind(globalThis, null),
};

function setup() {
	document.getElementById("end-turn-btn").onclick = endTurn;

	document.getElementById("reset-btn").onclick =
		async function () {
			await init();
		};
	document.getElementById("move-btn").onclick = view.bind(
		globalThis,
		"move",
	);
	document.getElementById("attack-btn").onclick = view.bind(
		globalThis,
		"attack",
	);

	document.addEventListener("keyup", (event) => {
		if (event.isComposing || event.keyCode === 229) {
			return;
		}
		const bind = keybinds[event.key];
		if (!bind) return;
		bind();
	});
}

async function endTurn() {
	await apply(new Atom("end_turn"));
	await apply(new Atom("next_turn"));
	delete document.getElementById("game").dataset.view;
}

function view(what) {
	console.log("view", what);
	const game = document.getElementById("game");
	if (what) {
		game.dataset.view = what;
	} else {
		delete game.dataset.view;
	}
}

async function init() {
	pl = new Prolog();
	state = null;
	focus = null;
	turns = 0;
	moveRadius = [];
	attackRadius = [];
	matchStatus = null;
	menu = [];

	await Promise.all(FILES.map((x) => download(x)));

	const load = await pl.queryOnce(
		`consult(tactics), ${FILES.map((x) => `use_module(${new Atom(x).toProlog()})`).join(", ")}.`,
	);
	if (load.status !== "success") {
		alert("failed to load: " + load.error);
		return;
	}
	console.log(load);

	const r = await pl.queryOnce(
		`
					begin(Seed, State, Cues),
					current_unit(State, Unit),
					move_radius(State, Unit, MoveRadius),
					attack_radius(Unit, AttackRadius),
					menu(State, Menu),
					match_status(State, MatchStatus),
					state_json_chars(State, StateJSON),
					match_controller(State, Controller),
					ai_actions(State, AI).
					`,
		{ bind: { Seed: Date.now() } },
	);
	console.log(r);
	handle(r);
	console.log("loaded", Date.now() - b4load);
}

async function download(name) {
	const filename = `/${name}.pl`;
	const script = await (await fetch("." + filename)).text();
	pl.fs
		.open(filename, { write: true, create: true })
		.writeString(script);
}

globalThis.TRACE = false;

async function apply(goal) {
	console.time("apply");
	const result = await pl.queryOnce(
		`
				  ${globalThis.TRACE ? "trace," : ""}
					do(Goal, State0, State, Cues),
					current_unit(State, Unit),
					move_radius(State, Unit, MoveRadius),
					attack_radius(Unit, AttackRadius),
					menu(State, Menu),
					match_status(State, MatchStatus),
					state_json_chars(State, StateJSON),
					match_controller(State, Controller),
					ai_actions(State, AI).
					`,
		{ bind: { Goal: goal, State0: state } },
	);
	console.timeEnd("apply");
	console.log(result);
	if (result.status === "failure") {
		log(`You can't do that.`);
		return;
	}
	log(result.stdout);
	if (result.status !== "success") {
		throw new Error(`${result.status} ${result.error}`);
	}
	handle(result);
}

function handle(result) {
	if (result.stderr) {
		console.log(result.stderr);
	}
	if (result.status == "error" || result.status == "failure") {
		log(`You can't do that. ${result.error}`);
		return;
	}

	state = result.answer.State;
	console.log(jstate);

	if (result.stderr) {
		console.log(result.stderr)
	}

	if (result.answer.StateJSON)
		jstate = JSON.parse(result.answer.StateJSON);
	if (result.answer.AttackRadius)
		attackRadius = result.answer.AttackRadius.map(
			(x) => `${x.args[0]}/${x.args[1]}`,
		);
	if (result.answer.MoveRadius)
		moveRadius = result.answer.MoveRadius.map(
			(x) => `${x.args[0]}/${x.args[1]}`,
		);
	if (result.answer.MatchStatus)
		matchStatus = result.answer.MatchStatus;
	if (result.answer.Menu)
		menu = result.answer.Menu.map((x) => x.functor);

	const cues = result.answer.Cues;
	for (const cue of cues) {
		switch (cue.functor) {
			case "tick":
				turns++;
				break;
			case "focus_unit":
				focus = cue.args[0];
				break;
		}
		console.log("cue", cue);
	}

	switch (result.answer.Controller?.functor) {
		case "player":
		case "none":
			break;
		case "ai":
			if (!runningAI) {
				console.log("run ai");
				playAI(result.answer.AI);
			}
			break;
	}
	render();
}

var runningAI = false;

async function playAI(actions) {
	try {
		runningAI = true;
		for (let i = 0; i < actions.length; i++) {
			if (i == actions.length - 1) {
				runningAI = false;
			}
			const goal = actions[i];
			console.log("ai apply", goal);
			await apply(goal);
			render();
			await new Promise((resolve) => setTimeout(resolve, 500));
		}
	} finally {
		runningAI = false;
	}
}

function render() {
	document.getElementById("turn-count").textContent = turns;
	document.getElementById("menu").dataset.can = menu.join(" ");

	if (matchStatus) {
		const ms = document.getElementById("match-status");
		document.getElementById("menu").dataset.status =
			matchStatus.functor;
		switch (matchStatus.functor) {
			case "input":
				ms.textContent = `${matchStatus.args[0].functor}'s turn`;
				break;
			case "win":
				ms.textContent = `${matchStatus.args[0].functor} wins!`;
				break;
		}
	}

	for (const tile of Array.from(
		document.querySelectorAll(".tile"),
	)) {
		const x = Number(tile.dataset.x);
		const y = Number(tile.dataset.y);
		const goal = new Compound("move", [
			new Compound("/", [x, y]),
		]);
		tile.onclick = apply.bind(tile, goal);
	}

	for (let y = 1; y <= 8; y++) {
		for (let x = 1; x <= 8; x++) {
			const pos = `${x}/${y}`;
			const id = `map-${pos}`;
			const elem = document.getElementById(id);
			elem.textContent = ".";
			elem.className = "tile";
			elem.dataset.unit = "";
			elem.classList.toggle(
				"can-attack",
				attackRadius.includes(pos),
			);
			elem.classList.toggle(
				"can-move",
				moveRadius.includes(pos),
			);
		}
	}

	Array.from(document.querySelectorAll(".focus")).forEach((x) =>
		x.classList.remove("focus"),
	);
	Array.from(document.querySelectorAll(".unit")).forEach((x) =>
		x.classList.remove("unit"),
	);

	const frag = new DocumentFragment();
	for (const ct_unit of jstate) {
		const {
			ct,
			unit,
			type,
			id,
			team,
			pos,
			hp,
			maxhp,
			mp,
			maxmp,
			speed,
			equipment,
			status,
		} = ct_unit;
		const eid = `map-${pos.x}/${pos.y}`;
		const elem = document.getElementById(eid);
		if (!elem) {
			continue;
		}
		let glyph = "@";
		if (hp === 0) {
			glyph = "%";
		}
		elem.textContent = glyph;
		elem.classList.add(`team-${team}`);
		elem.classList.add("unit");
		elem.dataset.unit = id;
		if (id == focus) {
			elem.classList.add("focus");
		}
		const attack = new Compound("attack", [id]);
		elem.onclick = apply.bind(elem, attack);

		const li = document.createElement("li");
		li.textContent = `${team} ${type} (HP: ${hp}/${maxhp}, MP: ${mp}/${maxmp}) [CT: ${ct}]`;
		li.style.textDecoration = "underline";
		li.dataset.unit = id;
		li.style.textDecorationColor = team;
		frag.appendChild(li);
	}
	const units = document.getElementById("units");
	units.textContent = "";
	units.append(frag);
}

function log(msg) {
	if (msg == "") return;
	const elem = document.getElementById("log");
	// elem.innerText = `${msg.trim()}\n\n\u2042\n\n${elem.innerText}`;
	elem.innerText = msg;
}

function tryCrash() {
	state = fromJSON('[{"functor":"-","args":[44,{"functor":"unit","args":[2,{"functor":"red","args":[]},{"functor":"dog","args":[]},{"functor":"/","args":[2,4]},{"functor":"/","args":[1,1]},{"functor":"/","args":[1,1]},12,[{"functor":"weapon","args":[{"functor":"bite","args":[]},1,3]}],[{"functor":"wait","args":[]}]]}]},{"functor":"-","args":[91,{"functor":"unit","args":[1,{"functor":"red","args":[]},{"functor":"soldier","args":[]},{"functor":"/","args":[4,4]},{"functor":"/","args":[10,10]},{"functor":"/","args":[5,5]},13,[{"functor":"weapon","args":[{"functor":"sword","args":[]},3,7]}],[{"functor":"wait","args":[]}]]}]},{"functor":"-","args":[85,{"functor":"unit","args":[3,{"functor":"blue","args":[]},{"functor":"guy","args":[]},{"functor":"/","args":[2,5]},{"functor":"/","args":[0,10]},{"functor":"/","args":[5,5]},15,[{"functor":"weapon","args":[{"functor":"sword","args":[]},3,7]}],[{"functor":"wait","args":[]},{"functor":"dead","args":[]}]]}]},{"functor":"-","args":[84,{"functor":"unit","args":[4,{"functor":"blue","args":[]},{"functor":"cat","args":[]},{"functor":"/","args":[5,4]},{"functor":"/","args":[0,1]},{"functor":"/","args":[1,1]},12,[{"functor":"weapon","args":[{"functor":"scratch","args":[]},1,3]}],[{"functor":"wait","args":[]},{"functor":"dead","args":[]}]]}]}]');

	var GOAL = fromJSON('{"functor":"next_turn","args":[]}');
	apply(GOAL);
}
globalThis.tryCrash = tryCrash;


setup();
await init();
