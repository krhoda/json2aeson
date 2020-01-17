import React from 'react';

const Input = (props) => {
	return (
		<textarea rows="30" className="json-input" onChange={(e) => {
			handleInput(e, props);
		}} />
	);
};

const handleInput = (e, props) => {
	let {setJSON, instanceName, setBadWordList, setError} = props;

	if (!e || !e.target || !e.target.value) {
		console.error('Failed to read event:');
		console.error(e);
		setError(`${e}`);
		return;
	}

	let parsed = checkJSON(e.target.value, setError);
	if (parsed) {
		let badWordList = [];
		badWordList = deepCheckForBadWords(parsed, instanceName, badWordList);

		setBadWordList(badWordList);
		console.log("BAD WORD LIST ", badWordList);

		return setJSON(parsed);
	}

	setJSON(false);
}

const checkJSON = (target, setError) => {
	try {
		let parsed = JSON.parse(target);
		setError('');
		return parsed;
	} catch (err) {
		setError(`${err}`);
		return false;
	}
}

const deepCheckForBadWords = (parsed, instanceName, badWordList) => {
	let pKeys = Object.keys(parsed);
	for (let i = 0, x = pKeys.length; i < x; i ++) {
		let k = pKeys[i];
		let nextK = k;

		let a = parsed[k];

		let bad = checkForBadWords(k);

		if (bad) {
			nextK = `${instanceName.toLowerCase()}_${k}`;
			let nextBadWord = {
				oldWord: k,
				newWord: nextK
			};
			badWordList.push(nextBadWord);
		}

		if (typeof a === "object") {
			badWordList = deepCheckForBadWords(a, nextK, badWordList)
		}
	}

	return badWordList
}

// TODO: BREAK INTO OWN MODEL -- (W)e (E)njoy (T)yping v DRY.
const checkForBadWords = (word) => {
	for (let i = 0, x = badWords.length; i < x; i++)  {
		if (word === badWords[i]) {
			return word;
		}
	}

	return false;
};

const badWords = [
	'case',
	'class',
	'data',
	'default',
	'deriving',
	'do',
	'else',
	'forall',
	'id',
	'if',
	'import',
	'in',
	'infix',
	'infixl',
	'infixr',
	'instance',
	'let',
	'module',
	'newtype',
	'of',
	'qualified',
	'then',
	'type',
	'where',
	'_',
	'foreign',
	'ccall',
	'as',
	'safe',
	'unsafe'
];

export default Input;
