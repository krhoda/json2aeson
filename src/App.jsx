import React, {useState} from 'react';
import SnippetHolder from './containers/SnippetHolder.jsx';

const initState = {
	jsonInput: '',
	consume: '',
	instanceName: '',
	errorMsg: ''
};

const App = () => {
	let [state, setState] = useState({...initState});

	const safeSetState = (nextState) => {
		setState((prevState) => {
			let merged = Object.assign({}, prevState, nextState);
			let same = comparePrevWithMerged(prevState, merged);
			console.log(nextState);
			console.log(prevState);
			console.log(merged);
			console.log(same);

			if (!same) {
				return merged;
			}

			return prevState;
		});
	}

	const comparePrevWithMerged = (prevState, merged) => {
		let mergedKeys = Object.keys(merged);

		for (
			let mergedIndex = 0,
				x = mergedKeys.length;
			mergedIndex < x;
			mergedIndex++
		) {
			let sharedKey = mergedKeys[mergedIndex];

			let prevVal = prevState[sharedKey];
			let mergedVal = mergedKeys[sharedKey];

			if (!compareAnything(prevVal, mergedVal)) {
				return false;
			}
		}
	}

	const compareAnything = (a, b) => {
		let aT = typeof a;
		let bT = typeof b;

		if (aT !== bT) {
			return false;
		}

		switch (aT) {
			case "function":
				console.warn("Attempting to assert the equality of two functions...");
				console.warn("...I ain't no GHC, but I'll sure try");
				return a.toString() === b.toString();

			case "object":
				if (null === a === b) {
					return true;
				}

				if (Array.isArray(a)) {
					if (!Array.isArray(b)) {
						return false;
					}

					if (a.length !== b.length) {
						return false;
					}

					for (let i = 0, x = a.length; i < x; i++) {
						let a1 = a[i];
						let b1 = b[i];
						if (!compareAnything(a1, b1)) {
							return false;
						}
					}
				}

				return comparePrevWithMerge(a, b);

			default:
				return a === b;
		}
	}

	const setStateField = (nextKey, nextValue) => {
		let update = {};
		update[nextKey] = nextValue;

		let lastValue = state[nextKey];
		if (lastValue === nextValue) {
			return;
		}

		let nextState = Object.assign({}, state, update);
		setState(nextState);
	}

	const updateInput = (nextValue) => {
		let nextState = {
			'jsonInput': nextValue,
			'consume': nextValue
		};

		safeSetState(nextState);

		/* setStateField('jsonInput', nextValue); */
		/* setStateField('consume', nextValue); */
	};

	const updateConsume = (nextValue) => {
		safeSetState({'consume': nextValue});

		/* setStateField('consume', nextValue); */
	};

	const updateError = (nextError) => {
		safeSetState({'errorMsg': nextError});

		/* setStateField('errorMsg', nextError); */
	};

	const updateInstanceName = (nextInstance) => {
		safeSetState({'instanceName': nextInstance})

		/* setStateField('instanceName', nextInstance) */
	};

	console.log(state);
	let snippetProps = {
		target: state.consume,
		updateConsume: updateConsume,
		targetName: state.instanceName,
		updateError: updateError
	};

	return (
		<div className="container main">
			<div className="header">
				<h1>THIS SET IS THE HEADER</h1>
				<p>{state.errorMsg} {state.errorMsg && ' Please try again!'}</p>
			</div>
			<div>
				<p>Name the instance:</p>
				<p>
					<input
						type="text"
						onChange={(e) => {
							if (!e || !e.target || !e.target.value) {
								console.error("Failed to read event:");
								console.error(e);
								return
							}

							return updateInstanceName(e.target.value);
						}}
					/>
				</p>

				{/* <p><button onClick={() => {updateConsume(state.jsonInput)}}>Go!</button></p> */}

				<textarea rows="30" className="json-input" onChange={(e) => {
					if (!e || !e.target || !e.target.value) {
						console.error("Failed to read event:");
						console.error(e);
						return
					}

					return updateInput(e.target.value);
				}} />
			</div>
			<div>
				<p>THIS SHOULD BE RIGHT.</p>
				<SnippetHolder {...snippetProps} />
			</div>
		</div>
	);
};

export default App;
