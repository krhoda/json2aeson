import React, {useState} from 'react';
import SnippetHolder from './containers/SnippetHolder.jsx';

const initState = {
	jsonInput: '',
	errorMsg: ''
};

const App = () => {
	let [state, setState] = useState([ {...initState} ]);

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
		setStateField('jsonInput', nextValue);
	}

	const updateError = (nextError) => {
		setStateField('errorMsg', nextError);
	}

	let snippetProps = {
		target: state.jsonInput,
		updateError: updateError
	};

	return (
		<div className="container main">
			<div className="header">
				<h1>THIS SET IS THE HEADER</h1>
			</div>
			<div>
				<p>THIS SHOULD BE LEFT.</p>
				<textarea rows="30" className="json-input" onChange={(e) => {
					if (!e || !e.target || !e.target.value) {
						console.error("FAILED TO READ EVENT");
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
