import React, {useState} from 'react';
import SnippetHolder from './containers/SnippetHolder.jsx';

const initState = {
	jsonInput: '',
	consumeJSON: '',
	instanceName: '',
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
		setStateField('consumeJSON', nextValue);
	}

	const updateConsume = (nextValue) => {
		setStateField('consumeJSON', nextValue);
	}

	const updateError = (nextError) => {
		setStateField('errorMsg', nextError);
	}

	const updateInstanceName = (nextInstance) => {
		setStateField('instanceName', nextInstance)
	}

	let snippetProps = {
		target: state.consumeJSON,
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
