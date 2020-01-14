import React, {useState} from 'react';
import SnippetHolder from './containers/SnippetHolder.jsx';

const App = () => {
	let [jsonInput, setJsonInput] = useState('');
	let [consume, setConsume] = useState('');
	let [instanceName, setInstanceName] = useState('');
	let [errorMsg, setErrorMsg] = useState('');

	let snippetProps = {
		target: consume,
		updateConsume: setConsume,
		targetName: instanceName,
		updateError: setErrorMsg
	};

	return (
		<div className="container main">
			<div className="header">
				<h1>THIS SET IS THE HEADER</h1>
				<p>{errorMsg} {errorMsg && ' Please try again!'}</p>
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

							setInstanceName(e.target.value);
						}}
					/>
				</p>
				<textarea rows="30" className="json-input" onChange={(e) => {
					if (!e || !e.target || !e.target.value) {
						console.error("Failed to read event:");
						console.error(e);
						return
					}

					setConsume(e.target.value);
					return setJsonInput(e.target.value);
				}} />
			</div>
			<div>
				<SnippetHolder {...snippetProps} />
			</div>
		</div>
	);
};

export default App;
