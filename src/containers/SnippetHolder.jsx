import React from 'react';
import SyntaxHighlighter from 'react-syntax-highlighter';
import { gruvboxDark } from 'react-syntax-highlighter/dist/esm/styles/hljs';

const SnippetHolder = (props) => {
	let {target, targetName, updateError} = props;

	let name = targetName;
	if (!name) {
		name = 'AutoGenerated';
	}

	if (!target) {
		return '';
	}

	try {
		let parsedTarget = JSON.parse(target);
		let targetObj = createRecordObject(parsedTarget, toPascalCase(name), updateError);
		if (!targetObj) {
			return '';
		}
		let snippet = renderRecordObj(targetObj);

		return (
			<div>
				<SyntaxHighlighter className="snippet" language="haskell" style={gruvboxDark}>
					{snippet}
				</SyntaxHighlighter>
			</div>
		);
	} catch (err) {
		updateError(`Cannot parse input ${err}`);
		return <p>Error encountered!</p>;
	}
};

const createRecordObject = (target, recordName, updateError) => {
	let targetKeys = Object.keys(target);
	let badWord = checkForBadWords(targetKeys);
	if (badWord) {
		updateError(`Haskell Reserved Word Detected: ${badWord}`);
		return false;
	}

	let recordBody = [];
	let nestedModels = [];

	targetKeys.forEach((key) => {
		let value = target[key];
		switch (typeof value) {
		case 'string':
			recordBody.push(`\t${key} :: Text\n`);
			break;
		case 'boolean':
			recordBody.push(`\t${key} :: Bool\n`);
			break;
		case 'number':
			recordBody.push(`\t${key} :: Number\n`);
			break;
		case 'object':
			if (Array.isArray(value)) {
				let {body, nested} = handleArrayField(value, toPascalCase(key), updateError);

				recordBody.push(body);
				if (nested) {
					nestedModels.push(nested);
				}
				break;
			}

			let nextModelRecord = createRecordObject(value, toPascalCase(key), updateError);

			let nextModel = {
				name:         toPascalCase(key),
				recordBody:   nextModelRecord.recordBody,
				nestedModels: nextModelRecord.nestedModels
			};

			recordBody.push(`\t${key} :: ${toPascalCase(key)}\n`);
			nestedModels.push(nextModel);
			break;
		default:
			console.error('Unexpected type heard: ' + typeof value);
		}
	});

	updateError('');

	return {
		name:         recordName,
		recordBody:   recordBody,
		nestedModels: nestedModels
	};
};

const handleArrayField = (value, key, updateError) => {
	// TODO: something
	let insideType = `\t${key} :: ()\n`;
	let nestedModel = false;
	if (value.length > 0) {
		let testValue = value[0];
		switch (typeof testValue) {
		case 'string':
			insideType = `\t${key} :: [Text]\n`;
			break;
		case 'boolean':
			insideType = `\t${key} :: [Bool]\n`;
			break;
		case 'number':
			insideType = `\t${key} :: [Number]\n`;
			break;
		case 'object':
			insideType = `\t${key} :: [${toPascalCase(key)}]\n`;
		    nestedModel = createRecordObject(testValue, key, updateError)
			break;
		}
	}

	return {body: insideType, nested: nestedModel};
};

const upperFirst = (s) => {
	return s.charAt(0).toUpperCase() + s.substr(1);
}

const toPascalCase = (str) => {
	let noSpace = str.split(' ').map(upperFirst).join('');
	let noDash = noSpace.split('-').map(upperFirst).join('');
	let noUnder = noDash.split('_').map(upperFirst).join('');

	return noUnder;
};

const checkForBadWords = (listOfWords) => {
	if (listOfWords.length > 0) {
		for (let i = 0, x = listOfWords.length; i < x; i++) {
			for (let j = 0, y = badWords.length; j < y; j++)  {
				if (listOfWords[i] === badWords[j]) {
					return listOfWords[i];
				}
			}
		}
	}

	return '';
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

const makeFileHeader = (name) => {
	return `{-# LANGUAGE DeriveGeneric #-}

module ${name} where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

${makeDataHeader(name)}`;
};

const makeDataHeader = (name) => {
	return `data ${name} = ${name} {\n`;
};

const makeDataFooter = (name) => {
	return `} deriving (Show, Eq, Generic)\n\ninstance FromJSON ${name}\n`;
};

const renderRecordObj = (recordObj) => {
	let fileHeader = makeFileHeader(recordObj.name);
	let others = recordObj.nestedModels.map((x) => {
		return renderNestedObj(x);
	});

	return `${fileHeader}${recordObj.recordBody.join('')}${makeDataFooter(recordObj.name)}${others}`;
};

const renderNestedObj = (recordObj) => {
	let others = recordObj.nestedModels.map((x) => {
		return renderNestedObj(x);
	});

	return `\n${makeDataHeader(recordObj.name)}${recordObj.recordBody.join('')}${makeDataFooter(recordObj.name)}${others}`;
};

export default SnippetHolder;
