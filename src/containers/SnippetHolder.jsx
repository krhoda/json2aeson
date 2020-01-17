import React from 'react';
import SyntaxHighlighter from 'react-syntax-highlighter';
import {gruvboxDark} from 'react-syntax-highlighter/dist/esm/styles/hljs';

const SnippetHolder = (props) => {
	let {target, instanceName, badWordList}  = props;

	let badWord = badWordList.length > 0;

	if (!target) {
		console.log("Target Obj");
		return '';
	}

	let targetObj = createRecordObject(target, toPascalCase(instanceName));

	if (!targetObj) {
		console.log("No Target Obj");
		return '';
	}

	let fileHeader = makeFileHeader(instanceName, badWord);

	let snippetBody = renderRecordObj(targetObj);

	return (
		<div>
			<SyntaxHighlighter className="snippet" language="haskell" style={gruvboxDark}>
				{fileHeader + snippetBody}
			</SyntaxHighlighter>
		</div>
	);
};

const createRecordObject = (target, recordName) => {
	let targetKeys = Object.keys(target);
	let targetLen = targetKeys.length;

	let recordBody = [];
	let nestedModels = [];
	let hasBadWord = false;
	let badWordList = [];

	for (let i = 0, x = targetLen; i < x; i++) {

		let key = targetKeys[i];
		console.log(target);
		console.log(typeof target);

		console.log(key);
		let value = target[key];

		let badWordCheck = checkForBadWords(key);
		if (badWordCheck) {
			let oldKey = key;
			key = `${recordName.toLowerCase()}_${key}`;

			let nextBadWordEntry = {
				oldWord: oldKey,
				newWord: key
			};

			hasBadWord = true;
			badWordList.push(nextBadWordEntry);
		}

		let valType = '';

		switch (typeof value) {
		case 'string':
			valType = `\t${key} :: Text,\n`;
			break;
		case 'boolean':
			valType = `\t${key} :: Bool,\n`;
			break;
		case 'number':
			valType = `\t${key} :: Number,\n`;
			break;
		case 'object':
			if (Array.isArray(value)) {
				let {body, nested} = handleArrayField(value, toPascalCase(key));

				valType = (body);
				if (nested) {
					nestedModels.push(nested);
				}
				break;
			}

			let nextModelRecord = createRecordObject(value, toPascalCase(key));

			valType = `\t${key} :: ${toPascalCase(key)},\n`;
			nestedModels.push(nextModelRecord);
			break;
		default:
			console.error('Unexpected type heard: ' + typeof value);
		}

		if (i == targetLen - 1) {
			valType = valType.replace(/,/g, '');
		}
		recordBody.push(valType);
	};

	return {
		name:         recordName,
		recordBody:   recordBody,
		nestedModels: nestedModels,
		hasBadWord:   hasBadWord,
		badWordList:  badWordList
	};
};

const handleArrayField = (value, key) => {
	// TODO: something
	let insideType = `\t${key} :: (),\n`;
	let nestedModel = false;
	if (value.length > 0) {
		let testValue = value[0];
		switch (typeof testValue) {
		case 'string':
			insideType = `\t${key.toLowerCase()} :: [Text],\n`;
			break;
		case 'boolean':
			insideType = `\t${key.toLowerCase()} :: [Bool],\n`;
			break;
		case 'number':
			insideType = `\t${key.toLowerCase()} :: [Number],\n`;
			break;
		case 'object':
			insideType = `\t${key.toLowerCase()} :: [${toPascalCase(key)}],\n`;
		    nestedModel = createRecordObject(testValue, key);
			break;
		}
	}

	return {body: insideType, nested: nestedModel};
};

const upperFirst = (s) => {
	return s.charAt(0).toUpperCase() + s.substr(1);
};

const toPascalCase = (str) => {
	let noSpace = str.split(' ').map(upperFirst).join('');
	let noDash = noSpace.split('-').map(upperFirst).join('');
	let noUnder = noDash.split('_').map(upperFirst).join('');

	return noUnder;
};


const makeFileHeader = (name, badWord) => {
	let extensions = '{-# LANGUAGE DeriveGeneric #-}';
	let extraAeson = '';

	if (badWord) {
		extensions += '\n{-# LANGUAGE TemplateHaskell #-}';
		extraAeson = '\nimport Data.Aeson.TH (deriveJSON, defaultOptions, Options(fieldLabelModifier))\n';
	}

	return `${extensions}

module ${name} where

import Data.Aeson${extraAeson}
import Data.Text (Text)

import GHC.Generics (Generic)

`;
};

const makeDataHeader = (name) => {
	return `data ${name} = ${name} {\n`;
};

const makeDataFooter = (recordObj) => {
	let prefix = '} deriving (Show, Eq, Generic)\n\n';

	if (recordObj.hasBadWord) {
		 console.log('HERE:');
		 let cases = recordObj.badWordList.map(makeTemplateFooter).join('\n');
		 return `${prefix}$(deriveJSON defaultOptions {fieldLabelModifier = \\x ->\n\tcase x of\n${cases}\n\t_ -> x} ''${recordObj.name})\n\n`;
	}

	return `${prefix}instance FromJSON ${recordObj.name}\n\n`;
};

const makeTemplateFooter = (badWordEntry) => {
	return `\t"${badWordEntry.newWord}" -> "${badWordEntry.oldWord}"`;
};

const renderRecordObj = (recordObj) => {
	let others = recordObj.nestedModels.map((x) => {
		return renderRecordObj(x);
	});

	return `${others}${makeDataHeader(recordObj.name)}${recordObj.recordBody.join('')}${makeDataFooter(recordObj)}`;
};

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

export default SnippetHolder;
