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
	let targetLen = targetKeys.length;


	let recordBody = [];
	let nestedModels = [];
	let badWordList = [];
	let hasBadWord = false;

	targetKeys.forEach((key, i) => {
		let value = target[key];

		let badWord = checkForBadWords(key);
		if (badWord) {
			key = `${recordName.toLowerCase()}_${key}`;
			hasBadWord = true;
			badWordList.push(key);
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
				let {body, nested} = handleArrayField(value, toPascalCase(key), updateError);

				valType = (body);
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

			valType = `\t${key} :: ${toPascalCase(key)},\n`;
			nestedModels.push(nextModel);
			break;
		default:
			console.error('Unexpected type heard: ' + typeof value);
		}

		if (i == targetLen - 1) {
			valType = valType.replace(/,/g, "");
		}
		recordBody.push(valType);
	});

	updateError('');

	return {
		name:         recordName,
		recordBody:   recordBody,
		nestedModels: nestedModels,
		hadBadWord: hasBadWord,
		badWordList: []
	};
};

const handleArrayField = (value, key, updateError) => {
	// TODO: something
	let insideType = `\t${key} :: (),\n`;
	let nestedModel = false;
	if (value.length > 0) {
		let testValue = value[0];
		switch (typeof testValue) {
		case 'string':
			insideType = `\t${key} :: [Text],\n`;
			break;
		case 'boolean':
			insideType = `\t${key} :: [Bool],\n`;
			break;
		case 'number':
			insideType = `\t${key} :: [Number],\n`;
			break;
		case 'object':
			insideType = `\t${key} :: [${toPascalCase(key)}],\n`;
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

`;
};

const makeDataHeader = (name) => {
	return `data ${name} = ${name} {\n`;
};

const makeDataFooter = (name) => {
	return `} deriving (Show, Eq, Generic)\n\ninstance FromJSON ${name}\n`;
};

// TODO: MAKE WORK:

/* const makeDataBadWordFooter = (name) => { */
/* let x = `} deriving (Show, Eq, Generic)\n\ninstance FromJSON ${name} where\n`; */
/* let y = key.split("_").join("_"); */
/* let z = `$(deriveJSON defaultOptions {fieldLabelModifier = \x -> */
/* if x == "${key}" */
 //                               then "class"
/* else x} ''Asset)` */
//}

const renderRecordObj = (recordObj) => {
	/* let fileHeader = makeFileHeader(recordObj.name); */
	/* if (recordObj.hasBadWord)  { */
		fileHeader += "import Data.Aeson.TH (deriveJSON, defaultOptions, Options(fieldLabelModifier))"
	}

	let others = recordObj.nestedModels.map((x) => {
		return renderNestedObj(x);
	});

	return `${fileHeader}${makeDataHeader(recordObj.name)}${recordObj.recordBody.join('')}${makeDataFooter(recordObj.name)}${others}`;
};

const renderNestedObj = (recordObj) => {
	let others = recordObj.nestedModels.map((x) => {
		return renderNestedObj(x);
	});

	return `\n${makeDataHeader(recordObj.name)}${recordObj.recordBody.join('')}${makeDataFooter(recordObj.name)}${others}`;
};

export default SnippetHolder;
