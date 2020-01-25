# JSON2Aeson
## A WIP In-Browser Tool for quickly generating Aeson compliant Haskell Records from JSON.

This is a work in progress, but is actually quite usable to generate REST API consumers.
There are several caveats, but it does some nice work to handle renaming reserved words in Haskell, along with `id`.

### Motivation:

Haskell is an incredibly ergonomic language but lacks the practical tools that spring up in even younger languages like Golang. I want an equivilant to something like [this nice little tool](https://mholt.github.io/json-to-go/) for Haskell. [This project](https://github.com/migamake/json-autotype) is likely a better bet for anything bigger than a prototype. With that space in mind, I err on the side of simplicity with the assumption a human will be embelish and edit the output, but I can get a lot of the boilerplate out of the way. I found [my development](https://github.com/krhoda/cria) speed with [Servant](https://github.com/haskell-servant/servant/) improved greatly after creating this tool.

And I learned React Hooks.

### Caveats:
##### This is a website, shouldn't this be hosted?
Yes, but it is terribly ugly and doesn't announce of its shortcomings. Once it is simply ugly but also sincere, I'll host it. Until then, presuming you have [Node and NPM](https://nodejs.org/en/download/) installed, clone, open the directory and:

``` shell
$ npm i
$ npm run start
```

It will attempt to run on port 3000;

##### So what else is wrong with it?
* It only inspects the first elment of an array, so arrays with mixed values do not currently work.
* Similarly arrays of arrays will ignore the intermediate arrays and proclaim to be the inner most type of the first primative encountered (one might say I was over aggressive in my monadic transformations, if one were to obscure the point).
* If two keys with the same name at different levels point to different objects (not primatives, or arrays of primatives), one overwrites the other.
* I have no idea what circular JSON would do, I want to believe that implementation is recursive enough to do it right, provided the input had very modest arrays.
