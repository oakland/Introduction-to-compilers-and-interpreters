# Introduction-to-compilers-and-interpreters
Self learning notes on course ["Introduction to compilers and interpreters"](https://bobzhang.github.io/courses/)

## Install
Run `npm run install` to install dependencies.

## Build
Run `npm run res:build` to build whatever `*.res` files created in `src` directory, then using `node *.bs.js` to execute the result.

For example, I have already created `src/language-implementation/tiny-lan-0/tiny0.res`, after run `npm run res:build` you will find a file named `src/language-implementation/tiny-lan-0/tiny0.bs.js`, run `node src/language-implementation/tiny-lan-0/tiny0.bs.js` you can see the result.
