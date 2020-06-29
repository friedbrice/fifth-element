'use strict';

exports.undefined = undefined;

exports.debug = function (msg) {
  return function (x) {
    console.log(msg);
    return x;
  };
};
