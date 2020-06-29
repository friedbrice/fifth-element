"use strict";

exports.setImageSmoothing = function (context) {
  return function (b) {
    return function() {
      context.imageSmoothingEnabled = b;
    };
  };
};

exports.unsafeFisherYates = function (xs) {
  var i;
  var j;
  var tmp;
  for (i = xs.length - 1; i >= 1; i--) {
    j = Math.floor(Math.random() * (i + 1));
    tmp = xs[i];
    xs[i] = xs[j];
    xs[j] = tmp;
  }
  return xs;
};
