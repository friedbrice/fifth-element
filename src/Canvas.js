"use strict";

exports.setImageSmoothing = function (context) {
  return function (b) {
    return function() {
      context.imageSmoothingEnabled = b;
    };
  };
};
