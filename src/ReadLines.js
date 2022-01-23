"use strict";
var Iterator = require("../Iterator/index.js");

exports.readLinesImpl = function (filename) {
  const lineByLine = require('n-readlines');
  const liner = new lineByLine(filename);
  // let lineNumber = 0;

  var next = function (unit) {
    var self = this;
    return function () {
      // console.log('performing step');
      let line;

      if (line = liner.next()) {
        // console.log('Line ' + lineNumber + ': ' + line.toString('ascii'));
        // lineNumber++;
        return new Iterator.Yield(line.toString('ascii'), self);
      }

      // console.log('end of line reached');
      return new Iterator.Done();
    }
  }

  return new Iterator.IteratorT(next);
}
