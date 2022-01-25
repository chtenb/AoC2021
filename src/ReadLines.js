"use strict";
var Iterator = require("../Iterator/index.js");
var lineByLine = require('n-readlines');

exports.readLinesImpl = function (filename) {

  var liner = null;

  var next = function (unit) {
    var self = this;

    return function () {
      // Open the file once we are in the Effect monad
      if (liner === null)
        liner = new lineByLine(filename);

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
