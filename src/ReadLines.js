"use strict";
var Iterator = require("../Iterator/index.js");

exports.readLinesImpl = function (filename) {
  const lineByLine = require('n-readlines');
  const liner = new lineByLine(filename);

  var step = function (unit) {
    console.log('performing step');
    let line;
    let lineNumber = 0;

    if (line = liner.next()) {
      console.log('Line ' + lineNumber + ': ' + line.toString('ascii'));
      lineNumber++;
      return new Iterator.Yield(line, this);
    }

    console.log('end of line reached');
    return new Iterator.End();
  }

  return new Iterator.Iterator(step);
}
