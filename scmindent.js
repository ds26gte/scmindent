#!/usr/bin/env node

// Dorai Sitaram
// last modified 2014-09-18

// this script takes lines of Lisp or Scheme code from its
// stdin and produces an indented version thereof on its
// stdout

var fs = require('fs');
var events = require('events');

var tbd = new events.EventEmitter();
var lispKeywords = {};

function numLeadingSpaces(s) {
  if (s.match(/^\s*$/)) {
    return 0;
  } else {
    s = s.replace(/^(\s*).*/, '$1');
    s = s.replace(/\t/g, '        ');
    return s.length;
  }
}

function stringTrimBlanks(s) {
  s = s.replace(/^\s*/, '');
  s = s.replace(/\s*$/, '');
  return s;
}

function literalTokenP(s) {
  return !!s.match(/^[0-9#"]/);
}

function lispIndentNumber(s) {
  var n;
  s = s.toLowerCase();
  if (n = lispKeywords[s]) {
    return n;
  } else if (s.match(/^def/)) {
    return 0;
  } else {
    return -1;
  }
}

function pastNextToken(s, i) {
  var n = s.length;
  var escapeP = false;
  while (true) {
    if (i >= n) {
      return n;
    } else {
      var c = s[i];
      if (escapeP) {
        escapeP = false; i++;
      } else if (c === '\\') {
        escapeP = true; i++;
      } else if (c === '#') {
        if (s[i+1] === '\\') {
          escapeP = true; i = i+2;
        } else {
          return i;
        }
      } else if (c.match(/[ \t\(\)\[\]"'`,;]/)) {
        return i;
      } else {
        escapeP = false; i++;
      }
    }
  }
}

function calcSubindent(s, i) {
  var n = s.length;
  var j = pastNextToken(s, i);
  var numAlignedSubforms = 0;
  var leftIndent;
  if (j === i) {
    leftIndent = 1;
  } else {
    var w = s.substring(i, j);
    if (i >= 2 && s[i-2].match(/['`]/)) {
      leftIndent = 1;
    } else {
      var nas = lispIndentNumber(w);
      if (nas >= 0) {
        numAlignedSubforms = nas;
        leftIndent = 2;
      } else if (literalTokenP(w)) {
        leftIndent = 1;
      } else if (j === n) {
        leftIndent = 1;
      } else {
        leftIndent = j - i + 2;
      }
    }
  }
  return {
    leftIndent: leftIndent,
    numAlignedSubforms: numAlignedSubforms,
    nextTokenIndex: j
  }
}

var leftI = 0;
var parenStack = [];
var stringP = false;

function indentLine(currLine) {
  var leadingSpaces = numLeadingSpaces(currLine);
  var currLeftI;
  if (stringP) {
    currLeftI = leadingSpaces;
  } else if (parenStack.length === 0) {
    if (leftI === 0) { leftI = leadingSpaces; }
    currLeftI = leftI;
  } else {
    currLeftI = parenStack[0].spacesBefore;
    var extraW = 0;
    if (parenStack[0].numFinishedSubforms < parenStack[0].numAlignedSubforms) {
      parenStack[0].numFinishedSubforms++;
      currLeftI += 2;
    }
  }
  currLine = stringTrimBlanks(currLine);
  for (var i = 0; i < currLeftI; i++) {
    process.stdout.write(' ');
  }
  process.stdout.write(currLine);
  process.stdout.write('\n');

  var escapeP = false;
  var interWordSpaceP = false;
  var i = 0;
  while (i < currLine.length) {
    var c = currLine[i];
    if (escapeP) {
      escapeP = false; i++;
    } else if (c === '\\') {
      escapeP = true; i++;
    } else if (stringP) {
      if (c === '"') { stringP = false; } i++;
    } else if (c === ';') {
      break;
    } else if (c === '"') {
      stringP = true; i++;
    } else if (c.match(/[ \t]/)) {
      if (!interWordSpaceP) {
        interWordSpaceP = true;
        if (parenStack.length > 0) {
          parenStack[0].numFinishedSubforms++;
        }
      }
      i++;
    } else if (c.match(/[\(\[]/)) {
      interWordSpaceP = false;
      var si = calcSubindent(currLine, i+1);
      parenStack.unshift({ spacesBefore: i + currLeftI + si.leftIndent,
        numAlignedSubforms: si.numAlignedSubforms,
        numFinishedSubforms: 0
      });
      i = si.nextTokenIndex;
    } else if (c.match(/[\)\]]/)) {
      interWordSpaceP = false;
      if (parenStack.length > 0) {
        parenStack.shift();
        leftI = 0;
      }
      i++;
    } else {
      interWordSpaceP = false; i++;
    }
  }
}

function indentLines() {
  process.stdin.setEncoding('utf8');
  var prevLine;
  process.stdin.on('data', function(data) {
    var lines = data.split('\n');
    if (lines.length) {
      if (prevLine) {
        lines[0] = prevLine + lines[0];
      }
      for (var i = 0; i < lines.length-1; i++) {
        tbd.emit('indentLine', lines[i]);
      }
      prevLine = lines[lines.length-1];
    }
  });
  process.stdin.on('end', function() {
    if (prevLine) {
      tbd.emit('indentLine', prevLine);
      prevLine = '';
    }
  });
}

function customize() {
  fs.readFile(process.env.HOME + '/lispwords.json', 'utf8',
      function(err, data) {
        if (!err) {
          var lw = JSON.parse(data);
          for (var kw in lw) {
            lispKeywords[kw] = lw[kw];
          }
        } else {
          process.stderr.write('~/lispwords.json missing or ill-formed\n');
        }
        tbd.emit('customizationDone');
      });
}

tbd.on('start', customize);
tbd.on('customizationDone', indentLines);
tbd.on('indentLine', indentLine);

tbd.emit('start');
