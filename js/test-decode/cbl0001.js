// import * as fs from 'node:fs'
// import { Buffer } from 'node:buffer'
const fs = require('node:fs')
const { Buffer } = require('node:buffer')

const READ_EOF = "READ-EOF"

// File Descriptors

function FileDescriptor(fileName) {
  this.fileName = fileName
  this.fd = undefined
  this.varSpec = undefined

  this.open = function (flags) {
    this.fd = fs.openSync(this.fileName, flags)
  }

  this.close = function () {
    fs.closeSync(this.fd)
  }

  this.read = function (atEnd) {
    try {
      this.data = readVarSpec(this.fd, this.varSpec)
    } catch (e) {
      if (e.message === READ_EOF) {
        atEnd()
      }
    }
  }

  this.write = function () {
    writeVarSpec(this.fd, this.varSpec, this.data)
  }

  this.loadVarSpec = function (varSpec) {
    this.varSpec = varSpec
    this.data = newObjFromVarSpec(varSpec)
  }
}

function newObjFromVarSpec(varSpec) {
  let obj = {}
  for (spec of varSpec) {
    obj[spec.name] = newObjFromVarSpecItem(spec)
  }
  return obj
}

function newObjFromVarSpecItem(specItem) {
  if (specItem.type === 'string') {
    return ''
  } else if (specItem.type === 'binary-decimal') {
    return 0
  } else if (specItem.type === 'compound') {
    return newObjFromVarSpec(specItem.children)
  } else {
    throw new Error("Unrecognized var spec: " + specItem.type)
  }
}

function readVarSpec(fd, varSpec) {
  let obj = {}
  for (spec of varSpec) {
    obj[spec.name] = readVarSpecItem(fd, spec)
  }
  return obj
}

function readVarSpecItem(fd, specItem) {
  if (specItem.type === 'string') {
    return readText(fd, specItem.length)
  } else if (specItem.type === 'binary-decimal') {
    return readPackedDecimal(fd, specItem.length, specItem.wholeDigits)
  } else if (specItem.type === 'compound') {
    return readVarSpec(fd, specItem.children)
  } else {
    throw new Error("Unrecognized var spec: " + specItem.type)
  }
}

function writeVarSpec(fd, varSpec, data) {
  for (spec of varSpec) {
    writeVarSpecItem(fd, spec, data[spec.name])
  }
}

function writeVarSpecItem(fd, specItem, data) {
  if (specItem.type === 'string') {
    return writeText(fd, specItem.length, data.toString())
  } else if (specItem.type === 'compound') {
    return writeVarSpec(fd, specItem.children, data)
  } else {
    throw new Error("Unrecognized var spec: " + specItem.type)
  }
}

function writeText(fd, length, text) {
  fs.writeSync(fd, leftPad(length, text))
}

function leftPad(len, str) {
  if (len <= str.length) {
    return str
  }

  return ' '.repeat(len - str.length) + str
}

// Read Text

function readText(fd, length) {
  return decodeEBCDICBuffer(_read(fd, length))
}

const EBCDIC_MAP = buildEBCDICMap()

function buildEBCDICMap() {
  const map = new Map()

  map.set(0x40, ' ')
  map.set(0x4B, '.')
  map.set(0x4E, '+')

  map.set(0x5B, '$')

  map.set(0x60, '-')
  map.set(0x6B, ',')
  map.set(0x6C, '%')
  map.set(0x6D, '_')

  map.set(0x7D, '\'')
  map.set(0x7E, '=')
  map.set(0x7F, '"')

  map.set(0x81, 'a')
  map.set(0x82, 'b')
  map.set(0x83, 'c')
  map.set(0x84, 'd')
  map.set(0x85, 'e')
  map.set(0x86, 'f')
  map.set(0x87, 'g')
  map.set(0x88, 'h')
  map.set(0x89, 'i')

  map.set(0x91, 'j')
  map.set(0x92, 'k')
  map.set(0x93, 'l')
  map.set(0x94, 'm')
  map.set(0x95, 'n')
  map.set(0x96, 'o')
  map.set(0x97, 'p')
  map.set(0x98, 'q')
  map.set(0x99, 'r')

  map.set(0xA1, '~')
  map.set(0xA2, 's')
  map.set(0xA3, 't')
  map.set(0xA4, 'u')
  map.set(0xA5, 'v')
  map.set(0xA6, 'w')
  map.set(0xA7, 'x')
  map.set(0xA8, 'y')
  map.set(0xA9, 'z')

  map.set(0xC0, '{')
  map.set(0xC1, 'A')
  map.set(0xC2, 'B')
  map.set(0xC3, 'C')
  map.set(0xC4, 'D')
  map.set(0xC5, 'E')
  map.set(0xC6, 'F')
  map.set(0xC7, 'G')
  map.set(0xC8, 'H')
  map.set(0xC9, 'I')

  map.set(0xD0, '}')
  map.set(0xD1, 'J')
  map.set(0xD2, 'K')
  map.set(0xD3, 'L')
  map.set(0xD4, 'M')
  map.set(0xD5, 'N')
  map.set(0xD6, 'O')
  map.set(0xD7, 'P')
  map.set(0xD8, 'Q')
  map.set(0xD9, 'R')
                 
  map.set(0xE0, '\\')
  map.set(0xE2, 'S')
  map.set(0xE3, 'T')
  map.set(0xE4, 'U')
  map.set(0xE5, 'V')
  map.set(0xE6, 'W')
  map.set(0xE7, 'X')
  map.set(0xE8, 'Y')
  map.set(0xE9, 'Z')

  map.set(0xF0, '0')
  map.set(0xF1, '1')
  map.set(0xF2, '2')
  map.set(0xF3, '3')
  map.set(0xF4, '4')
  map.set(0xF5, '5')
  map.set(0xF6, '6')
  map.set(0xF7, '7')
  map.set(0xF8, '8')
  map.set(0xF9, '9')

  return map
}

function decodeEBCDICBuffer(buffer) {
  return [...buffer].map(decodeEBCDICByte).join("")
}

function decodeEBCDICByte(byte) {
  const c = EBCDIC_MAP.get(byte)
  if (c == undefined) {
    console.error("Unable to decode byte: " + byte.toString(16))
    return '?'
  } else {
    return c
  }
}

// Read Packed Decimals

function readPackedDecimal(fd, length, wholeDigits) {
  let ds = decodePackedDecimalDigits(_read(fd, length))
  return decimalFromPackedDigits(ds, wholeDigits)
}

// http://www.3480-3590-data-conversion.com/article-packed-fields.html
function decimalFromPackedDigits(digits, wholeDigits) {
  let numDigitsLength = digits.length - 1  // last digit is actually the 'sign'

  if (wholeDigits > (digits.length - 1)) {
    throw new Error('Whole Part of packed decimal is longer than provided digits')
  } else if (wholeDigits == digits.length) {
    return parseInt(digits.join(''))
  } 

  let wholePartStr = digits.slice(0, wholeDigits).join('')
  let decPartStr = digits.slice(wholeDigits, numDigitsLength).join('')
  let sign = wholeDigits == 0xD ? '-' : ''

  let numStr = sign + wholePartStr + '.' + decPartStr

  return parseFloat(numStr)
}

function decodePackedDecimalDigits(packedNums) {
  let res = []
  for (let i = 0; i < packedNums.length; i++) {
    res.push(...decodePackedDecimalDigitPair(packedNums[i]))
  }
  return res
}

function decodePackedDecimalDigitPair(packedNum) {
  const first = packedNum >> 4
  const second = packedNum & 0x0F
  // console.log("Packed:", first, second)
  return [first, second]
}

/**
 * @returns Buffer containing the read content
 */
function _read(fd, length) {
  let b = Buffer.alloc(length)
  const bytesRead = fs.readSync(fd, b, 0, length)
  if (bytesRead === 0) {
    throw new Error("READ-EOF")
  } else if (bytesRead < length) {
    throw new Error("ERROR: Asked to read " + length + " bytes but only read " + bytesRead)
  }
  return b
}

// Program Executor

function _runProcedures(procedures) {
  for (proc of procedures) {
    let procRes = proc()

    if (procRes === 'GOBACK') {
      console.log("TRACE asked to 'go back'")
      break;
    }
  }
}

// BEGIN PROGRAM

// ENVIRONMENT DIVISION

let printLine = new FileDescriptor("js/test-decode/PRTLINE")
let acctRec = new FileDescriptor("js/test-decode/ACCTREC")

// DATA DIVISION

printLine.loadVarSpec([
  {
    name: "printRec",
    type: "compound",
    children: [
      { name: "acctNoO" , length: 8, type: "string" },
      { name: "acctLimitO" , length: 13, type: "string" },
      { name: "acctBalanceO" , length: 13, type: "string" },
      { name: "lastNameO" , length: 20, type: "string" },
      { name: "firstNameO" , length: 15, type: "string" },
      { name: "commentsO" , length: 50, type: "string" }
    ]
  }
])

acctRec.loadVarSpec([
  { 
    name: "acctFields",
    type: "compound",
    children: [
      {  name: "acctNo", type: "string", length: 8 },
      {  name: "acctLimit", type: "binary-decimal", length: 5, wholeDigits: 7 },
      {  name: "acctBalance", type: "binary-decimal", length: 5, wholeDigits: 7 },
      {  name: "lastName", type: "string", length: 20 },
      {  name: "firstName", type: "string", length: 15 },
      {
        name: "clientAddr",
        type: "compound",
        children: [
          {  name: "streetAddr", length: 25, type: "string" },
          {  name: "cityCounty", length: 20, type: "string" },
          {  name: "usaState", length: 15, type: "string" }
        ]
      }, 
      { name: "reserved", length: 7, type: "string" }, 
      { name: "comments", length: 50, type: "string" }
    ]
  }
])

let flags = { lastRec: ' ' }

// PROCEDURE DIVISION

function openFiles() {
  console.log("TRACE openFiles")
  acctRec.open('r')
  printLine.open('a')
}

function readNextRecord() {
  console.log("TRACE readNextRecord")
  readRecord()
  while (!(flags.lastRec == 'Y')) {
    writeRecord()
    readRecord()
  }
}

function closeStop() {
  console.log("TRACE closeStop")
  acctRec.close()
  printLine.close()
  return 'GOBACK'
}

function readRecord() {
  console.log("TRACE readRecord")
  acctRec.read(function () {
    flags.lastRec = 'Y'
  })
}

function writeRecord() {
  console.log("TRACE writeRecord")
  printLine.data.printRec.acctNoO      = acctRec.data.acctFields.acctNo
  printLine.data.printRec.acctLimitO   = acctRec.data.acctFields.acctLimit
  printLine.data.printRec.acctBalanceO = acctRec.data.acctFields.acctBalance
  printLine.data.printRec.lastNameO    = acctRec.data.acctFields.lastName
  printLine.data.printRec.firstNameO   = acctRec.data.acctFields.firstName
  printLine.data.printRec.commentsO    = acctRec.data.acctFields.comments
  printLine.write()
}

// PROGRAM EXECUTION

const __PROCEDURES__ = [
  openFiles,
  readNextRecord,
  closeStop,
  readRecord,
  writeRecord
]

_runProcedures(__PROCEDURES__)