// import * as fs from 'node:fs'
// import { Buffer } from 'node:buffer'
const fs = require('node:fs')
const { Buffer } = require('node:buffer')

const EBCDIC_MAP = buildEBCDICMap()

function buildEBCDICMap() {
  const map = new Map()

  map.set(0x4B, '.')
  map.set(0x4E, '+')

  map.set(0x5B, '$')

  map.set(0x6B, ',')
  map.set(0x6C, '%')
  map.set(0x6D, '_')

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

  map.set(0xa1, '~')
  map.set(0xa2, 's')
  map.set(0xa3, 't')
  map.set(0xa4, 'u')
  map.set(0xa5, 'v')
  map.set(0xa6, 'w')
  map.set(0xa7, 'x')
  map.set(0xa8, 'y')
  map.set(0xa9, 'z')

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
  return buffer.map(decodeEBCDICByte).join("")
}

function decodeEBCDICByte(byte) {
  const c = EBCDIC_MAP.get(byte)
  if (c == undefined) {
    throw new Error("Unable to decode byte: " + byte.toString())
  } else {
    return c
  }
}

function FileDescriptor(fileName) {

}

let printLine = new FileDescriptor("PRTLINE")
let acctRec = new FileDescriptor("ACCTREC")

function initFileDesc(fileParser, varSpec) {

}

let printRec = initFileDesc(printLine, [
  { name: "acctNo0", length: 8, type: "string" },
  { name: "acctLimit0", length: 13, type: "string" },
  { name: "acctBalance0", length: 13, type: "string" },
  { name: "lastName0", length: 20, type: "string" },
  { name: "firstName0", length: 15, type: "string" },
  { name: "comments0", length: 50, type: "string" }
])

let acctFields = initFileDesc(acctRec, [
  { name: "acctNo", length: 8, type: "string" },
  { name: "acctLimit", length: 11, type: "binary-decimal" },
  { name: "acctBalance", length: 11, type: "binary-decimal" },
  { name: "lastName", length: 20, type: "string" },
  { name: "firstName", length: 15, type: "string" },
  { 
    name: "clientAddr", 
    children: [
      { name: "streetAddr", length: 25, type: "string" },
      { name: "cityCounty", length: 20, type: "string" },
      { name: "usaState", length: 15, type: "string" }
    ]
  },
  { name: "comments0", length: 50, type: "string" }
])

function concatArrays(arrs) {
  return arrs.reduce((res, arr) => res.concat(arr), [])
}

// Read Text

function readText(fd, length) {
  let b = Buffer.alloc(length)
  fs.readSync(fd, b, 0, length)
  return decodeEBCDICBuffer(b)
}

// Read Packed Decimals

function readPackedDecimal(fd, length, wholePartLength) {
  let b = Buffer.alloc(length)
  fs.readSync(fd, b, length)
  let ds = decodePackedDecimalDigits(b)
  return decimalFromPackedDigits(ds, wholePartLength)
}

// http://www.3480-3590-data-conversion.com/article-packed-fields.html
function decimalFromPackedDigits(digits, wholePartLength) {
  let numDigitsLength = digits.length - 1  // last digit is actually the 'sign'

  if (wholePartLength > (digits.length - 1)) {
    throw new Error('Whole Part of packed decimal is longer than provided digits')
  } else if (wholePartLength == digits.length) {
    return parseInt(digits.join(''))
  } 

  let wholePartStr = digits.slice(0, wholePartLength).join('')
  let decPartStr = digits.slice(wholePartLength, numDigitsLength).join('')
  let sign = wholePartLength == 0xD ? '-' : ''

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

let fd = fs.openSync("data")

console.log("Text 1:", readText(fd, 8))
console.log("Dec 1: ", readPackedDecimal(fd, 5, 7))

// console.log(decimalFromDigits([1,2,3]))

fs.closeSync(fd)