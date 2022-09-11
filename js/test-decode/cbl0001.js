const fs = require('node:fs')

eval(fs.readFileSync('js/runtime.js', {
  encoding: 'utf-8'
}))

// BEGIN PROGRAM

// ENVIRONMENT DIVISION

let printLine = new FileDescriptor(process.env["PRTLINE"])
let acctRec = new FileDescriptor(process.env["ACCTREC"])

// DATA DIVISION

printLine._loadVarSpec({
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
})

acctRec._loadVarSpec({ 
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
})

let flags = _valueFromVarSpec({
  type: "compound",
  name: "flags",
  children: [
    {
      type: "string",
      name: "lastrec",
      length: 1,
      initialValue: ' '
    }
  ]
})

// PROCEDURE DIVISION

function openFiles() {
  console.log("TRACE openFiles")
  acctRec._open('r')
  printLine._open('w')
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
  acctRec._close()
  printLine._close()
  return 'GOBACK'
}

function readRecord() {
  console.log("TRACE readRecord")
  acctRec._read(function () {
    flags.lastRec = 'Y'
  })
}

function writeRecord() {
  console.log("TRACE writeRecord")
  printLine.printRec.acctNoO      = acctRec.acctFields.acctNo
  printLine.printRec.acctLimitO   = acctRec.acctFields.acctLimit
  printLine.printRec.acctBalanceO = acctRec.acctFields.acctBalance
  printLine.printRec.lastNameO    = acctRec.acctFields.lastName
  printLine.printRec.firstNameO   = acctRec.acctFields.firstName
  printLine.printRec.commentsO    = acctRec.acctFields.comments
  printLine._write()
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