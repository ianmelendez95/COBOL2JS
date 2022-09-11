let printLine = new FileDescriptor(process.env["PRTLINE"])
let acctRec = new FileDescriptor(process.env["ACCTREC"])
printLine.loadVarSpec({
  children: [
    {
      length: 8,
      name: "acctNoO",
      type: "string",
    },
    {
      length: 0,
      name: "acctLimitO",
      type: "string",
    },
    {
      length: 0,
      name: "acctBalanceO",
      type: "string",
    },
    {
      length: 20,
      name: "lastNameO",
      type: "string",
    },
    {
      length: 15,
      name: "firstNameO",
      type: "string",
    },
    {
      length: 50,
      name: "commentsO",
      type: "string",
    },
  ],
  name: "printRec",
  type: "compound",
})
acctRec.loadVarSpec({
  children: [
    {
      length: 8,
      name: "acctNo",
      type: "string",
    },
    {
      length: 5,
      name: "acctLimit",
      type: "binary-decimal",
      wholeDigits: 7,
    },
    {
      length: 5,
      name: "acctBalance",
      type: "binary-decimal",
      wholeDigits: 7,
    },
    {
      length: 20,
      name: "lastName",
      type: "string",
    },
    {
      length: 15,
      name: "firstName",
      type: "string",
    },
    {
      children: [
        {
          length: 25,
          name: "streetAddr",
          type: "string",
        },
        {
          length: 20,
          name: "cityCounty",
          type: "string",
        },
        {
          length: 15,
          name: "usaState",
          type: "string",
        },
      ],
      name: "clientAddr",
      type: "compound",
    },
    {
      length: 7,
      name: "reserved",
      type: "string",
    },
    {
      length: 50,
      name: "comments",
      type: "string",
    },
  ],
  name: "acctFields",
  type: "compound",
})
function openFiles() {
  acctRec.open("r")
  printLine.open("w")
}
function readNextRecord() {
  readRecord()
  while (!((lastrec = "Y"))) {
    writeRecord()
    readRecord()
  }
}
function closeStop() {
  acctRec.close()
  printLine.close()
  return "GOBACK"
}
function readRecord() {
  acctRec.read(function () {
    lastrec = "Y"
  })
}
function writeRecord() {
  acctNoO = acctNo
  acctLimitO = acctLimit
  acctBalanceO = acctBalance
  lastNameO = lastName
  firstNameO = firstName
  commentsO = comments
  printRec.write()
}
const __PROCEDURES__ = [
  openFiles,
  readNextRecord,
  closeStop,
  readRecord,
  writeRecord,
]
_runProcedures(__PROCEDURES__)