/********
* Assembler.scala
* Author: Kevin Rodriguez, Clinton Keen, and Nicholas Hartmann
* Date: Fall 2021, CSC110
*
* Implements a very simple interpreter for an assembler-like language
* called LC-q (a variant of LC-3)
*********/

import scala.io.StdIn._

// The Machine State
var map = Map[String,Int]()
var program: List[List[String]] = List()          // The program itself
var programCounter: Int = 0                       // Where in the program are we?
var conditionCode: Int = 0
val memory: Array[Int] = Array.fill(1024)(0)
val registers: Array[Int] = Array.fill(8)(0)

// Report an error and TERMINATE
def errorExit(msg: String) {
  println("ERROR: " + msg)
  sys.exit(0)  // This terminates the program
}

def setConditionCode(value: Int) = {
if (value > 0) {
  conditionCode = 1
  } else if (value < 0) {
    conditionCode = -1
  } else {
    conditionCode = 0
  }
}

def getRegister(reg: Int, line: List[String]): Int = line(reg)(1) - '0'

def executePrint(line: List[String]) {
  if (line.length != 2) errorExit("INVALID INSTRUCTION PRINT")
  println(registers(getRegister(1,line)))
}

def executeLDI(line: List[String]) {
if (line.length != 3) errorExit("INVALID INSTRUCTION LDI")
registers(getRegister(1, line)) = line(2).toInt
conditionCode = line(2).toInt
}

def executeLDA(line: List[String]) {
  try {
if (line.length != 3) errorExit("INVALID INSTRUCTION LDA")
registers(getRegister(1, line)) =  memory(registers(getRegister(2, line)))
conditionCode = memory(registers(getRegister(2, line)))
} catch {
  case e: Exception => errorExit("MEMORY ADDRESS IS NOT IN RANGE.")
  }
}

def executeStore(line: List[String]) {
  try {
 if (line.length != 3) errorExit("INVALID INSTRUCTION STORE")
 memory(registers(getRegister(2, line))) = registers(getRegister(1, line))
conditionCode = registers(getRegister(1, line))
} catch {
  case e: Exception => errorExit("MEMORY ADDRESS IS NOT IN RANGE.")
  }
}

def executeAdd(line: List[String]) {
  if (line.length != 4) errorExit("INVALID INSTRUCTION ADD")
  registers(getRegister(1, line)) = registers(getRegister(2, line)) + registers(getRegister(3, line))
   conditionCode = registers(getRegister(1, line))
}

def executeMult(line: List[String]) {
  if (line.length != 4) errorExit("INVALID INSTRUCTION MULT")
  registers(getRegister(1, line)) = registers(getRegister(2, line)) * registers(getRegister(3, line))
  conditionCode = registers(getRegister(1, line))
}

def executeXOR(line: List[String]) {
  if (line.length != 4) errorExit("INVALID INSTRUCTION XOR")
  registers(getRegister(1, line)) = registers(getRegister(2, line))^registers(getRegister(3, line))
  conditionCode = registers(getRegister(1, line))
}

def executeAnd(line: List[String]) {
  if (line.length != 4) errorExit("INVALID INSTRUCTION AND")
  registers(getRegister(1, line)) = registers(getRegister(2, line)) & registers(getRegister(3, line))
  conditionCode = registers(getRegister(1, line))
}


def excecuteBranchZero(line: List[String]){
if (!map.contains(line(1))) errorExit("LABEL DOES NOT EXIST.")
if (line.length != 2) errorExit("LABEL DOES NOT EXIST")
    if (conditionCode == 0) {
        programCounter = map(line(1))
    }
}

def excecuteBranchPositive(line: List[String]){
  if (!map.contains(line(1))) errorExit("LABEL DOES NOT EXIST.")
  if (line.length != 2) errorExit("LABEL DOES NOT EXIST")
      if (conditionCode > 0) {
          programCounter = map(line(1))
      }
}

def excecuteBranchNegative(line: List[String]){
  if (!map.contains(line(1))) errorExit("LABEL DOES NOT EXIST.")
  if (line.length != 2) errorExit("LABEL DOES NOT EXIST")
      if (conditionCode < 0) {
          programCounter = map(line(1))
      }
}


def executeJUMP(line: List[String]) {
if (!map.contains(line(1))) errorExit("LABEL DOES NOT EXIST.")
if(line.length != 2) errorExit("LABEL DOES NOT EXIST")
   programCounter = map(line(1))
 }


// Process the current line based on Program Counter
def processCurrentLine() {
  // Check out the line
  val line = program(programCounter)
  line.head match {
    case "PRINT" => executePrint(line)
    case "LDI" => executeLDI(line)
    case "LDA" => executeLDA(line)
    case "STORE" => executeStore(line)
    case "ADD" => executeAdd(line)
    case "MULT" => executeMult(line)
    case "XOR" => executeXOR(line)
    case "AND" => executeAnd(line)
    case ";" =>
    case "LABEL" =>
    case "" =>
   case "BRZ" => excecuteBranchZero(line)
   case "BRN" => excecuteBranchNegative(line)
   case "BRP" => excecuteBranchPositive(line)
    case "JMP" => executeJUMP(line)
    case _     => errorExit("INVALID INSTRUCTION") // Or not yet recognized
  }
  programCounter += 1 // Move on to the next line
}



// Read in the program
def readProgram(): List[List[String]] = {
  val line = readLine()   // Get current line
  if (line == null)
    // End of input reached
    List()
  else {
    // Read rest of Program and attach this line (parsed by spaces) to it
    val restOfProgram = readProgram()  // Read rest of List
      line.toUpperCase.split(" ").toList::restOfProgram
  }
}




// Determine if the program is finished (programCounter is beyond end of program!)
def programFinished(): Boolean =  {
  programCounter == program.length
}

// Main body (load and preprocess the program)
 program = readProgram()


// Identify all of the labels (useful to do this before hand for branching)
var lineNo = 0
for (line <- program) {
  if (line.head == "LABEL") {
    map = map + (line(1) -> lineNo)
  }
  lineNo += 1  // Increment the line number
}


// Keep this!
programCounter = 0
while (!programFinished()) {
  processCurrentLine()
}
