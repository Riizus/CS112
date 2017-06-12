#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

FileStream extend [
   |bitIndex currentChar|

   parseFile [
      |freqtable|
      freqtable := ZeroArray new: 257.
      [self atEnd not] whileTrue: [
         |ordChar|
         ordChar := self next asInteger.
         freqtable incr: ordChar.
      ].
      freqtable incr: 256.
      ^ freqtable
   ]

   atBitEnd [
      ^ (bitIndex isNil | (bitIndex = 0)) & self atEnd
   ]

]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: BitStream [
   |bitIndex myByte myStream|
   BitStream class >> new [
      self shouldNotImplement.
   ]
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         myByte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^myByte value bitAt: bitIndex
   ]
   putBit: bit [
      bitIndex := bitIndex - 1.
      myByte := myByte bitAt: bitIndex put: bit.
      bitIndex = 1 ifTrue: [
         myStream nextPutByte: myByte.
         self clearByte.
      ].
   ]
   clearByte [
      bitIndex := 9.
      myByte := 0.
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   putString: code [
      code do: [:bit|
         self putBit: bit asInteger.
      ].
   ]
   padOut [
      [bitIndex > 1 & bitIndex < 9] whileFalse: [ 
         self putBit: 0. 
         bitIndex := bitIndex - 1.].
   ]
   myByte [
      ^myByte
   ]
]

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new: aChar [
      |result|
      result := super new.
      result setChar: aChar andCount: 1.
      ^result
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar asCharacter.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]
   decodeTree: myStream [
      myStream putBit: 0.
      (1 to: 8) do: [:index | 
         myStream putBit: ( (self char asInteger) bitAt: index) ].
      char asInteger = 0 ifTrue: [
         myStream putBit: 0.
      ].
      char asInteger = 256 ifTrue: [
         myStream putBit: 1.
      ].
   ]

]

Leaf subclass: Tree [
   |left root right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]
   decodeTree: myStream [
      right decodeTree: myStream.
      left decodeTree: myStream.
      myStream putBit:1.
   ]

]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

inFile := ''.
outFile := stdout.

dFlag := false.
cFlag := false.
tFlag := false.
uFlag := false.

freqTable := ZeroArray new: 257.
codeTable := ZeroArray new: 257.
qew := SortedCollection new.

(1 to: Smalltalk arguments size)
   do: [:index |
      argi := (Smalltalk arguments at: index).
      (argi includes: $-) ifTrue: [
         (argi includes: $d) ifTrue: [dFlag := true.].
         (argi includes: $c) ifTrue: [cFlag := true.].
         (argi includes: $t) ifTrue: [tFlag := true.].
         (argi includes: $u) ifTrue: [uFlag := true.].
      ] ifFalse: [
         (inFile isEmpty) ifTrue: [
            inFile := argi.

         ] ifFalse: [
            outFile :=  argi.
         ]
      ]
   ].

readFile := FileStream open: inFile mode: FileStream read.

uFlag ifFalse: [
   freqTable := readFile parseFile.
   (0 to: freqTable size - 1)
   do: [ :index |
      |leaf count char|
      count := freqTable at: index.
      count isNil ifFalse: [
          char := index asCharacter.
          leaf := Leaf new: char count: count.
          qew add: leaf.
      ].
   ].
].

dFlag ifTrue: [
   'Debug turned on' display.
   nl display.
   tFlag ifTrue: [
      'Decoding tree will be printed' display.
      nl display.
      qew inspect.
   ]. 
   cFlag ifTrue: [
      'Compressing: ' display.
      inFile display.
      (outFile = stdout) ifFalse: [
         ' to file ' display.
         outFile display.
         nl display.
      ] ifTrue: [
         ' to stdout' display.
         nl display.
      ].
      qew inspect.
   ].
   uFlag ifTrue: [
      'Uncompressing: ' display.
      inFile display.
      (outFile = stdout) ifFalse: [
         ' to file ' display.
         outFile display.
         nl display.
      ] ifTrue: [
         ' to stdout' display.
         nl display.
      ]
   ].    
   (0 to: freqTable size - 1)
   do: [:index |
      item := freqTable at: index.
      item isNil ifFalse: [
         index asCharacter visible display.
         ' ' display.
         Character nl = item asCharacter  
            ifTrue: ['nl' display.]
            ifFalse: [item display.].
         nl display.
      ].
   ].
].

uFlag ifFalse: [
   [qew size > 1] whileTrue: [
       |left big newTree treeChar treeCount|
       little := qew removeFirst.
       big := qew removeFirst.
       treeChar := little char.
       treeCount := little count + big count.
       newTree := Tree new: treeChar count: treeCount left: little 
                                                    right: big.
       qew add: newTree.
   ].
].

dFlag ifTrue: [qew inspect.].

uFlag ifFalse: [
   myTree := qew removeLast. 
   myTree inorder: [:char :string |
      codeTable at: char asInteger put: string.
   ] prefix: ''.
].

tFlag ifTrue: [
   (0 to: codeTable size - 1) do: [:index |
      |char count string|
      count := freqTable at: index.
      count isInteger ifTrue: [
         string := codeTable at: index.
         stdout << index asCharacter visible << ' ' << count << ' ' << string << nl.
       ].
   ]. 
   readFile close.
].

cFlag ifTrue: [
   |codeString myStream|
   myStream := BitStream new: outFile.
   readFile reset.
   myTree decodeTree: myStream.
   myStream putBit: 1.
   [readFile atEnd not] whileTrue: [
      |index|
      index := readFile next asInteger.
      codeString := (codeTable at: index).
      myStream putString: codeString.
   ].
   codeString := codeTable at: 256.
   myStream putString: codeString.
   myStream padOut.
   readFile close.
   outFile close.
].

uFlag ifTrue: [
   |inStream outStream myChar myStack myTree tempTree baseTwo done| 
   inStream := BitStream new: readFile.
   outStream := BitStream new: outFile.
   done := false.
   
   myStack := OrderedCollection new.

   inStream atEnd not & done not whileTrue: [
      |flipBit|
      flipBit := inStream nextBit.
      flipBit = 0 ifTrue: [
         myChar := 0.
         baseTwo := 1.
         1 to: 8 do: [
            |myBit|
            myBit := inStream nextBit.
            myChar := myChar + myBit * baseTwo.
            baseTwo := baseTwo * 2.
         ]
         myChar = 0 ifTrue: [
            |myBit|
            myBit := inStream nextBit.
            myChar := myChar + (256 * myBit).
         ]
         myStack addFirst: (Leaf new: myChar).
      ] ifFalse: [
         |left right|
         myStack size > 1 ifTrue: [
            right = myStack removeFirst.
            left = myStack removeFirst.
            myStack addFirst: (Tree new: (left char) count: 0 left: left right: right).
         ] ifFalse: [
            done := true.
            myTree := myStack removeFirst.
         ].
      ].
   ].
   dFlag ifTrue: [myTree inspect.].
   done := false.
   tempTree := myTree.
   inStream atEnd not & done not whileTrue: [
      |flipBit|
      flipBit := inStream nextBit.
      flipBit = 0 ifTrue: [
         tempTree = tempTree left.
      ] ifFalse: [
         tempTree = tempTree right.
      ].
      tempTree count = 1 ifTrue: [
         tempTree char asInteger = 256 ifTrue: [done = true.]
         ifFalse: [
             outStream << tempTree char.
         ].
         tempTree := decodeTree.
      ].
   ].
   readFile close.
   outFile close.
].
