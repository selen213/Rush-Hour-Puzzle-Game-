Object subclass: #Monom
	instanceVariableNames: 'exp coef'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OOP1'!

!Monom methodsFor: 'get' stamp: 'Selen 11/28/2024 18:14'!
"Returns the coefficient of the monomial."
coef
	^coef! !

!Monom methodsFor: 'get' stamp: 'Selen 11/28/2024 18:14'!
"Returns the exponent of the monomial."
exp
	^exp! !

!Monom methodsFor: 'mathOperations' stamp: 'Selen 11/29/2024 13:29'!
"Adds another monomial to the current monomial.
 Only works if both monomials have the same exponent."
addMon: aMonom
	|tmpCoef|
	( (self exp) = (aMonom exp) )
	ifTrue:[
		tmpCoef := (self coef) + (aMonom coef).
		self coef: tmpCoef.
		^self
	]
	ifFalse:[
		^self
	]! !

!Monom methodsFor: 'mathOperations' stamp: 'Selen 11/29/2024 13:22'!
"Calculates the derivative of the monomial."
derivative
	| res tmpExp tmpCoef |
	(( self exp ) = 0 )
	ifTrue:[ 
		"Constant monomial, derivative is zero."
		res := Monom new.
		^res
	]
	ifFalse:[
		"Apply the power rule: d(a*x^n)/dx = a*n*x^(n-1)"
		tmpCoef := (self coef)*(self exp).
		tmpExp := (self exp) - 1.
		res := Monom new.
		res exp: tmpExp; coef: tmpCoef.
		^res
	]! !

!Monom methodsFor: 'mathOperations' stamp: 'Selen 11/29/2024 13:44'!
"Evaluates the monomial at a given integer value."
evalAt: anInteger
	|res|
	res := 0.
	res := anInteger raisedToInteger: (self exp).
	res := res * (self coef).
	^res! !

!Monom methodsFor: 'mathOperations' stamp: 'Selen 11/29/2024 14:08'!
"Multiplies the current monomial with another monomial."
multMon: aMonom
	|tmpExp tmpCoef|
	((self coef = 0) and: [self exp = 0])
	ifTrue:[
		^self
	].
	((aMonom coef = 0) and: [aMonom exp = 0])
	ifTrue:[
		self exp: 0.
		self coef: 0.
		^self
	].
	"Multiply coefficients and add exponents: ax^n * bx^m = abx^(n+m)"
	tmpExp := (self exp)+ (aMonom exp).
	tmpCoef := (self coef)*(aMonom coef).
	self exp: tmpExp.
	self coef: tmpCoef.
	^self! !

!Monom methodsFor: 'comparing' stamp: 'Selen 11/29/2024 13:20'!
"Checks equality based on the exponent."
= anObject
	^ ((self exp) = (anObject exp))! !

!Monom methodsFor: 'comparing' stamp: 'Selen 11/29/2024 13:24'!
"Generates a hash value based on the exponent."
hash
	^ (self exp) hash! !

!Monom methodsFor: 'set' stamp: 'Selen 11/29/2024 13:44'!
"Sets the coefficient of the monomial.
 Validates that input is an integer. If coefficient is zero, sets the exponent to zero as well."
coef: anInteger
	(anInteger isKindOf: Integer )
	ifTrue: [ 
		(anInteger = 0)
		ifTrue:[
			exp := 0.
			coef := anInteger
		]
		ifFalse: [
			coef := anInteger
		]
	]
	ifFalse: [
		self error: 'invalid input'
	]! !

!Monom methodsFor: 'set' stamp: 'Selen 11/29/2024 14:50'!
"Sets the exponent of the monomial.
 Validates that input is a non-negative integer."
exp: anInteger
	( ( anInteger isKindOf: Integer ) and: [ anInteger >= 0] )
	ifTrue: [ 
		exp := anInteger
	]
	ifFalse: [
		self error: 'invalid input'
	]! !

!Monom methodsFor: 'initialize-release' stamp: 'Selen 11/29/2024 23:55'!
"Initializes the monomial to the zero polynomial (0*x^0)."
initialize
	exp := 0.
	coef := 0! !





Object subclass: #PolyStream
	instanceVariableNames: 'block'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OOP1'!

!PolyStream methodsFor: 'additional' stamp: 'Selen 12/1/2024 21:12'!
"Adds another PolyStream to the current one by combining their blocks."
add: aPolyStream
	|selfPrevBlock aPolyStreamBlock|
	(aPolyStream isKindOf: PolyStream)
	ifTrue:[
		selfPrevBlock := self block.
		aPolyStreamBlock := aPolyStream block.
		block := [:x :expSet | (selfPrevBlock value:x value: expSet) + (aPolyStreamBlock value:x value: expSet)].
	]
	ifFalse:[
		self error: 'invalid input'.
	]! !

!PolyStream methodsFor: 'additional' stamp: 'Selen 12/1/2024 23:31'!
"Adds a term to the polynomial defined by the coefficient and exponent."
addCoef: coef withExp: exp
    | tmp a b |
    ((coef isKindOf: Integer) and: [ exp isKindOf: Integer ])
    ifTrue: [
        (exp >= 0)
        ifTrue: [
            tmp := block.
            block := [:x :expSet |
                (expSet includes: exp)
                ifTrue: [
                    a := (tmp value: x value: expSet).
                    a]
                ifFalse: [
                    a := (tmp value: x value: expSet).
                    b := ((x raisedToInteger: exp) * coef).
                    a + b
                ]
            ].
        ]
        ifFalse: [
            self error: 'invalid input'.
        ]
    ]
    ifFalse: [
        self error: 'invalid input'.
    ]! !


!PolyStream methodsFor: 'additional' stamp: 'Selen 12/1/2024 22:26'!
"Evaluates the polynomial for a given integer input."
eval: anInteger
	|tempSet res|
	res := 0.
	(anInteger isKindOf: Integer)
	ifTrue:[
		tempSet := Set new.
		res := block value: anInteger value: tempSet.
		^res
	]
	ifFalse:[
		self error: 'invalid input'.
	]! !

!PolyStream methodsFor: 'additional' stamp: 'Selen 12/1/2024 23:30'!
"Filters the polynomial by restricting terms to those with exponents in the given set."

filter: aSet

    | prevblock setCopy |

    "Create a copy of the input set"
    setCopy := aSet copy.

    "Save the current block to preserve its behavior"
    prevblock := block.

    "Redefine the block to extend its functionality"
    block := [:x :expSet |

        | expSetCopy |

        "Create a new set to combine elements"
        expSetCopy := Set new.

        "Add elements from the copied input set"
        expSetCopy addAll: setCopy.

        "Add elements from the copied polyset"
        expSetCopy addAll: (expSet copy).

        "Invoke the previous block with updated arguments"
        prevblock value: x value: expSetCopy.
    ].
! !


!PolyStream methodsFor: 'additional' stamp: 'Selen 12/1/2024 20:51'!
"Multiplies the polynomial by a given integer."
multiplyBy: anInteger
	|tmp|
	(anInteger isKindOf: Integer)
	ifTrue:[
		"Replaces the block to scale the polynomial by the given integer."
		tmp := block.
		block := [:x :expSet | (anInteger * (tmp value: x value: expSet))].
	]
	ifFalse:[
		self error: 'invalid input'.
	]! !

!PolyStream methodsFor: 'additional' stamp: 'Selen 12/1/2024 20:59'!
"Substitutes the variable x in the polynomial with x * anInteger."
substitute: anInteger
	|tmp|
	((anInteger isKindOf: Integer) and: [ ((anInteger = 0) not)])
	ifTrue:[
		"Adjusts the block to replace x with (x * anInteger) in all terms."
		tmp := block.
		block := [:x :expSet | tmp value: (x * anInteger) value: expSet].
	]
	ifFalse:[
		self error: 'invalid input'.
	].! !

!PolyStream methodsFor: 'initialize-release' stamp: 'Selen 12/1/2024 19:35'!
"Accessor method for the block representing the polynomial."
block
	^block! !

!PolyStream methodsFor: 'initialize-release' stamp: 'Selen 12/1/2024 19:52'!
"Initializes the polynomial stream to represent the zero polynomial."
initialize
	block := [:x :expSet | 0].! !





Object subclass: #Polynom
	instanceVariableNames: 'monoms'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'OOP1'!

!Polynom methodsFor: 'converting' stamp: 'Selen 11/30/2024 11:50'!
"Converts the polynomial into a dictionary mapping exponents to coefficients."
asDictionary
	| res tmpExp tmpCoef |
	((self monoms) isEmpty)
	ifTrue:[
		"Return an empty dictionary if the polynomial is empty."
		res := Dictionary new.
		^res  
	]
	ifFalse:[
		res := Dictionary new.
		"Iterate over all monomials and add their exponent-coefficient pairs."
		(self monoms) do: [:each |  
			(each coef = 0)
			ifTrue: []
			ifFalse:[					
				tmpExp := each exp.
				tmpCoef := each coef.
				res add: tmpExp -> tmpCoef.
			].
		].
		^res
	]! !

!Polynom methodsFor: 'mathOperations' stamp: 'Selen 11/30/2024 10:40'!
"Adds another polynomial to the current one."
add: aPolynom
	|res|
	res := Polynom new.
	(aPolynom isKindOf: Polynom)
	ifTrue: [ 
		"Merge monomials from both polynomials."
		(aPolynom monoms) do: [:each | res addMonom: each].
		(self monoms) do: [:each | res addMonom: each].
		^res
	]
	ifFalse: [
		self error: 'invalid input'.
	]! !

!Polynom methodsFor: 'mathOperations' stamp: 'Selen 11/30/2024 11:02'!
"Adds a single monomial to the polynomial."
addMonom: aMonom
	|nodups tmpMon|
	tmpMon := Monom new.
	tmpMon exp: (aMonom exp).
	tmpMon coef: (aMonom coef).
	(aMonom isKindOf: Monom)
	ifTrue:[
		"Check if monomial with the same exponent exists, if yes, add coefficients."
		((self monoms) includes: aMonom)
		ifTrue:[
			(self monoms) do: [:each | each addMon: tmpMon].
			nodups := (self monoms) asSet.
			self monoms: nodups asOrderedCollection.
		]
		ifFalse:[
			"Add new monomial if it doesn't already exist."
			(self monoms) add: tmpMon.
			nodups := (self monoms) asSet.
			self monoms: nodups asOrderedCollection.
		].
	]
	ifFalse:[
		self error: 'invalid input'.
	]! !

!Polynom methodsFor: 'mathOperations' stamp: 'Selen 11/30/2024 10:39'!
"Calculates the derivative of the polynomial."
derivative
	|res resDerivatives tmp|
	resDerivatives := OrderedCollection new.
	"Compute the derivative of each monomial."
	(self monoms) do: [:each | tmp := each derivative.
								resDerivatives add: tmp].
	res := Polynom new.
	res monoms: resDerivatives.	
	^res! !


!Polynom methodsFor: 'mathOperations' stamp: 'Selen 11/30/2024 10:30'!
"Evaluates the polynomial at a given integer."
eval: anInteger
	|res|
	res := 0.
	(anInteger isKindOf: Integer)
	ifTrue: [ 
		"Sum up evaluations of all monomials."
		(self monoms) do: [:each | res := res + (each evalAt: anInteger)].
		^res
	]
	ifFalse: [
		self error: 'invalid input'.
	]! !

!Polynom methodsFor: 'mathOperations' stamp: 'Selen 11/30/2024 10:12'!
"Multiplies the polynomial by a monomial."
multiplyByMonom: aMonom
	|nodups|
	(aMonom isKindOf: Monom)
	ifTrue:[
		"Multiply each monomial in the polynomial by the given monomial."
		(self monoms) do: [:each | each multMon: aMonom].
		nodups := (self monoms) asSet.
		self monoms: nodups asOrderedCollection.
		^self
	]
	ifFalse:[
		self error: 'invalid input'.
	]! !

!Polynom methodsFor: 'initialize-release' stamp: 'Selen 11/30/2024 10:10'!
"Initializes the polynomial as the zero polynomial."
initialize
	|zeroMon|
	monoms := OrderedCollection new.
	zeroMon := Monom new.
	monoms add: zeroMon.
! !

!Polynom methodsFor: 'initialize-release' stamp: 'Selen 11/30/2024 10:01'!
"Accessor method for the monomials collection."
monoms
	^monoms! !

!Polynom methodsFor: 'initialize-release' stamp: 'Selen 11/30/2024 10:04'!
"Setter method for the monomials collection."
monoms: noDupsSet
	monoms := noDupsSet! !
