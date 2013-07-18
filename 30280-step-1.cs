'From Pharo3.0 of 18 March 2013 [Latest update: #30268] on 12 July 2013 at 2:13:10 pm'!!TClass methodsFor: '*FuelTests' stamp: 'SebastianTleye 7/11/2013 17:40'!renameSilently: aName	 [ self rename: aName] fuelValueWithoutNotifications! !!TClass methodsFor: '*FuelTests' stamp: 'SebastianTleye 7/11/2013 17:40'!renameSilently: aName	 [ self rename: aName] fuelValueWithoutNotifications! !Trait removeSelector: #renameSilently:!Class removeSelector: #renameSilently:!!TClass methodsFor: '*GroupManagerUI' stamp: 'SebastianTleye 7/11/2013 17:40'!prettyName	^ self printString! !!TClass methodsFor: '*GroupManagerUI' stamp: 'SebastianTleye 7/11/2013 17:40'!prettyName	^ self printString! !!TClass methodsFor: '*Manifest-Core' stamp: 'SebastianTleye 7/11/2013 17:40'!criticTheNonMetaclassClass	"Return the class of the receiver for the critic browser. This behavior may be folded later by changing the name of this method or using another one."		^ self ! !Class removeSelector: #prettyName!Class removeSelector: #criticTheNonMetaclassClass!!TClass methodsFor: '*HelpSystem-Core' stamp: 'SebastianTleye 7/11/2013 17:41'!asHelpTopic	^SystemReference forClass: self! !!TClass methodsFor: '*HelpSystem-Core' stamp: 'SebastianTleye 7/11/2013 17:41'!asHelpTopic	^SystemReference forClass: self! !Class removeSelector: #asHelpTopic!!MCClassTraitDefinition methodsFor: 'comparing' stamp: 'SebastianTleye 7/12/2013 13:48'!= aDefinition	^ (super = aDefinition)		and: [baseTrait = aDefinition baseTrait		and: [self classTraitCompositionString = aDefinition classTraitCompositionString]]! !!MCClassTraitDefinition methodsFor: 'comparing' stamp: 'SebastianTleye 7/12/2013 13:48'!= aDefinition	^ (super = aDefinition)		and: [baseTrait = aDefinition baseTrait		and: [self classTraitCompositionString = aDefinition classTraitCompositionString]]! !!Trait methodsFor: '*Monticello' stamp: 'SebastianTleye 7/12/2013 13:59'!classDefinitions	| definitions |	definitions := OrderedCollection with: self asClassDefinition.	(self hasClassSide		and: [self classTrait hasTraitComposition])					ifTrue: [definitions add: self classTrait asMCDefinition].	^definitions asArray! !!Trait methodsFor: '*Monticello' stamp: 'SebastianTleye 7/12/2013 13:59'!classDefinitions	| definitions |	definitions := OrderedCollection with: self asClassDefinition.	(self hasClassSide		and: [self classTrait hasTraitComposition])					ifTrue: [definitions add: self classTrait asMCDefinition].	^definitions asArray! !                Class removeSelector: #subclass:layoutClass:slots:sharedVariableNames:sharedPoolNames:category:!!TClass methodsFor: '*Spec-Builder' stamp: 'SebastianTleye 7/11/2013 17:51'!subclass: newName category: newCategory	| result |	result := self subclass: newName					instanceVariableNames: ''					classVariableNames: ''					poolDictionaries: ''					category: newCategory.	^ result 		ifNil: [ Smalltalk at: self name ifAbsent: [ nil ]]		ifNotNil: [ result ]! !!TClass methodsFor: '*Spec-Builder' stamp: 'SebastianTleye 7/11/2013 17:47'!addSourceCode: source into: selector	| method newSource |	method := self methodDict at: selector asSymbol ifAbsent: [ ^ self ].	newSource := String streamContents: [:s |		s << method sourceCode << '.' ; cr ; tab ;<< source ].	self compileWithoutReturn: newSource classified: method category! !!TClass methodsFor: '*Spec-Builder' stamp: 'SebastianTleye 7/11/2013 17:47'!addInstVarNamed: aName type: aClass	^ self addInstVarNamed: aName! !!TClass methodsFor: '*Spec-Builder' stamp: 'SebastianTleye 7/11/2013 17:51'!subclass: newName category: newCategory	| result |	result := self subclass: newName					instanceVariableNames: ''					classVariableNames: ''					poolDictionaries: ''					category: newCategory.	^ result 		ifNil: [ Smalltalk at: self name ifAbsent: [ nil ]]		ifNotNil: [ result ]! !!TClass methodsFor: '*Spec-Builder' stamp: 'SebastianTleye 7/11/2013 17:47'!addSourceCode: source into: selector	| method newSource |	method := self methodDict at: selector asSymbol ifAbsent: [ ^ self ].	newSource := String streamContents: [:s |		s << method sourceCode << '.' ; cr ; tab ;<< source ].	self compileWithoutReturn: newSource classified: method category! !!TClass methodsFor: '*Spec-Builder' stamp: 'SebastianTleye 7/11/2013 17:47'!addInstVarNamed: aName type: aClass	^ self addInstVarNamed: aName! !Class removeSelector: #addInstVarNamed:type:!Class removeSelector: #addSourceCode:into:!Class removeSelector: #subclass:category:!