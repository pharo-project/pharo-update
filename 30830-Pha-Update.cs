'From Pharo3.0 of 18 March 2013 [Latest update: #30829] on 24 April 2014 at 9:40:11.413894 am'!SlotClassBuilderTest subclass: #SlotClassVariableTest	instanceVariableNames: 'announcement collectedAnnouncements'	classVariableNames: ''	poolDictionaries: ''	category: 'SlotTests'!!TClass methodsFor: 'accessing class hierarchy' stamp: 'EstebanLorenzano 4/11/2014 15:48'!addSubclass: aSubclass 	"Make the argument, aSubclass, be one of the subclasses of the receiver. 	Create an error notification if the argument's superclass is not the receiver."		self explicitRequirement! !!Class methodsFor: 'accessing class hierarchy' stamp: 'EstebanLorenzano 4/11/2014 15:52'!addSubclass: aSubclass 	"Make the argument, aSubclass, be one of the subclasses of the receiver. 	Create an error notification if the argument's superclass is not the receiver."	aSubclass superclass ~~ self 		ifTrue: [^self error: aSubclass name , ' is not my subclass'].	subclasses ifNil: [		self subclasses: (Array with: aSubclass).		^ self ].	self subclasses do:[:cl| cl == aSubclass ifTrue:[^self]]. "Already my subclass"	self subclasses: (subclasses copyWith: aSubclass).! !!Trait methodsFor: 'accessing class hierarchy' stamp: 'CamilleTeruel 4/14/2014 14:41'!addSubclass: aSubclass 	self shouldNotImplement! !!PharoClassInstaller methodsFor: 'notifications' stamp: 'EstebanLorenzano 4/11/2014 15:52'!basicClassDefinitionChangedFrom: oldClass to: newClass using: classModification	" Copy over the trait composition "	self copyTraitCompositionFrom: oldClass to: newClass.			" Copy over the method organization "	newClass organization: oldClass organization.	" Update the subclass links "	oldClass superclass == newClass superclass ifFalse: [ 		oldClass superclass removeSubclass: oldClass.		newClass superclass addSubclass: newClass ].	" Announce if necessary "	classModification isPropagation ifFalse: [ 		self systemAnnouncer classDefinitionChangedFrom: oldClass to: newClass ].! !!PharoClassInstaller methodsFor: 'migrating' stamp: 'CamilleTeruel 4/4/2014 16:18'!updateClass: oldClass to: newClass	newClass layout compactClassIndex: oldClass layout compactClassIndex.	self updateInstancesFrom: oldClass to: newClass! !!SlotClassBuilder methodsFor: 'private' stamp: 'CamilleTeruel 4/4/2014 17:06'!applyAndUpdateSharedVariableOrSharedPool: classModification	^ self track: classModification during: [ :old :new |		installer classDefinitionChangedFrom: old to:  new by: classModification.		classModification propagate ].! !!SlotClassBuilderTest methodsFor: 'running' stamp: 'EstebanLorenzano 4/11/2014 16:01'!tearDown	"We remove the classes that could have been created during test run"	super tearDown.	SystemAnnouncer uniqueInstance suspendAllWhile: [		{ self aClassName. self anotherClassName. self yetAnotherClassName. self yetYetAnotherClassName } do: [ :each | 			Smalltalk globals 				at: each 				ifPresent: [ :class | class removeFromSystem ]]].	SystemAnnouncer uniqueInstance unsubscribe: self.		self 		cleanUpTrait: TOne;		cleanUpTrait: TTwo.			Smalltalk organization removeCategory: self aCategory.	(RPackageOrganizer default 		packageNamed: self aCategory		ifAbsent: [ ^ self ]) 		unregister.! !!SlotClassVariableTest methodsFor: 'tests' stamp: 'EstebanLorenzano 4/11/2014 15:28'!testClassVariableDoesNotDuplicatesSubclassesOfSuperclass	"Issue: 13028"	| class1 class2 |			class1 := self make: [ :builder | 		builder			name: self aClassName;			superclass: Object ].		class2 := self make: [ :builder | 		builder			name: self anotherClassName;			superclass: class1 ].		self assert: class1 subclasses equals: { class2 }.	class2 := self make: [ :builder | 		builder			name: self anotherClassName;			superclass: class1;			sharedVariables: 'ASharedVariable' ].	self assert: class1 subclasses equals: { class2 }.! !