'From Pharo1.4a of ''16 June 2011'' [Latest update: #14369] on 1 March 2012 at 5:58:11 pm'!

!Debugger methodsFor: 'accessing' stamp: 'CamilloBruni 2/21/2012 16:43'!
isPostMortem
	"return whether we're inspecting a frozen exception without a process attached"
	|selectedContext|
	selectedContext := self selectedContext.
	(interruptedProcess suspendedContext == selectedContext)
		ifTrue: [ ^ false ].
	^ (interruptedProcess suspendedContext findContextSuchThat: [:c | c sender == selectedContext]) isNil! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:46'!
contextStackMenu: aMenu shifted: shifted 
	"Set up the menu appropriately for the context-stack-list, either shifted
	or unshifted as per the parameter provided"
	^ shifted
		ifTrue: [ self shiftedContextStackMenu: aMenu ]
		ifFalse: [ self unshiftedContextStackMenu: aMenu ].
		! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:59'!
doStep
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	| currentContext newContext |
	self okToChange ifFalse: [^ self].
	self isPostMortem ifTrue: [^ self].
	self checkContextSelection.
	currentContext := self selectedContext.
	newContext := interruptedProcess completeStep: currentContext.
	newContext == currentContext ifTrue: [
		newContext := interruptedProcess stepToSendOrReturn].
	self contextStackIndex > 1
		ifTrue: [self resetContext: newContext]
		ifFalse: [newContext == currentContext
				ifTrue: [self changed: #contentsSelection.
						self updateInspectors]
				ifFalse: [self resetContext: newContext]].
! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:50'!
implementStackMenu: aMenu 
	^ aMenu
		add: 'Implement in...'
		subMenu: (self populateImplementInMenu: (UIManager default newMenuIn: self for: self))
		target: nil
		selector: nil
		argumentList: #(nil).
	! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:59'!
peelToFirst
	"Peel the stack back to the second occurance of the currently selected message.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning.  Also frees a lot of space!!"

	| ctxt |
	self isPostMortem ifTrue: [^ self].
	contextStackIndex = 0 ifTrue: [^ Beeper beep].
	"self okToChange ifFalse: [^ self]."
	ctxt := interruptedProcess popTo: self selectedContext findSecondToOldestSimilarSender.
	self resetContext: ctxt.
! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:56'!
postMortemStackActionMenu: aMenu
	
	^ aMenu
		labels: 'FullStack (f)
Where (w)'
		lines: #(2)
		selections: #(#fullStack #where)
! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:59'!
proceed: aTopView 
	"Proceed from the interrupted state of the currently selected context. The 
	argument is the topView of the receiver. That view is closed."

	self okToChange ifFalse: [^ self].
	self isPostMortem ifTrue: [^ self].
	self checkContextSelection.
	self resumeProcess: aTopView! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 20:00'!
restart
	"Proceed from the initial state of the currently selected context. The 
	argument is a controller on a view of the receiver. That view is closed."
	"Closing now depends on a setting (RestartAlsoProceeds class variable)"

	| ctxt noUnwindError |
	self okToChange ifFalse: [^ self].
	self isPostMortem ifTrue: [^ self].
	self checkContextSelection.
	ctxt := interruptedProcess popTo: self selectedContext.
	noUnwindError := false.
	ctxt == self selectedContext ifTrue: [
		noUnwindError := true.
		interruptedProcess restartTop; stepToSendOrReturn].
	self resetContext: ctxt.
	"Issue 3015 - Hernan"
	self isInterruptedContextATest ifTrue: [ self prepareTestToRunAgain ].	

	(self class restartAlsoProceeds and: [noUnwindError]) ifTrue: [self proceed].
! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 20:00'!
returnValue
	"Force a return of a given value to the previous context!!"

	| previous selectedContext expression value |
	contextStackIndex = 0 ifTrue: [^Beeper beep].
	self isPostMortem ifTrue: [^ self].
	selectedContext := self selectedContext.
	expression := UIManager default request: 'Enter expression for return value:'.
	value := self class evaluatorClass new 
				evaluate: expression
				in: selectedContext
				to: selectedContext receiver.
	previous := selectedContext sender.
	self resetContext: previous.
	interruptedProcess popTo: previous value: value! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 20:00'!
send
	"Send the selected message in the accessed method, and take control in 
	the method invoked to allow further step or send."

	self okToChange ifFalse: [^ self].
	self isPostMortem ifTrue: [^ self].
	self checkContextSelection.
	interruptedProcess step: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.
! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:46'!
shiftedContextStackMenu: aMenu
	^ aMenu
				labels: 'Browse class hierarchy
Browse class
Browse method (O)
Implementors of sent messages
Change sets with this method
Inspect instances
Inspect subinstances
Revert to previous version
Remove from current change set
Revert & remove from changes
More...'
				lines: #(5 7 10 )
				selections: #(#classHierarchy #browseClass #openSingleMessageBrowser #browseAllMessages #findMethodInChangeSets #inspectInstances #inspectSubInstances #revertToPreviousVersion #removeFromCurrentChanges #revertAndForget #unshiftedYellowButtonActivity )! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 19:56'!
stackActionMenu: aMenu
	
	^ aMenu
		labels: 'FullStack (f)
Restart (r)
Proceed (p)
Step (t)
Step through (T)
Send (e)
Where (w)
Peel to first like this
Return entered value
Toggle break on entry'
		lines: #(8 9)
		selections: #(#fullStack #restart #proceed #doStep #stepIntoBlock #send #where #peelToFirst #returnValue #toggleBreakOnEntry)
! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 2/13/2012 20:00'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method."

	self isPostMortem ifTrue: [^ self].
	interruptedProcess stepToHome: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.! !

!Debugger methodsFor: 'context stack menu' stamp: 'CamilloBruni 3/1/2012 10:19'!
unshiftedContextStackMenu: aMenu
	self selectedContext selector = #doesNotUnderstand:
		ifTrue: [ self implementStackMenu: aMenu ].
		
	self isPostMortem 
		ifTrue: [ self postMortemStackActionMenu: aMenu]
		ifFalse: [ self stackActionMenu: aMenu ].
	
	^ aMenu
		labels: 'Senders of... (n)
Implementors of... (m)
Inheritance (i)
Versions (v)
Inst var refs...
Inst var defs...
Class var refs...
Class variables
Class refs (N)
Browse full (b)
File out
Copy to clipboard
More...'
		lines: #(35 8 11)
		selections: #(#browseSendersOfMessages #browseMessages #methodHierarchy #browseVersions #browseInstVarRefs #browseInstVarDefs #browseClassVarRefs #browseClassVariables #browseClassRefs #browseMethodFull #fileOutMessage #copyToClipboard #shiftedYellowButtonActivity)
! !

!Debugger methodsFor: 'dependents access' stamp: 'CamilloBruni 2/13/2012 22:33'!
step 
	"Update the inspectors."
	self isPostMortem ifTrue: [ ^ self ].
	receiverInspector ifNotNil: [receiverInspector step].
	contextVariablesInspector ifNotNil: [contextVariablesInspector step].
! !

!Debugger methodsFor: 'initialize' stamp: 'CamilloBruni 2/13/2012 19:42'!
customButtonSpecs
	"Answer an array of elements of the form wording, selector, help-message, that characterize the custom button row of a debugger."

	| list |
	self isPostMortem ifTrue: [ ^ self postMortemCustomeButtonSpecs ].
	
	list := #(('Proceed'	proceed				'Close the debugger and proceed.')
		('Restart'		restart				'Reset this context to its start.')
		('Into'			send				'Step Into message sends')
		('Over'			doStep				'Step Over message sends')
		('Through'		stepIntoBlock		'Step into a block')
		('Full Stack'		fullStack			'Show full stack')
		('Run to Here'	runToSelection		'Run to selection')
		('Where'		where				'Select current pc range')).
								
	self class restartAlsoProceeds ifTrue:
		[list := list collect: [:each |
			each second == #restart
				ifTrue: [each copy at: 3 put: 'Proceed from the beginning of this context.'; yourself]
				ifFalse: [each]]].
	^ list! !

!Debugger methodsFor: 'initialize' stamp: 'CamilloBruni 2/13/2012 20:03'!
openFullMorphicLabel: aLabelString
	"Open a full morphic debugger with the given label"

	^UIManager default 
		openDebugger: self 
		fullMorphicLabel: aLabelString! !

!Debugger methodsFor: 'initialize' stamp: 'CamilloBruni 2/13/2012 19:43'!
postMortemCustomeButtonSpecs
	
	^ #(('Full Stack'		fullStack			'Show full stack')
		('Where'		where				'Select current pc range'))! !

!Debugger methodsFor: 'initialize' stamp: 'CamilloBruni 2/13/2012 19:44'!
postMortemPreDebugButtonQuads
	^ {
		{'Abandon' translated.	#abandon. 	#black.	'Abandon this execution by closing this window' translated}.
		{'Debug'	 translated.		#debug.		#red. 	'Bring up a debugger' translated}
	}! !

!Debugger methodsFor: 'initialize' stamp: 'CamilloBruni 2/13/2012 22:34'!
preDebugButtonQuads
	| buttons |
	self isPostMortem 
		ifTrue: [ ^ self postMortemPreDebugButtonQuads ].
	^ {
		{'Proceed' translated.	#proceed. 	#blue. 	'Continue execution' translated}.
		{'Abandon' translated.	#abandon. 	#black.	'Abandon this execution by closing this window' translated}.
		{'Debug'	 translated.		#debug.		#red. 	'Bring up a debugger' translated}
	}! !


!Debugger class methodsFor: 'opening' stamp: 'CamilloBruni 2/13/2012 19:25'!
openOn: process context: context label: title contents: contentsStringOrNil fullView: bool 
	"Open a notifier in response to an error, halt, or notify. A notifier view
	just shows a short view of the sender stack and provides a menu that
	lets the user open a full debugger."
	|fullView|
	fullView := (bool or: [self alwaysOpenFullDebugger]).
	UIManager default 
		openDebuggerOn: process 
		context: context 
		label: title 
		contents: contentsStringOrNil 
		fullView: fullView! !


!CodeLoader methodsFor: 'installing' stamp: 'CamilloBruni 2/13/2012 22:51'!
handleError: error request: aUrlDownloadRequest
	"Print a header before failing on an errro / syntax notification from the the script loaded by the given request"
	|stderr|
	
	"spit out a warning if in headless mode, otherwise a debugger will popup"
	Smalltalk isHeadless 
		ifTrue: [ self inform: 'Errors in script loaded from ', aUrlDownloadRequest url asString].
																														
	(error isKindOf: SyntaxErrorNotification)
		"for syntax errors we can used the default action"
		ifTrue: [ error defaultAction ]
		"otherwise resignal it"
		ifFalse: [ error pass ]! !

!CodeLoader methodsFor: 'installing' stamp: 'CamilloBruni 2/13/2012 17:17'!
installSourceFile: aRequest
	"Install the previously loaded source file"
	| contents aStream|
	
	aStream := aRequest contentStream.
	aStream ifNil:[^self error: ('No loadable file at ', aRequest url)].
	
	contents := aStream ascii upToEnd unzipped.
	(aStream respondsTo: #close) ifTrue:[aStream close].
	
	[ ^ (RWBinaryOrTextStream with: contents) reset fileIn ]
		on: Error, ParserNotification
		do: [ :e| 	self handleError: e request: aRequest].! !

!SmalltalkImage methodsFor: 'vm parameters' stamp: 'CamilloBruni 2/13/2012 18:55'!
isInteractive
	"Check if vm were run with headless parameter.
	Different VMs for different platform have different multiple way(s) to indicate that"
	
	"non-headless mode is always interactive"
	self isHeadless ifFalse: [ ^ true ].
	
	-1000 to: 1000 do: [ :n | 
		(#('-interactive') includes: (self vm getSystemAttribute: n)) 
			ifTrue: [ ^ true ]].
	
	^ false! !


!Process methodsFor: 'debugging' stamp: 'CamilloBruni 3/1/2012 11:25'!
debug: context title: title full: bool
	
	Smalltalk tools debugger
						openOn: self 
						context: context 
						label: title 
						contents: nil 
						fullView: bool.
						! !

!ContextPart methodsFor: 'controlling' stamp: 'CamilloBruni 2/13/2012 18:05'!
shortDebugStack
	"Answer a String showing the top ten contexts on my sender chain."

	^ String streamContents: [:stream |
		self debugStack: 10 on: stream]! !

!ContextPart methodsFor: 'printing' stamp: 'CamilloBruni 2/13/2012 18:08'!
debugStack: stackSize on: aStream
	"print a condensed version of the stack up to stackSize on aStream"
	(self stackOfSize: stackSize) do: [:item | 
		item printDebugOn: aStream.
		aStream cr]! !

!ContextPart methodsFor: 'printing' stamp: 'CamilloBruni 2/13/2012 18:08'!
debugStackOn: aStream
	"print the top ten contexts on my sender chain."

	^ self debugStack: 100 on: aStream! !

!ContextPart methodsFor: 'printing' stamp: 'CamilloBruni 2/13/2012 23:22'!
printDebugOn: aStream
	"print a condensed for of the stack.
		For methods simply print Class >> selector
		For blocks only print the first line"
	self printOn: aStream! !

!ContextPart methodsFor: 'printing' stamp: 'CamilloBruni 2/13/2012 18:07'!
shortDebugStackOn: aStream
	"print the top ten contexts on my sender chain."

	^ self debugStack: 10 on: aStream! !


!MethodContext methodsFor: 'printing' stamp: 'CamilloBruni 2/13/2012 23:21'!
printDebugOn: aStream	
	"print a condensed for of the stack.
		For methods simply print Class >> selector
		For blocks only print the first line"
	| blockSource blockSourceSize |
	
	super printOn: aStream.
	self outerContext ifNil: [ ^ self ].
	"print the block..."
	aStream 
		nextPutAll: ' in Block: '.
	
	blockSource := closureOrNil printStringLimitedTo: 50.
	blockSourceSize := blockSource size.
	blockSource := blockSource copyUpTo: Character cr.
	
	aStream nextPutAll: blockSource.
	blockSource size < blockSourceSize
		ifTrue: [ aStream nextPutAll: '...' ].! !


!Exception methodsFor: 'handling' stamp: 'CamilloBruni 2/13/2012 22:35'!
debug
	"open a debugger on myself"
	Smalltalk tools debugError: self! !

!Exception methodsFor: 'handling' stamp: 'CamilloBruni 2/13/2012 16:59'!
freeze
	"freeze the context stack to keep the exception usable outside the catch blocks"
	self freezeUpTo: thisContext! !

!Exception methodsFor: 'handling' stamp: 'CamilloBruni 2/13/2012 16:59'!
freezeUpTo:  aContext
	"freeze the signal context up to the given context so the exception is usable outside the catch block"
	signalContext := signalContext copyTo: aContext.! !

!SmalltalkImage methodsFor: 'miscellaneous' stamp: 'CamilloBruni 2/13/2012 22:29'!
logError: errMsg inContext: aContext

	" we should think about integrating a toothpick here someday"
	self logStdErrorDuring: [ :stderr|
		"install the line end conversion and force initialize the converter"
		stderr
			nextPutAll: errMsg; cr;
			"reset the color"
			nextPut: Character escape; nextPutAll: '[0m'.
		aContext shortDebugStackOn: stderr.].
				
	self logDuring: [:logger | 
		logger	
			nextPutAll: 'THERE_BE_DRAGONS_HERE'; cr; 
		  	nextPutAll: errMsg; cr.
			"just print the error message if no context is given"
			aContext ifNotNil: [
				aContext errorReportOn: logger ].
		" write some type of separator"
		logger nextPutAll: (String new: 79 withAll: $- ); cr; cr]! !

!SmalltalkImage methodsFor: 'miscellaneous' stamp: 'CamilloBruni 2/13/2012 19:21'!
logStdErrorDuring: aBlock
	| stderr |
	[
		"install the line end conversion and force initialize the converter"
		stderr := FileStream  stderr
					wantsLineEndConversion: true;
					converter;
					yourself.
		
		"log in red"
		stderr nextPut: Character escape; nextPutAll: '[31m'.
		"rund the loggin block"
		aBlock value: stderr.
		"reset the coloring"
		stderr nextPut: Character escape; nextPutAll: '[0m'.
	] on: Error do: [ :e| "we don't care if the logging to stdout fails..." ].! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/13/2012 23:35'!
executeDeferredStartupActions: resuming
	"Execute the deferred actions which where added during startup, resuming is true if the iage was started fresh, false if we just saved an image"

	| errors |
	
	deferredStartupActions ifNil: [^ #()].
	
	errors := OrderedCollection new.	
		
	deferredStartupActions do: [:each |
		self 
			logStartUpErrorDuring: [ each cull: resuming ] 
			into: errors
			tryDebugger: self isInteractive].
		
	^ errors! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/21/2012 01:05'!
exit: exitStatus
	"Primitive. Exit to another operating system on the host machine, if one
	exists. All state changes in the object space since the last snapshot are lost.
	Essential. See Object documentation whatIsAPrimitive.
	
	Possible values for exitStatus:
	0:   success
	> 1: error"

	<primitive: 113>
	self primitiveFailed! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/21/2012 01:12'!
exitFailure
	"Quit the VM with a failing signal.
	Will lose all current changes."
	self exit: 1! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/21/2012 01:12'!
exitSucess
	"Quit the VM with a success signal.
	Will lose all current changes."
	self exit: 0! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/21/2012 01:11'!
handleStartupErrors: startupErrors
	"Handle the errors produced during startup.
	
	Resume to open a non-interactive debugger on the recorded errors"
	self logError: 'Got startup errors: ' inContext: nil.
	
	startupErrors do: [ :error|
		self logError: '    ', error description inContext: nil ].
	
	self isInteractive 
		ifFalse: [ self exitFailure ]
		ifTrue: [  |answer|
			answer := (UIManager default 
				confirm: 'Got startup errors, proceed to open debuggers'
				trueChoice: 'Debug'
				falseChoice: 'Cancel').
			answer == true
				ifTrue: [ startupErrors do: #debug ]]! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/13/2012 23:45'!
logStartUpErrorDuring: aBlock into: aCollection tryDebugger: tryDebugger
	"handle errors thrown by the given block and log a freezed version of them in aCollection"
	aBlock on: Error do: [ :err|
		self logStartupError: err into: aCollection tryDebugger: tryDebugger]! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/13/2012 23:47'!
logStartupError: anError into: aCollection tryDebugger: tryDebugger
	"try to debug the given error or add a freezed version to aCollection"
	tryDebugger ifTrue: [ 
		[ ^ anError debug ] 
			on: Error do: [ 
				"an error occured during error handling... treat the error as a normal startup error " ]].

	self 
		logError: '==== Startup Error: ', anError description 
		inContext: anError signalerContext.
	"freeze the error"
	aCollection add: anError freeze.! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/13/2012 23:35'!
send: startUpOrShutDown toClassesNamedIn: startUpOrShutDownList with: argument
	"Send the message #startUp: or #shutDown: to each class named in the list.
	The argument indicates if the system is about to quit (for #shutDown:) or if
	the image is resuming (for #startUp:).
	If any name cannot be found, then remove it from the list."

	| removals errors |
	removals := OrderedCollection new.
	errors := OrderedCollection new.
	startUpOrShutDownList do: [ :name | 
			| class |
			class := self at: name ifAbsent: [ nil ].
			class isNil
				ifTrue: [ removals add: name ]
				ifFalse: [ 
					self 
						logStartUpErrorDuring: [ class perform: startUpOrShutDown with: argument ] 
						into: errors 
						tryDebugger: false]].
			
	"Remove any obsolete entries, but after the iteration"
	startUpOrShutDownList removeAll: removals.
	^ errors! !

!SmalltalkImage methodsFor: 'snapshot and quit' stamp: 'CamilloBruni 2/13/2012 17:24'!
snapshot: save andQuit: quit
	"Mark the changes file and close all files as part of #processShutdownList.
	If save is true, save the current state of this Smalltalk in the image file.
	If quit is true, then exit to the outer OS shell.
	The latter part of this method runs when resuming a previously saved image. This resume logic checks for a document file to process when starting up."

	| snapshotResult resuming startupErrors |
	Object flushDependents.
	Object flushEvents.

	self addSnapshotRecord: save andQuit: quit.
	self processShutDownList: quit.
	Cursor write show.
	save
		ifTrue: [ 
			snapshotResult := self snapshotPrimitive.	"<-- PC frozen here on image file"
			resuming := snapshotResult == true ]
		ifFalse: [ resuming := false ].
	(quit and: [ resuming not ])
		ifTrue: [ self quitPrimitive ].
	Cursor normal show.
	
	self vm setGCParameters.
	
	deferredStartupActions := nil.
	resuming
		ifTrue: [ Smalltalk clearExternalObjects ].
	
	startupErrors := OrderedCollection new.
	
	UIManager default boot: resuming during: [
		startupErrors addAll: (self processStartUpList: resuming).
		resuming
			ifTrue: [ self recordStartupStamp ]].
	startupErrors addAll: (self executeDeferredStartupActions: resuming).
	
	startupErrors isEmpty 
		ifFalse: [ self handleStartupErrors: startupErrors ].
		
	snapshotResult == nil
		ifTrue: [ self error: 'Failed to write image file (disk full?)' ].
		
	^ resuming! !


CodeLoader removeSelector: #printSyntaxNotification:request:!


!MorphicUIManager methodsFor: 'services' stamp: 'CamilloBruni 2/13/2012 20:03'!
openDebugger: aDebugger fullMorphicLabel: aLabelString
	"Open a full morphic debugger with the given label"

	| window aListMorph oldContextStackIndex label |
	oldContextStackIndex := aDebugger contextStackIndex.
	aDebugger expandStack. "Sets contextStackIndex to zero."
	
	label := aDebugger isPostMortem 
					ifTrue: [ 'PostMortem: ', aLabelString ]
					ifFalse: [ aLabelString ] .
	window := (SystemWindow labelled: label) model: aDebugger.
	aListMorph := self buildListOfElementsFor: aDebugger.
	aListMorph menuTitleSelector: #messageListSelectorTitle.
	window 
		addMorph: aListMorph
		frame: (0@0 corner: 1@0.25).

	aDebugger addLowerPanesTo: window at: (0@0.25 corner: 1@0.8) with: nil.

	window 
		addMorph: (self buildListOfFieldsFor: aDebugger)
			"For doubleClick to work best disable autoDeselect"	
		frame: (0@0.8 corner: 0.2@1).
		
	window
		addMorph: (self buildFieldInspectorFor: aDebugger)
		frame: (0.2@0.8 corner: 0.5@1).
		
	window
		addMorph: (self buildListOfContextFor: aDebugger)
		frame: (0.5@0.8 corner: 0.7@1).
		
	window
		addMorph: (self buildContextInspectorFor: aDebugger)
		frame: (0.7@0.8 corner: 1@1).
		
	window openInWorld.
	window center: Display center.
	aDebugger toggleContextStackIndex: oldContextStackIndex.
	^ window! !

!MorphicUIManager methodsFor: 'events' stamp: 'CamilloBruni 2/13/2012 23:22'!
onSnapshot: resuming
	"The resuming argument is true when image boots from disk,
	and false, if user just did an image snapshot."
	
	"if we resuming, check if we're still interactive "
	
	resuming ifTrue: [
		Smalltalk isInteractive ifFalse: [
			^ self nonInteractiveManager onSnapshot: resuming ].
		Smalltalk isHeadless ifTrue: [
			^ self headlessManager onSnapshot: resuming ]].

	SystemWindow wakeUpTopWindowUponStartup! !

!MorphicUIManager methodsFor: 'private' stamp: 'CamilloBruni 2/13/2012 19:37'!
buildMorphicNotifierFor: aDebugger labelled: label message: messageString
	| notifyPane window extentToUse row|
	aDebugger expandStack.
	window := (PreDebugWindow labelled: label) model: aDebugger.
	extentToUse := 450 @ 156. "nice and wide to show plenty of the error msg"
	
	window
		addMorph: (row := aDebugger buttonRowForPreDebugWindow: window)
		fullFrame: (LayoutFrame fractions: (0@0 corner: 1@0) offsets: (0@0 corner: 0@row minExtent y)).
	
	row color: Color transparent.
	messageString notNil
		ifFalse:
			[notifyPane := self buildNotifyListPane: aDebugger ]
		ifTrue:
			[notifyPane := self buildNotifyTextPane: aDebugger.
			notifyPane editString: (aDebugger preDebugNotifierContentsFrom: messageString);
				askBeforeDiscardingEdits: false].
	window
		addMorph: notifyPane
		fullFrame: (LayoutFrame fractions: (0@0 corner: 1@1) offsets: (0@24 corner: 0@0)).
	window setBalloonTextForCloseBox.
	window openInWorldExtent: extentToUse.
	window currentWorld displayWorld. "helps with interrupt not working somehow."
	^window! !

UIManager subclass: #CommandLineUIManager
	instanceVariableNames: 'doNotQuitOnRestart uiManager'
	classVariableNames: 'SnapshotErrorImage'
	poolDictionaries: ''
	category: 'UIManager'!


!CommandLineUIManager methodsFor: 'events' stamp: 'CamilloBruni 2/13/2012 19:11'!
onPrimitiveError: aString	

	" log error and quit "
	^ self quitFrom: thisContext sender withMessage: aString! !

!CommandLineUIManager methodsFor: 'events' stamp: 'CamilloBruni 2/13/2012 21:35'!
onSnapshot: resuming
	"The resuming argument is true when image boots from disk,
	and false, if user just did an image snapshot."
	
	resuming ifTrue: [
		Smalltalk isInteractive ifFalse: [
			FileStream stdout nextPutAll: 'non interactive headless'; crlf.
			^ self nonInteractiveManager onSnapshot: resuming ].
		Smalltalk isHeadless ifFalse: [
			FileStream stdout nextPutAll: 'moprhic'; crlf.
			uiManager beDefault.   "restore old, or nil, so it will be set in #default "
			UIManager default onSnapshot: resuming]. 
		^ self].
	
	
	" this flag set to true only if we are saving a snapshot before quitting "
	doNotQuitOnRestart ifTrue: [
		Smalltalk snapshot: false andQuit: true].! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/14/2012 09:59'!
choose: questionsAnswerDict title: queryString
! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/13/2012 18:48'!
chooseFrom: aList lines: linesArray title: aString
	| maxPad |
	maxPad := aList size asString size.
	self stdout 
		nextPutAll: aString; nextPut: $:; cr.
	aList withIndexDo: [ :item :index |
		self stdout 
			nextPutAll: '    ['; nextPutAll: (index asString padded: #left to: maxPad with: $ ); nextPutAll: '] ';
			print: item; cr].
	self stdout nextPutAll: '> '.	
	^ aList at: (self stdin upToAnyOf: String crlf do: [ :chr| ]) asInteger.! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/14/2012 09:51'!
confirm: queryString trueChoice: trueLabel falseChoice: falseLabel
	^self
		confirm: queryString
		trueChoice: trueLabel
		falseChoice: falseLabel
		cancelChoice: nil
		default: nil
		! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/14/2012 09:59'!
confirm: queryString trueChoice: trueLabel falseChoice: falseLabel cancelChoice: cancelLabel default: trueFalseNil
	| questions |
	questions := Dictionary new.
	trueLabel ifNotNil: [ questions at: 'y' put: trueLabel ].
	falseLabel ifNotNil: [ questions at: 'n' put: falseLabel ].
	cancelLabel ifNotNil: [ questions at: 'c' put: cancelLabel ].
	^ self choose: questions title: queryString
! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/13/2012 18:29'!
inform: aString

	| logBlock |

	"Just log notifications"
	(ProvideAnswerNotification signal: aString) ifNotNil: [:answer | ^true].
	
	logBlock := [:logger |
		logger cr;
			nextPutAll: (String new: 79 withAll: $= ); cr;
			nextPutAll: 'Notice: ';
			nextPutAll: aString; cr;
			nextPutAll: (String new: 79 withAll: $= ); cr].

	Smalltalk logDuring: logBlock.
	self logYellowDuring: logBlock.
		
	Transcript show: aString; cr.! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/13/2012 19:10'!
informUserDuring: aBlock
	self
		displayProgress: ''
		at: 0@0
		from: 1 to: 100
		during: aBlock! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/13/2012 19:08'!
progressInitiationExceptionDefaultAction: anException

	| result |

	result := anException workBlock value: [:barVal | ].

	anException resume: result! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/21/2012 02:05'!
syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification
	"log the syntax notificaiton and print a nicely formatted and colored syntax error on stderr"
	| contents position errorMessage lineNumber maxLineNumberSize errorLine |
	
	"log the error"
	Smalltalk logDuring: [ :logger |
		logger print: aSyntaxErrorNotification; cr.
		aSyntaxErrorNotification signalerContext errorReportOn: logger ].
	
	"format the error"
	position := aSyntaxErrorNotification location.
	contents := aSyntaxErrorNotification errorCode.
	errorLine := contents lineNumberCorrespondingToIndex: position.
		
	"first gather the error title to be able to underline it properly"
	errorMessage := String streamContents: [ :s|
		s nextPutAll: 'Syntax Error on line '; 
			print: errorLine; nextPutAll: ': '; 
			print: aSyntaxErrorNotification errorMessage].
	
	self logRedDuring: [ :s|
		s nextPutAll: errorMessage; cr;
			nextPutAll: ('' padded: #left to: errorMessage size with: $=); cr].
	
	"print each source line and mark the found syntax error"
	maxLineNumberSize := (contents lines size) asString size.
	lineNumber := 0.
	contents lineIndicesDo: [:start :endWithoutDelimiters :end |
		lineNumber := lineNumber + 1.
		self logColored:  (lineNumber == errorLine ifTrue:['31'] ifFalse:['33']) during: [ :s|
			"0 pad the line numbers to the same size"
			s nextPutAll: ( lineNumber asString padded: #left to: maxLineNumberSize with: $0); nextPutAll: ': ' ].
		
		self stderr nextPutAll: (contents copyFrom: start to: endWithoutDelimiters); cr.
		"print the marker under the error line"
		(lineNumber == errorLine) ifTrue: [
			self logRedDuring: [ :s|
				s nextPutAll:( '_^_' padded: #left to: position - start + maxLineNumberSize + 4 with: $ ); cr]]].
			
	"in noninteractive mode simply quit"
	^ self exitFailure! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/13/2012 19:12'!
uiProcess
	" receiver don't have a ui process, associated with it,
	client should check explicitly if #uiProcess answers nil or not"
	^ nil! !

!CommandLineUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/13/2012 19:09'!
warningDefaultAction: aWarning
	| logBlock |
	"Pass all warnings, but log them"
	
	logBlock := [:logger |
		logger 
			cr;
			nextPutAll: '*** Warning: ';
			nextPutAll: aWarning description;
			cr ].
	
	Smalltalk logDuring: logBlock.
	self logYellowDuring: logBlock.
	
	aWarning resume.

	! !

!CommandLineUIManager methodsFor: 'deprecated' stamp: 'CamilloBruni 2/13/2012 19:10'!
displayProgress: titleString at: aPoint from: minVal to: maxVal during: workBlock
	"Display titleString as a caption over a progress bar while workBlock is evaluated."
	^ProgressInitiationException 
		display: titleString
		at: aPoint 
		from: minVal 
		to: maxVal 
		during: workBlock! !

!CommandLineUIManager methodsFor: 'utils' stamp: 'CamilloBruni 2/21/2012 02:05'!
exitFailure
	[
		self class snapshotErrorImage ifTrue: [
			doNotQuitOnRestart := true.
			"make a new image version snapshot before leaving"
			Smalltalk saveAsNewVersion  ].
	] ensure: [ 
		doNotQuitOnRestart ifFalse: [ Smalltalk exitFailure ].
		doNotQuitOnRestart := false.	
	].
	
	! !

!CommandLineUIManager methodsFor: 'utils' stamp: 'CamilloBruni 2/9/2012 12:54'!
logColored: anAnsiiColorCode during: aBlock
	|stderr|
	
	stderr := self stderr.
	stderr
		nextPut: Character escape; 
		nextPut: $[; nextPutAll: anAnsiiColorCode; nextPut: $m.
		
	aBlock value: self stderr.
		
	stderr nextPut: Character escape; nextPutAll: '[0m'! !

!CommandLineUIManager methodsFor: 'utils' stamp: 'CamilloBruni 2/13/2012 22:28'!
logDuring: aBlock

	aBlock value: self stderr.! !

!CommandLineUIManager methodsFor: 'utils' stamp: 'CamilloBruni 2/9/2012 12:55'!
logGreenDuring: aBlock
	
	^ self logColored: '32' during: aBlock! !

!CommandLineUIManager methodsFor: 'utils' stamp: 'CamilloBruni 2/9/2012 12:53'!
logRedDuring: aBlock
	
	^ self logColored: '31' during: aBlock! !

!CommandLineUIManager methodsFor: 'utils' stamp: 'CamilloBruni 2/9/2012 12:55'!
logYellowDuring: aBlock
	
	^ self logColored: '33' during: aBlock! !

!CommandLineUIManager methodsFor: 'utils' stamp: 'CamilloBruni 2/21/2012 02:05'!
quitFrom: aContext withMessage: aString

	" log error and quit "
	[ Smalltalk 
		logError: aString
		inContext: aContext.
		
	" Print stacks of all current processes "

	Smalltalk logDuring: [:logger | 
		logger nextPutAll: 'Processes and their stacks: ';cr.
		
		Process allInstances do: [:each | | ctx |
			logger nextPutAll: 'Process: '; print: each; cr; nextPutAll: '  stack:'; cr; cr.
		
			ctx := each isActiveProcess ifTrue: [ thisContext sender ] ifFalse: [ each suspendedContext ].
			ctx ifNotNil: [
				(ctx stackOfSize: 20) do: [:s | logger print: s; cr ]].
			logger nextPutAll: '------------------------------'; cr; cr.	
		]].
	] ensure: [ self exitFailure ]! !

!CommandLineUIManager methodsFor: 'accessing' stamp: 'CamilloBruni 2/13/2012 21:23'!
headlessManager
	self class == CommandLineUIManager 
		ifFalse: [ ^ self ].
		
	^ CommandLineUIManager replacing: uiManager! !

!CommandLineUIManager methodsFor: 'accessing' stamp: 'CamilloBruni 2/13/2012 17:06'!
stderr
	
	"install the line end conversion and initialize the converter"
	 FileStream stderr wantsLineEndConversion: true; converter.
	
	^ FileStream stderr! !

!CommandLineUIManager methodsFor: 'accessing' stamp: 'CamilloBruni 2/13/2012 18:40'!
stdin
	
	^ FileStream stdin! !

!CommandLineUIManager methodsFor: 'accessing' stamp: 'CamilloBruni 2/13/2012 18:37'!
stdout
	
	"install the line end conversion and initialize the converter"
	 FileStream stdout wantsLineEndConversion: true; converter.
	
	^ FileStream stdout! !

!CommandLineUIManager methodsFor: 'non-interactive' stamp: 'CamilloBruni 2/13/2012 19:14'!
nonInteractiveManager
	" Answer an instance of non-interactive manager, which will be used when image runs headless.
	  We put it here, so subclasses can override it. "
	^ NonInteractiveUIManager replacing: uiManager! !

!CommandLineUIManager methodsFor: 'default actions' stamp: 'CamilloBruni 2/13/2012 19:12'!
unhandledErrorDefaultAction: anException

	self quitFrom: anException signalerContext  withMessage: anException description.
	
	UIManager default == self ifFalse: [
		^ UIManager default unhandledErrorDefaultAction: anException
		]! !

!CommandLineUIManager class methodsFor: 'instance creation' stamp: 'CamilloBruni 2/9/2012 12:02'!
replacing: aUIManager

	"Replace the current UI manager with instance of myself. 
	Keep a backup reference to old manager, and then restore it, when image will be interactive again. "
	
	^ self new replacing: aUIManager! !

!CommandLineUIManager class methodsFor: 'accessing' stamp: 'CamilloBruni 2/13/2012 21:25'!
snapshotErrorImage
	^ SnapshotErrorImage == true! !

!CommandLineUIManager class methodsFor: 'accessing' stamp: 'CamilloBruni 2/13/2012 21:25'!
snapshotErrorImage: aBoolean
	SnapshotErrorImage := aBoolean! !

!CommandLineUIManager class methodsFor: 'as yet unclassified' stamp: 'CamilloBruni 2/13/2012 21:27'!
uiSettingsOn: aBuilder
	<systemsettings>
	(aBuilder group: #nonInteractive)
		label: 'Headless mode';
		with: [
			(aBuilder setting: #snapshotErrorImage)
				label: 'Make a snapshot of new version before quit' translated;
				target: CommandLineUIManager;
				description: 'On unhandled exception, save a new version of image before quit' translated]! !

!CommandLineUIManager methodsFor: 'display' stamp: 'CamilloBruni 2/13/2012 19:10'!
displayProgress: titleString from: minVal to: maxVal during: workBlock
	"Display titleString as a caption over a progress bar while workBlock is evaluated."
	^ProgressInitiationException 
		display: titleString
		at: Display center 
		from: minVal 
		to: maxVal 
		during: workBlock! !

CommandLineUIManager subclass: #NonInteractiveUIManager
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'UIManager'!

!NonInteractiveUIManager methodsFor: 'as yet unclassified' stamp: 'CamilloBruni 2/13/2012 21:34'!
onSnapshot: resuming
	"The resuming argument is true when image boots from disk,
	and false, if user just did an image snapshot."
	
	resuming ifTrue: [
		Smalltalk isHeadless ifFalse: [
			FileStream stdout nextPutAll: 'morphic'; crlf.
			"restore old, or nil, so it will be set in #default "
			uiManager beDefault.  
			UIManager default onSnapshot: resuming.
			^ self ].
		Smalltalk isInteractive ifTrue: [
			FileStream stdout nextPutAll: 'interactive headless'; crlf.
			"use a headless but interactive manager"
			^ self headlessManager onSnapshot: resuming ]].
	
	
	" this flag set to true only if we are saving a snapshot before quitting "
	doNotQuitOnRestart ifTrue: [
		Smalltalk snapshot: false andQuit: true].! !

!NonInteractiveUIManager methodsFor: 'non-interactive' stamp: 'CamilloBruni 2/13/2012 19:02'!
nonInteractiveManager
	" Answer an instance of non-interactive manager, which will be used when image runs headless.
	  We put it here, so subclasses can override it. 
	
	We already non-interactive. Just answer self
	"
	^ self! !

!NonInteractiveUIManager methodsFor: 'ui requests' stamp: 'CamilloBruni 2/21/2012 02:05'!
syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification
	
	"display and log the syntax error"
	super syntaxErrorNotificationDefaultAction: aSyntaxErrorNotification.
	
	"in noninteractive mode simply quit"
	^ self exitFailure! !
!NonInteractiveUIManager commentStamp: 'IgorStasenko 1/24/2011 15:36' prior: 0!
This is a non-interactive UI manager, i.e. a UI manager which doesn't provides any kind of interactivity with users.

For most of requests, it throws an ErrorNonInteractive exception, which can be handled by various tools to do things differently when UI is not avaliable. For example:

response := [ UIManager default request: 'what is your name?' ] on: ErrorNonInteractive do: [:ex | ex resume: 'Mr. John Smith' ].

You can replace the default UI Manager with my instance in cases, when you need to guarantee that your task(s) will run in fully automated mode. This is useful for things like:
  - when image runs as a persistent application on a server
  - image runs headless from command-line with some batch scripts/commands

!
NonInteractiveUIManager class removeSelector: #snapshotErrorImage!
NonInteractiveUIManager class removeSelector: #snapshotErrorImage:!
NonInteractiveUIManager class removeSelector: #uiSettingsOn:!
NonInteractiveUIManager removeSelector: #displayProgress:at:from:to:during:!
NonInteractiveUIManager removeSelector: #displayProgress:from:to:during:!
NonInteractiveUIManager removeSelector: #inform:!
NonInteractiveUIManager removeSelector: #informUserDuring:!
NonInteractiveUIManager removeSelector: #onPrimitiveError:!
NonInteractiveUIManager removeSelector: #progressInitiationExceptionDefaultAction:!
NonInteractiveUIManager removeSelector: #quit!
NonInteractiveUIManager removeSelector: #quitFrom:withMessage:!
NonInteractiveUIManager removeSelector: #uiProcess!
NonInteractiveUIManager removeSelector: #unhandledErrorDefaultAction:!
NonInteractiveUIManager removeSelector: #warningDefaultAction:!

