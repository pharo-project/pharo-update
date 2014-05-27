"Migration from MethodContext, ContextPart to: Context"

"1. keep a pointer to MethodContext object"

Author fullName: 'ClementBera'.
contextObj := MethodContext.

"2. Temporarily Prefill globals with new reference"

Smalltalk globals at: #Context put: MethodContext.

"3. Rename references to MethodContext and ContextPart"

{ MethodContext . ContextPart } do: [ :oldClass | 
(oldClass allCallsOnIn: SystemNavigation new) do: [:method |
    | index newSource oldMethod |
    oldMethod := method compiledMethod.
    newSource := oldMethod sourceCode.
    [(index := newSource indexOfSubCollection: oldClass name) = 0] 
        whileFalse: [ 
            newSource := 
                (newSource copyFrom: 1 to: index - 1) , 
                #Context , 
                (newSource copyFrom: index + oldClass name size to: newSource size)  ].
    oldMethod methodClass compile: newSource classified: oldMethod category] ].

"4. Remove the fake global"

Smalltalk globals removeKey: #Context.

"5. Renaming of MethodContext."

MethodContext rename: #Context.

"6. InstVarMoves"

ContextPart instVarNamed: #instanceVariables put: #( ).
contextObj instVarNamed: #instanceVariables put: #(stackp method closureOrNil receiver).

"7. Temporarily remove crashing method"

ContextPart class removeSelector: #basicNew:.

"8. methods copy down"

{ {ContextPart . contextObj} . {ContextPart class  . contextObj class }} do: [ :array |
    | old new |
    old := array first.
    new := array second.
    old methods do: [ :method |
        (new selectors includes: (method selector))
            ifFalse: [ 
                new compile: method sourceCode classified: method protocol.
                old removeSelector: method selector  ] ].
     ].

"9. Superclass switch"

contextObj superclass: InstructionStream.

"10. Restore crashing method"

contextObj class compile: 'basicNew: size

    self error: ''Contexts must only be created with newForMethod:''' classified: #'instance creation'.
    
"11. classPool migration"

contextObj classPool: ContextPart classPool.

"12. ContextPart destruction"

ContextPart removeFromSystem.

"13. Manual copy of printing methods (which was split between ContextPart and MethodContext with a super send)"

contextObj compile: 'printOn: aStream
    self outerContext
        ifNil: [    
            | selector class mclass |
            self method == nil ifTrue: [^ super printOn: aStream].
            class := self receiver class.
            mclass := self methodClass.
            selector := self selector ifNil:[self method defaultSelector].
            aStream nextPutAll: class name.
            mclass == class 
                ifFalse: 
                    [aStream nextPut: $(.
                    aStream nextPutAll: mclass name.
                    aStream nextPut: $)].
            aStream nextPutAll: ''>>''.
            aStream nextPutAll: selector.
            selector = #doesNotUnderstand: ifTrue: [
                aStream space.
                (self tempAt: 1) selector printOn: aStream ] ]
        ifNotNil: [ :outerContext |
             aStream nextPutAll: closureOrNil printString,'' in ''.
             outerContext printOn: aStream ]' classified: #printing.
								 
Author reset