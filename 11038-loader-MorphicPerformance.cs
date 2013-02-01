'http://pharo.gforge.inria.fr/updates/pharo1.1/11038-handloaded-MorphicPerformance.cs' asUrl retrieveContents contentStream fileInSilentlyAnnouncing: nil.
Morph allSubInstances do: [:m |
#(fillStyle layoutPolicy layoutFrame layoutProperties borderStyle cornerStyle actionMap 
clipSubmorphs) do: [:prop |
m extension ifNotNilDo: [:extension |
extension otherProperties ifNotNilDo: [:otherProperties |
 extension instVarNamed: prop put: (
  otherProperties at: prop ifAbsent: []).
 otherProperties removeKey: #prop ifAbsent: []]]]]