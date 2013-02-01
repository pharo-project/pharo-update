|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.

(MCPackage named: 'Collections-SkipLists') workingCopy unregister.
(MCPackage named: 'CompilerTests') workingCopy unregister.
(MCPackage named: 'EToys') workingCopy unregister.
(MCPackage named: 'Issue1384-Patch-isSelfEvaluating') workingCopy unregister.
(MCPackage named: 'System') workingCopy unregister.
[
[(repository loadVersionFromFileNamed:'ScriptLoader11-MarcusDenker.158.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Support-MarcusDenker.190.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Polymorph-Widgets-MarcusDenker.179.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!