|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[
[(repository loadVersionFromFileNamed:'ScriptLoader11-MarcusDenker.99.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Support-MarcusDenker.21.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Weak-MarcusDenker.18.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'CollectionsTests-MarcusDenker.414.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!