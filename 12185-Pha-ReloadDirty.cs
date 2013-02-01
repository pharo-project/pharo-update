|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[[(repository loadVersionFromFileNamed:'Kernel-MarcusDenker.773.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Finalization-MarcusDenker.15.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'CollectionsTests-MarcusDenker.488.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Weak-MarcusDenker.50.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Support-MarcusDenker.374.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!