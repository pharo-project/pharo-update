|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[
[(repository loadVersionFromFileNamed:'ScriptLoader11-MarcusDenker.95.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'CollectionsTests-MarcusDenker.412.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Sequenceable-MarcusDenker.49.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Morphic-MarcusDenker.448.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Kernel-MarcusDenker.500.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Support-MarcusDenker.151.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Download-MarcusDenker.19.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!