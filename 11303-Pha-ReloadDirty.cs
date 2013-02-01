|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[[(repository loadVersionFromFileNamed:'System-Support-MarcusDenker.273.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'ScriptLoader11-MarcusDenker.332.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].

ScriptLoader new addHomeRepositoryToAllPackages.
ScriptLoader new flushCaches.
!