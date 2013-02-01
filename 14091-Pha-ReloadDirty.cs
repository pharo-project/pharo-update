|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo14/'
                user: ''
                password: ''.
[[(repository loadVersionFromFileNamed:'Graphics-Resources-MarcusDenker.11.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].

ScriptLoader new addHomeRepositoryToAllPackages.
ScriptLoader new flushCaches.
!