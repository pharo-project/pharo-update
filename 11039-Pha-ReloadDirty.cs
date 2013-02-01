|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[[(repository loadVersionFromFileNamed:'Morphic-MarcusDenker.407.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Polymorph-ToolBuilder-MarcusDenker.16.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Polymorph-Widgets-MarcusDenker.113.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'ScriptLoader11-MarcusDenker.47.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].

ScriptLoader new addHomeRepositoryToAllPackages.
ScriptLoader new flushCaches.
!