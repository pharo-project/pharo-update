|repository|
repository := MCHttpRepository
                location: 'http://ss3.gemstone.com/ss/Pharo14'
                user: ''
                password: ''.
[
[(repository loadVersionFromFileNamed:'Tools-MarcusDenker.788.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Kernel-MarcusDenker.1030.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'UIManager-MarcusDenker.56.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Polymorph-Widgets-MarcusDenker.600.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Download-MarcusDenker.70.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Support-MarcusDenker.598.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].

ScriptLoader new addHomeRepositoryToAllPackages.
ScriptLoader new flushCaches.
!