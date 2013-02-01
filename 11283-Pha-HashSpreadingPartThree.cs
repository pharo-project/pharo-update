|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[[(repository loadVersionFromFileNamed:'Kernel-MartinMcClure.627.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Unordered-MartinMcClure.76.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Weak-MartinMcClure.31.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!