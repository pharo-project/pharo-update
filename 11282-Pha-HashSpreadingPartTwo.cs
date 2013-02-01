|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[[(repository loadVersionFromFileNamed:'Collections-Unordered-MartinMcClure.75.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Weak-MartinMcClure.30.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Kernel-MartinMcClure.625.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'KernelTests-MartinMcClure.222.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!