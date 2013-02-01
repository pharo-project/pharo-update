|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[
[(repository loadVersionFromFileNamed:'ScriptLoader11-MarcusDenker.150.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Tools-MarcusDenker.18.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Changes-MarcusDenker.32.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!