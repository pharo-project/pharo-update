|repository|
repository := MCHttpRepository
                location: 'http://ss3.gemstone.com/ss/Pharo14'
                user: ''
                password: ''.
[
[(repository loadVersionFromFileNamed:'Tools-MarcusDenker.788.mcz') load] on: Warning do: [:ex | ex resume: true].
[(repository loadVersionFromFileNamed:'Kernel-MarcusDenker.1030.mcz') load] on: Warning do: [:ex | ex resume: true].
[(repository loadVersionFromFileNamed:'UIManager-MarcusDenker.56.mcz') load] on: Warning do: [:ex | ex resume: true].
[(repository loadVersionFromFileNamed:'Polymorph-Widgets-MarcusDenker.600.mcz') load] on: Warning do: [:ex | ex resume: true].
[(repository loadVersionFromFileNamed:'System-Download-MarcusDenker.70.mcz') load] on: Warning do: [:ex | ex resume: true].
[(repository loadVersionFromFileNamed:'System-Support-MarcusDenker.598.mcz') load] on: Warning do: [:ex | ex resume: true].
[(repository loadVersionFromFileNamed:'Monticello-MarcusDenker.573.mcz') load] on: Warning do: [:ex | ex resume: true].

] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.
!