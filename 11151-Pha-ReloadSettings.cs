|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.

[
[(repository loadVersionFromFileNamed:'CompilerSystemSettings-AlainPlantec.4.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'FreeTypeSystemSettings-AlainPlantec.5.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'GraphicsSystemSettings-AlainPlantec.3.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'KernelSystemSettings-AlainPlantec.3.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'MonticelloSystemSettings-AlainPlantec.3.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'NetworkSystemSettings-AlainPlantec.3.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'PolymorphSystemSettings-AlainPlantec.3.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'SystemSystemSettings-AlainPlantec.3.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'ToolsSystemSettings-AlainPlantec.3.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new addHomeRepositoryToAllPackages.
ScriptLoader new flushCaches.
!