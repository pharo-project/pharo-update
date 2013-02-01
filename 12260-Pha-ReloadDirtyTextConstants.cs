|repository|
repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[[(repository loadVersionFromFileNamed:'Collections-Text-StephaneDucasse.54.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Collections-Streams-StephaneDucasse.78.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Compiler-StephaneDucasse.254.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'FreeType-StephaneDucasse.542.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Graphics-StephaneDucasse.12.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Graphics-Fonts-StephaneDucasse.21.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Graphics-Primitives-StephaneDucasse.48.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Graphics-Support-StephaneDucasse.11.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Graphics-Text-StephaneDucasse.10.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Morphic-StephaneDucasse.788.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Multilingual-Display-StephaneDucasse.10.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'Multilingual-Scanning-StephaneDucasse.15.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'ST80-StephaneDucasse.163.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'ScriptLoader12-StephaneDucasse.331.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'System-Object Storage-StephaneDucasse.116.mcz') load] on: Warning do: [:ex | ex resume].
[(repository loadVersionFromFileNamed:'TrueType-StephaneDucasse.23.mcz') load] on: Warning do: [:ex | ex resume].
] on: ProgressInitiationException do: [:ex | ex sendNotificationsTo: [ :min :max :curr |  ]].
ScriptLoader new flushCaches.