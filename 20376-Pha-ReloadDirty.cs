""|repository|repository := MCHttpRepository                location: 'http://ss3.gemstone.com/ss/Pharo20'                user: ''                password: ''.[(repository loadVersionFromFileNamed:'Collections-Abstract-MarcusDenker.194.mcz') load] on: Warning do: [:ex | ex resume:true].ScriptLoader new flushCaches.!