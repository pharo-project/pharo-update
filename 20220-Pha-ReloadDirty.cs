""|repository|repository := MCHttpRepository                location: 'http://ss3.gemstone.com/ss/Pharo20'                user: ''                password: ''.[(repository loadVersionFromFileNamed:'Kernel-MarcusDenker.1155.mcz') load] on: Warning do: [:ex | ex resume:true].ScriptLoader new flushCaches.!