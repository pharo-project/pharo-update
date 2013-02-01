(MCPackage named: 'System') workingCopy unregister.
(MCPackage named: 'EventRewrite2') workingCopy unregister.
(MCPackage named: 'DeleteMe') workingCopy unregister.
(MCPackage named: 'Closure') workingCopy unregister.
(MCPackage named: 'ThreadSafeTranscript') workingCopy unregister.

repository := MCHttpRepository
                location: 'http://www.squeaksource.com/Pharo/'
                user: ''
                password: ''.
[(repository loadVersionFromFileNamed:'System-Tools-stephane_ducasse.8.mcz') load.] on: Warning do:[:warn | warn resume].
[(repository loadVersionFromFileNamed:'System-Support-sd.59.mcz') load.] on: Warning do:[:warn | warn resume].
[(repository loadVersionFromFileNamed:'System-Serial Port-stephane_ducasse.6.mcz') load.] on: Warning do:[:warn | warn resume].
[(repository loadVersionFromFileNamed:'System-Localization-marcus_denker.10.mcz') load.] on: Warning do:[:warn | warn resume].
[(repository loadVersionFromFileNamed:'System-Changes-stephane_ducasse.9.mcz') load.] on: Warning do:[:warn | warn resume].