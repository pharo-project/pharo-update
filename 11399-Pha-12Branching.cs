"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."|repository|repository := MCHttpRepository                location: 'http://www.squeaksource.com/Pharo/'                user: ''                password: ''.(repository loadVersionFromFileNamed:'ScriptLoader11-StephaneDucasse.459.mcz') load.ScriptLoader new update11399.(SystemVersion current version = 'Pharo1.1rc1') ifTrue: [SystemVersion newVersion: 'Pharo1.1rc2'].!