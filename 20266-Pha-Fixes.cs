"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."|repository|repository := MCHttpRepository                location: 'http://ss3.gemstone.com/ss/Pharo20'                user: ''                password: ''.(repository loadVersionFromFileNamed:'ScriptLoader20-MarcusDenker.313.mcz') load.ScriptLoader new update20266.!