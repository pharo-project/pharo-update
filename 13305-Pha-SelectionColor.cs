"Postscript:Leave the line above, and replace the rest of this comment by a useful one.Executable statements should follow this comment, and shouldbe separated by periods, with no exclamation points (!!).Be sure to put any further comments in double-quotes, like this one."|repository|repository := MCHttpRepository                location: 'http://www.squeaksource.com/Pharo/'                user: ''                password: ''.(repository loadVersionFromFileNamed:'ScriptLoader13-MarcusDenker.338.mcz') load.ScriptLoader new update13305.!