|repository|repository := MCHttpRepository                location: 'http://www.squeaksource.com/Pharo/'                user: ''                password: ''.Author initials: 'md'.[(repository loadVersionFromFileNamed:'Tools-marcus_denker.135.mcz') load] on: Warning do: [:ex | ex resume].ChangeSorter compileAll.ChangeSorter initializeChangeSetCategories.Author initials: ''.!