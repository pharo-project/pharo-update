Pharo 3.0 Update Files
======================

This repository contains a collection of the updates files used for the system-updates of Pharo 3.0.
The files are made available on the [file server](http://files.pharo.org/updates/pharo3.0/).
The [updates30.list](updates30.list) is contains a list of all update files and is generated by a [jenkins job](https://ci.inria.fr/pharo/job/Pharo-3.0-Update-Step-3-Release/).

Do not touch these files manually if you do not know what you are doing. 
Read the class comments of the `ScriptLoader` class for a complete description of the Pharo integration process.


Here is a bit of an explanation of how the staged and update.list are merged: this is the script of the jenkins job
https://ci.inria.fr/pharo/job/Pharo-3.0-Update-Step-3-Release/


./pharo Pharo.image eval "
updates := (ZnEasy get: 'http://files.pharo.org/updates/pharo3.0/updates30.list') contents lines. 
updates := updates reject: [ :each | each beginsWith: '#' ].

staged := 'updates30.staged' asFileReference readStream contents asString lines.
staged := staged reject: [ :each | each beginsWith: '#' ].

updates := (updates, staged) asSet asArray sorted.

'updates30.list' asFileReference ensureDeleted writeStreamDo: [ :output |

	output nextPutAll: '#Pharo3.0'; cr.
	updates do: [ :each |
		output nextPutAll: each; cr ]].

Smalltalk snapshot: false andQuit: true."


The update30.list file is generated on the server and in case of emergency this is the way to modify it.

	ssh files.pharo.org
	sudo su - filepharosync
	cd /appli/files.pharo.org/updates/pharo3.0/
	vi updates30.list
