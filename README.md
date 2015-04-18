We should update this readme

Pharo 4.0 Update Files
======================

This repository contains a collection of the updates files used for the system-updates of Pharo 4.0.
The files are made available on the [file server](http://files.pharo.org/updates/pharo4.0/).
The [updates40.list](updates40.list) is contains a list of all update files and is generated by a [jenkins job](https://ci.inria.fr/pharo/job/Pharo-4.0-Update-Step-3-Release/).

Do not touch these files manually if you do not know what you are doing. 
Read the class comments of the [`ScriptLoader`](https://github.com/pharo-project/pharo-core/tree/4.0/ScriptLoader40.package/ScriptLoader.class) class for a complete description of the Pharo integration process.

The update40.list file is generated on the server and in case of emergency this is the way to modify it.

	ssh files.pharo.org
	sudo su - filepharosync
	cd /appli/files.pharo.org/updates/pharo4.0/
	vi updates40.list
