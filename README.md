We should update this readme

Pharo 6.0 Update Files
======================

This repository contains a collection of the updates files used for the system-updates of Pharo 6.0.
The files are made available on the [file server](http://files.pharo.org/updates/pharo6.0/).
The [updates60.list](updates60.list) is contains a list of all update files and is generated by a [jenkins job](https://ci.inria.fr/pharo/job/Pharo-5.0).

Do not touch these files manually if you do not know what you are doing. 
Read the class comments of the [`ScriptLoader`](https://github.com/pharo-project/pharo-core/tree/6.0/ScriptLoader60.package/ScriptLoader.class) class for a complete description of the Pharo integration process.

The update60.list file is generated on the server and in case of emergency this is the way to modify it.

	ssh files.pharo.org
	sudo su - filepharosync
	cd /appli/files.pharo.org/updates/pharo6.0/
	vi updates60.list
