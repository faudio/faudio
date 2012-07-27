
Change Log
==========

List of changes to the Audio Engine API, using [semantic versioning](http://semver.org/).

v1.0
----
* Initial version (released with ScoreCleaner 2.0)

v1.1
----
* Support for non-blocking audio streams
* Lower latency for WASAPI devices

v1.5
----
* Support for Audio Units

v1.5.1
----
* New build system

(*Unreleased, planned changes:*)

v1.6
----              
* Sessions        
* Depracating `AudioHost::hosts()` and friends 
* Support for VST2
* No dependency on ICU, much smaller library

v1.7
----
* Support for non-realtime file streams
* Improved message passing interface
* Depracating actions
* Depracating absolute scheduler methods

v2.0
----
* Removed actions
* Removed absolute scheduling (?)