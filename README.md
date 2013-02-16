
# Zergling is a part of Zerg demo

The application is a part of Zerg demo.

Before use the application must be converted into a Xen image using an Erlang on
Xen build service (http://build.erlangonxen.org).

Use the following command to make the image:

	rebar ling-build-image

You may need to plug your build service credentials to rebar.conf. The image
should be copied to Dom0 for libvirtd to be able to find it.

Makefile and zergling.xml files are for testing only. The 'spawnpool'
application generates a domain configuration file on the fly (see
https://github.com/maximk/spawnpool).
