
PROJ_NAME := zergling

.PHONY: default

default:
	rebar compile

build:
	rebar compile
	rebar ling-build

image:
	rebar ling-image
	rsync vmling dom0::images/$(PROJ_NAME).img

me:
	rebar compile
	rebar ling-build-image
	rsync vmling dom0::images/$(PROJ_NAME).img

em:
	rebar compile
	rebar ling-build-image
	rsync vmling dom0::images/$(PROJ_NAME).img

start:
	virsh -c xen+tcp://dom0 create --console $(PROJ_NAME).xml

console:
	virsh -c xen+tcp://dom0 console $(PROJ_NAME)

stop:
	virsh -c xen+tcp://dom0 destroy $(PROJ_NAME)

