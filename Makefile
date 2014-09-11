
PROJ_NAME := zergling

.PHONY: default

default:
	rebar compile
	rebar ling-build-image

