#!/bin/bash

erl -ts_req_received `date +%s`.0 -pz ebin -pz deps/*/ebin -s zergling_app

