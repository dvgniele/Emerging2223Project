#!/bin/bash

erlc -o . ambient.erl
erlc -o . car.erl
erlc -o . grid.erl
erlc -o . main.erl
erlc -o . render.erl
erlc -o . wellknown.erl

PAUSE

erl -sname eep -s main init -noshell