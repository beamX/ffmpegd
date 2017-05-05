PROJECT = ffmpegd

DEPS = ranch lager
dep_ranch_commit = master

include erlang.mk

console:
	./_rel/ffmpegd/bin/ffmpegd console
