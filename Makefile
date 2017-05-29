PROJECT = ffmpegd

DEPS = ranch lager ffmpeg ucp jsx
dep_ranch_commit = master
dep_ucp = git https://github.com/glejeune/UnicodeCodePoints.git master
dep_ffmpeg  = git https://github.com/geekbull/ffmpeg

include erlang.mk

fast:
	$(MAKE) SKIP_DEPS=1

console:
	./_rel/ffmpegd_release/bin/ffmpegd_release console
