FFMPEG tcp stream pipeline
======================

Erlang implementation for handling a tcp stream of a transcoded file using ffmpeg.


Use ffmpeg to tanscode the file and redirect the output to the tcp port
```bash
ffmpeg -i file.mkv -f hls tcp://localhost:10009/feed1.ffm
```
