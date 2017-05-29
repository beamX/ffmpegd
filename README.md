FFMPEG tcp stream pipeline
======================

Erlang implementation for handling a tcp stream of a transcoded file using ffmpeg.


Use ffmpeg to tanscode the file and redirect the output to the tcp port
```bash
ffmpeg -i file.mkv -f hls tcp://localhost:10009/feed1.ffm
```


```bash
$ ffmpeg -y -i /tmp/test.aac -acodec libmp3lame -ac 2 -ar 44100 -joint_stereo 1 tcp://localhost:20001/test.mp3

$ ffmpeg -y -i /tmp/big_buck_bunny.mp4 -metadata:s:v rotate=0 -preset ultrafast -vcodec libx264 -f hls -hls_time 8 -hls_list_size 200 -start_number 0 tcp://localhost:10009/feed1.ffm
```

```erlang
> ffmpegd_sup:start_s3_child_audio(self(), 60.5, {uploaders, disk_audio, #{tmp_store_path => "/tmp/test.mp3"}}).

> ffmpegd_sup:start_s3_child(self(), 60.095000, {uploaders, disk, []}).
```