host-range
==========

Simple util for generate host range.

cabal build

cabal install

Usage as util:

echo "aaa-[000-004, 03 - 06 ].ya.ru,  test.ya.ru" | ./host-range

Output: ["aaa-000.ya.ru","aaa-001.ya.ru","aaa-002.ya.ru","aaa-003.ya.ru","aaa-004.ya.ru","aaa-03.ya.ru","aaa-04.ya.ru","aaa-05.ya.ru","aaa-06.ya.ru","test.ya.ru"]


Usage as network server:

./host-range -s

open 4000 port

work with memcached protocol

session example:
```
➜  ~  telnet localhost 4000
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
GET host-[001-003].ya.ru, simple.host.ru
VALUE host-[001-003].ya.ru, simple.host.ru 0 61
[host-001.ya.ru,host-002.ya.ru,host-003.ya.ru,simple.host.ru]
END
Connection closed by foreign host.
```

compiled binary for mac and ubuntu 12.04:

http://yadi.sk/d/h1gAU5JvLCkEk


