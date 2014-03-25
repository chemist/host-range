host-range
==========

Simple util for generate host range.

cabal build
cabal install

Usage as util:
echo "[aaa-000 aaa-010, bb00 bb05]" | ./host-range

Usage as network server:
./host-range -d
open 4000 port
work with memcached protocol

session example:
 ~ -$ telnet localhost 4000                                                                                                                                  limbo-air@chemist :(
Trying ::1...
telnet: connect to address ::1: Connection refused
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
GET [ab-001 ab-010, host01.example.com host05.example.com]
VALUE [ab-001 ab-010, host01.example.com host05.example.com] 0 166
[ab-001,ab-002,ab-003,ab-004,ab-005,ab-006,ab-007,ab-008,ab-009,ab-010,host01.example.com,host02.example.com,host03.example.com,host04.example.com,host05.example.com]
END
Connection closed by foreign host.




Output: ["aaa-000","aaa-001","aaa-002","aaa-003","aaa-004","aaa-005","aaa-006","aaa-007","aaa-008","aaa-009","aaa-010","bb00","bb01","bb02","bb03","bb04","bb05"]

