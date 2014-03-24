host-range
==========

Simple util for generate host range.

cabal build
cabal install

Usage:
echo "[aaa-000 aaa-010, bb00 bb05]" | ./host-range

Output: ["aaa-000","aaa-001","aaa-002","aaa-003","aaa-004","aaa-005","aaa-006","aaa-007","aaa-008","aaa-009","aaa-010","bb00","bb01","bb02","bb03","bb04","bb05"]

