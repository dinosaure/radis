Radis
=====

Radis is a little library to provide an implementation of a Radix tree
(specialized with a scalar key - like a string, bigarray or array). This project
is a part of ocaml-git to have a fast access to an immutable store. In this way,
remove operation does not exist and this data-structure is focused on lookup
operation when the key is specifically a hash (see
[digestif](https://github.com/mirage/digestif)).

The idea behind this library is to provide a fast access to an immutable store
(like git). So, in my mind, if we put a new object in this store, it can not be
deleted. It's why remove operation does not exists - of course, in a git store,
it's possible to delete an useless object (see `git gc`), however this
computation does not appear in this way.

Examples
========

This library is close to provide a Map like what stdlib provide (again, without
remove operation). Then, you have differents accessors (iter, fold, find).

Benchmarks & Performance
========================

From the initial purpose, the `lookup` operation must be fast. So we did a
benchmark between Radis, Map (from stdlib) and Hashtbl. About the benchmark, we
took a git repository (ocaml-git), do a `git rev-list --objects --all` and took
an arbitrary number of hashes binded with an empty string or a path.

Then, from this associative list, we make a Radis tree, a Map and a Hashtbl.
Finally, we try to lookup all elements from the initial list in these
data-structure and look how long we need to access (`lookup` operation) to a
value.

This benchmark was run on my computer (Thinkpad X1 Carbon - Intel i7-7500U CPU @
2.70 Ghz - 2.90 Ghz). You can run benchmarks with `./bench -p radis`. If you
catch an exception, it's because you don't have enough git objects on your
current directory. Results are available on `benchmarks.txt`.

Inspirations & License
======================

This library, as I said on the header, is a mix between ocaml-stringset and
ocaml-aliases. The license story and improvements on each library (and
abondonment of certain) is not clear to decide which license we should use.

So, I _re_-licensed this library under MIT (like others of my libraries) and
prepare a beer-pot to Hugo if he wants royalties.
