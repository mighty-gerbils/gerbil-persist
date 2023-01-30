# Gerbil-persist

Gerbil-persist is a package to persist concurrent activities as well as data.
It currently uses [LevelDB](https://github.com/google/leveldb) as a trivial key-value store underneath.

See db.ss for the main database interface.

## Copyright and License

Copyright 2020-2023 Mutual Knowledge Systems, Inc. All rights reserved.
Gerbil-Persist is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

## Changes Underway

We are in the process of:
- Replace LevelDB by an abstraction over key value stores (see kvs.ss, kvs-leveldb.ss)
- Use sqlite as second backend, intended as default for embedding on Gambit-C (kvs-sqlite.ss)
- Generalize db.ss into a multi-tx.ss that handles, queues, and merges multiple transactions.
- Add a cache so that changes can be queued while the db is busy committing.
- Make sure queries run against the cache for changes that haven't been committed yet.
- Add first layer of encryption: encrypted content-addressed store on top of key value store (ecabs.ss)
- Add second layer of encryption: btrees on top content-addressed store.
- Add third layer of encryption: regular database schema on top of the above btrees.
- In the future, support [PostgreSQL](https://www.postgresql.org/) on Gambit-C
- In the future, Support IndexedDB on Gambit-JS.

TODO: Port btree's from CL https://github.com/danlentz/cl-btree
to implement pure database indexes on top of (encrypted) key value store.

## Future Hopes

In the future, we might use
[CockroachDB](https://www.cockroachlabs.com/) as a replicated key-value store,
or rely on multiple remote IPFS providers,
or implement our own shared-memory object database in the style of
[manardb](https://github.com/danlentz/manardb).
