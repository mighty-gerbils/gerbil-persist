# Gerbil-persist

Gerbil-persist is a package to persist concurrent activities as well as data.
It currently uses [LevelDB](https://github.com/google/leveldb) as a trivial key-value store underneath.
In the future, it may use [PostgreSQL](https://www.postgresql.org/)
or [CockroachDB](https://www.cockroachlabs.com/) instead,
or its own shared-memory object database in the style of [manardb](https://github.com/danlentz/manardb)?

### Copyright and License

Copyright 2020 Mutual Knowledge Systems, Inc. All rights reserved.
Gerbil-Persist is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).
