<p align="center">
  <img width="400" src="public/gelatinous-cube.png">
</p>

# gelatinous cube

A re-do of [conformity](https://github.com/avescodes/conformity) to
work with Datomic Cloud, with some small deviations.

### Example config

A sample config lives under `test-resources`.

### Deviations from Conformity

##### Immutable norm default
  Norms are considered immutable by default: they won't be transacted
  on subsequent runs unless explicitly marked `:mutable`.

##### Single transaction per norm
  Conformity's data structure for describing norms allows for norms to
  accumulate transactions over time by appending new items to a given
  norm collection. This is a confusing aspect of Conformity, in my
  opinion, and allows editing mistakes to produce inconsistent schema.
  Here, every norm conveys a single transaction body; one extends past
  norms by writing distinct new ones bearing new names.

##### No dependency tracking
  Conformity's data structure for describing norms was associative.
  Here, it is ordered, freeing us from having to track dependency
  relationships manually.

##### External norm sources
  gelatinous-cube allows reference to resource files for schema; your
  schema doesn't have to live within gelatinous-cube's config file.

##### Extensible norm sources
  gelatinous-cube allows extension of the mechanism used to resolve
  config references into transaction data; your schema can reside in
  an S3 bucket, encrypted, written in a custom schema DSL.
  
### TODO

##### Understand Cognitect anomalies
  gelatinous-cube currently relies on exceptions to understand when to
  abort transactions; it should also respect anomalies.

##### Want separable transaction data
  Conformity's data structure (see "Single transaction per norm")
  *also* accommodates handling especially-large tx data bodies as
  separate transactions, which is useful. Consider a means for
  preserving this capability; perhaps treating `tx-fn` implementations
  specially, since other types, being literal data structures, aren't
  likely to get large enough.

##### Need transactional update for norms
  Ensuring norms currently allows for a race wherein two clients could
  both determine a norm was necessary and transact it at the same
  time. At one end of the impact spectrum this would cause a
  retransaction of an already-applied norm (resulting in an
  extraneously empty transaction); at the other, since the `tx-fn`
  facility involves arbitrary user code, anything could happen. The
  fix here seems to be providing a transaction function along with
  documentation around how to install/deploy.
  
##### Support migrating from existing solutions
  Folks may be already using migration solutions. Support integrating
  with these somehow.
  - Could insert synthetic tracking entities fast-forwarding past them.
  - Simpler might be to support an `:ignore` keyword or similar on
    previously-transacted norms.

### Tests

Tests can be run via `clj -A:test`. Thanks to `dev-local` these run
against an in-memory Datomic Cloud database; you'll need to follow
[setup instructions](https://docs.datomic.com/cloud/dev-local.html) to
get the corresponding JAR.
