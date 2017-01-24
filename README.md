
## Getting Started

At the moment `transducer` only supports find queries.  It takes bona fide JSON as input so there are some conventions and caviats.  Encode queries as a JSON object with the attributes `query`, `collection` and `projection`.  E.g.
```
$ stack exec transfuse
transfuse> {"collection": "mycol", "projection": {"col1": 1, "col2": 1}, "query": {"$or": [{"foo": "bar"}, {"bar": "baz"}]}}
select col2, col1 from mycol where (foo = 'bar' OR bar = 'baz')
```

## NOTE

In case there is any confusion, this project is first and formeost a joke, although one ariving (3 years late)[http://www.nosql-vs-sql.com/]; following that it is an oportunity for me to learn Haskell; finally it may be of some use in querying MongoDB or decyphering queries in JSON.
