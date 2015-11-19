
## NOTE

In case there is any confusion, this project is first and formeost a joke; following that it is an oportunity for me to learn Haskell; finally it may be of some use in querying MongoDB or decyphering queries in JSON.

## Design

Haskell-mongodb uses a Haskell encoding of bson rather than using json but the two can be converted using the aeson-bson library.

Database.MongoDB.Query.Query is a record structure which encodes a query.  It includes a projection term.
Database.MongoDB.Query.Projection is an alias for [Document] so there is a way to convert general pipelines to BSON.

In general we could do:

SQL <--> AST BSON <--> JSON

I.e. we need a new AST containing BSON values that can be converted to/from SQL and JSON

Therefore we can work in this dirction:

 1. Identify MongoDB query idioms
 2. Convert to AST BSON (MongoQuery) informed by Haskell data structures
 3. Implement toAction and toJSON for this data type
 4. Implement SQL -> MongoQuery
 4. Implement JSON -> MongoQuery

Maybe MongoQuery is simply a Query or Pipeline.  We could start with Query.
Work inside out.  Start with expression conversion SQL <-> BSON with QuickChecks.
