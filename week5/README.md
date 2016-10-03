## Week 5 (October 3 – October 9)

This week has only got two tasks, because experimenting with format is fun. One task is tricky and the other one is hard.

### 21. Write a quine `{quine}`

A quine is a program that prints its own source code.

Write a quine in Haskell. (Tricks like reading your own source with `readFile` aren't allowed.)

### 22. Write a database engine `{db}`

This is what you should be able to do with your database:

* Create a table:

  ```
  > CREATE TABLE foo (id UNIQUE NOT NULL, name, age NOT NULL)
  ```

  This command creates an empty table called `foo` with three columns – guaranteedly unique `id` which must not be `NULL`, arbitrary `name`, and `age` which must again not be `NULL`. (When any of the constraints is violated, the command which violated the constraint should fail.)

* Insert a record into the table:

  ```
  > INSERT INTO foo VALUES (13, 'Tom', 26)
  ```

  If the table doesn't exist, or the number of columns doesn't match, or any of the constraints are violated, the command should fail.

* Delete records from the table:

  ```
  > DELETE FROM foo WHERE name = 'Tom'
  > DELETE FROM foo WHERE age < 18 OR age > 65
  ```

* Select records from the table:

  ```
  > SELECT * FROM foo WHERE name = 'Tom'
  1. id = 13, name = 'Tom', age = 26
  2. id = 30, name = 'Tom', age = 52

  > SELECT name, age FROM foo WHERE name = 'Tom'
  1. name = 'Tom', age = 26
  2. name = 'Tom', age = 52
  ```

That's all.

Note that you don't actually need to implement a parser for queries! I wrote them in SQL so that it would be easier to understand what they do (if you know SQL). For **level one,** you only need to implement commands (e.g. `insert :: ... -> Db -> Either DbError Db`), tests (with `hspec`), and a benchmark suite (with `criterion`). The database will be in-memory.

I would suggest something like `data Value = Null | Number Scientific | String Text | ...` for storing values. Note that data in columns doesn't have to be type safe – it's entirely possible to insert a row with `age = 'foo'`. Also note that you would have to implement an extra type for expressions (such as `age = 3`). Something like `type Predicate = Value -> Bool` would work, but it would make implementing next levels harder.

For **level two,** you need to implement a parser for queries – I suggest `megaparsec`. You also need to implement serialisation for the database, and commands to save and load a database to/from a file. (With `binary` or `cereal` you can do that pretty much automatically.)

For **level three,** implement any of the following:

  * Add indices to make queries on specific columns faster. (Don't forget to benchmark.)

  * Add `JOIN`.

  * Add foreign keys.

  * Allow optional type constraints on columns. Use them to report more errors, as well as to store data more efficiently.

And finally, keep in mind that having implemented this task is more important than having done it “all by yourself” – so ask questions, Google, and don't hesitate to steal others' ideas if they're good.
