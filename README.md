Clicklac Sample Project
------------

When I started to learn Servant, I felt that the existing sample projects were good, but at the same
time they were missing too much stuff needed to create a realistic API server.

To help others, from my own personal project, I extracted essential parts that I deemed would be useful
in the context of a better bootstrap project with Servant.


Features
-----
* Session ID generation with CPRNG (nonce)
* Session handling with DB 
* Storing hashed and salted passwords (pwstore-fast)
* Cookie support
* Clientside cookie storage (clientsession)
* Protected APIs
* API versioning
* Logging errors in the background (fast-logger)
* User input Validation with Data.Either.Validation
* Producing meaningful error responses in JSON
* Producing JSON responses always for everything instead of text/html
* API documentation generation with servant-docs
* Lifted version of postgresql-simple (I should probably push this as a standalone library to hackage)

Database
----

The Clicklac service talks to two different DBs. It mainly uses Cassandra and 
for only ``/click`` API it uses Postgres. It would have been more befitting to use Postgres as the sole 
DB storage in the context of an example project since most of the time it is good enough for projects and is the more commonly used one. 


DB Installation
--------

1. To install Cassandra, go to http://cassandra.apache.org/download/, grab 2.2.5,
and follow the instructions on http://wiki.apache.org/cassandra/GettingStarted
to install and run one node on your local machine.

2. Run cqlsh and create a key space:
``CREATE KEYSPACE clicklac WITH replication = {'class': 'SimpleStrategy', 'replication_factor': '1'}``  

3. Select key space: ``use clicklac``

4. Execute the .cql file in ``migrations/1_init.cql`` from inside the cqlsh with 
[SOURCE](http://docs.datastax.com/en//cql/3.1/cql/cql_reference/source_r.html)

5. For PostgreSQL installation, refer to your distro's manual. 
After installing Postgres, create a database named "app" with username "app" and password "123456".
Run ``migrations/1_init.sql`` to create one table.

Notes on Coding Practices
--------------
I think Data.Either.Validation is great for public APIs - especilly if you want to inform
API consumers in one response about all fields that do not pass validation. 

You will also notice that I make heavy use of GADTs. The main driver behind this is because
web services always need to have a contract that their users have to obey. Otherwise, you are
leaving holes in your APIs and giving consumers of your API an opportunity to trash your database. 
By parametrizing GADTs based on input state, I can have strong guarantees about what goes into my 
system's database. Instead of GADTs, one can also use two ADTs to deal with unvalidated and 
validated input states. Having said that, I think when you have a specific concept that you can map to a data type and you need to construct different versions of that data type, the use of GADTs feel natural.


Additional Notes
--------------
Content of the "client_session_key.aes" was created by ``Web.ClientSession.randomKey``.
You should create a new key before deploying to production.

Running the project
----------

1. ``cabal sandbox init``
2. ``cabal install --dependencies-only && cabal configure && cabal build``
3. ``cabal run``


API roots:
* ``/auth``  -- authentication
* ``/user``  -- user creation and profile update
* ``/click`` -- posting tweet-like messages

Point your browser to http://localhost:8081/docs to see the detailed API documentation.


Enjoy making API requests with Servant!
