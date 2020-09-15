# REST API using WAI(Web Application Interface) with Warp

This is a simple web-server, which provides capabilities for publishing the news.
For now we have several essences, whose number, i guess, ease to increase: person, author, category, tag, draft, news and comments to news. In section "For followers" it's get in detail.

## How to install

### Step 1 - installing PostgreSQL(PSQL)

#### Download

[Site for download](https://www.postgresql.org/download/)
[Tutorial](https://youtu.be/qw--VYLpxG4?t=861)

#### src/Psql.json

After download PSQL, open src/Psql.json and change, if something is difference.

```json
{ "userName":"postgres"
, "password":"0000"
, "server":"localhost"
, "port":"5432"
, "database":"webserv"
, "page": 10
}
```

1. userName : default is postgres.
2. password : password for PostgreSQL, which you entered in installation.
3. server : default is localhost.
4. port : default is 5432.
5. database : name of database, which you wanna use for server, .
6. page : The maximum number of essences that fit on the page (with the <essence>/get paht of request). If no page number is specified, the first is used by default.

### Step 2 - building and testing
In the root folder of the repository, open git bash and write the following commands.
```git
$ stack build
```
Now you need create tables un your database, before testing.
```git
$ stack exec server-build
```
For rebuilding tables, it's deleting all tables with data.
```git
$ stack exec server-rebuild
```
This way you will do a little check functions and it also checks the basic functionality for accessing the database, so you need check after it logs/.
```git
$ stack test
```
To quickly check the server operation in Test curl/ there are .sh scripts, but they must be used in a specific order. First, you always need to create a person - personCreate.sh, He will be with admin rights, so then you need to create an author - authorCreate.sh, a tag and a category - tagCreate.sh, categoryCreate.sh.
Then you can create a draft - draftCreate.sh and publish it, at the first publication - draftFirstPublish.sh, for subsequent ones, if you want to change it - draftNextPublish.sh. And now you can create a comment for the news - commentCreate.sh.

After creating the entities, you may want to change them - <essence>Edit.sh, if there is no suitable script, you cannot change only person and news, to change news, you need to change the draft and publish it - draftNextPublish.sh.

Also, after creation, you can get a list of entities - <essence> Get.sh.

When you want to test deleting entities, you need to do it in the reverse order of entity creation. Namely - commentDelete.sh -> newsDelete.sh -> draftDelete.sh -> authorDelete.sh/tagDelete.sh/categoryDelete.sh -> personDelete.sh.

If problems arise at this or subsequent stages. I left my contacts it the end of README.

### Step 3 - explanation of work

#### Request

Request looks like that: http://<localhost>:<port>/<essence>/<action>?<params>
Port writing in App/Main.hs after Wai.run. For creating person: http://localhost:8000/person/create?first_name=firstName&last_name=lastName

#### src/Local.json

```json
{ "logLevel":"debug"
}
```

##### Details of Local.json

1. logLevel: during the execution of the program, on the passage of key and not only moments, logs will be sent to the console. Only 4 levels: DEBUG, INFO, WARNING, ERROR.
   - debug is the most insignificant.
   - infog - reports on key points.
   - warning - warns of possible errors.
   - error - indicates a critical error, which prevents further execution.
debug < infog < warning < error. Thus, by entering a certain level of logging into the value of the "logLevel" field, the application will send logs whose level is either equal to or higher than the specified level.

#### src/Api.json

```json
{ "api":
    { "create":"create"
    ...
    }
, "method":
    { "POST":
        [ "create"
        ...
        ]
    , "GET":
        [ "get"
        ]
    }
, "essences":
    [ "person"
    ...
    ]
, "relations":
    { "person":"access_key"
    , "author":
        { "person_id": []
        }
    , "draft":
        { "author_id":
            [ "author_id"
            ]
        }
    ...
    }
, "access":
    { "person":
        { "create": "Everyone"
        , "get": "Everyone"
        , "delete": "Admin"
        }
    ...
    }
, "news":
    { "filter_created_in":["DATE"]
    ...
    }
}
```

##### Details of Api.json

1. api: request takes field, and it's will translate into value.
{ "publish":"create" } - "publish" => "create". The database can only create, edit, delete and get.
2. method: each request method has only one possible essence method.
{ "GET": "get" }
3. essences: all essences.
4. relations: for example, if a draft is created, I do not write my id as an author in the request parameters, this is the field that should be added in accordance with the logic written in src / Data.Essence.RelationsTree.Methods.hs. For person: {"person": "access_key"}. Everything ends with person - through the access_key, which is passed in the request, the person from the request is found and then the author_id is found and returned, and then added to the Essence List.
5. access: each essence has its own methods, and each method has its own access.
6. news: for news, there are additional fields that can be filled in when received as a value. For example, {"filter_created_in": ["DATE"]} the query might look like this: http://localhost:8000/news/get?Filter_created_in=20-20-2020.

## Run

You can start the application from git bash with this line.
```git
$ stack exec server-exe
```
To stop an application launched in this way, first select the git bash window as the active one and press crtl + c.
If you closed git bash before the application stopped, then it will continue to work until you shut it down, for example, through the task manager.
Also after the assembly bot-exe.exe appeared, it is located in server/.stack-work/install/<something>/bin.

## For followers

### Architecture

Basically it is ReaderT Config IO = UnderApp. Then StateT s UnderApp appears on top, where s is usually an entity with three fields: elName is the name, elAction is the action that will eventually be executed in the database, and elList is a list of pairs of fields and values ​​in a custom data type, for convenience.

### Modules
* src/Config.hs - Handle, App types, testConfig and testHandle.
    - src/Config/Get.hs - Functions for getting certain values ​​from the config.
    - src/Config/Set.hs - Setting config from Config.json and <Bot>.json.
    - src/Config/Internal.hs - Newtypes and type synonyms for config objects.
* src/Setup.hs - Functions for first setup: building database and Config.json.
* src/App.hs - Function for server operation: processes the request and sends a response.
* src/Log.hs - Imports all Log's module from /src/Log.
    - src/Log/Level.hs - data type.
    - src/Log/Console.hs - Functions for logging.
    - src/Log/File.hs - Functions for writing logs in file.
    - src/Log/Handle.hs - data type.
        - src/Log/Handle/Builder.hs - For building Log.Handle
* src/Tests.hs - a function that collects tests that are written in src/Tests.
* src/Database
    - /Create.hs - Function for creating essence in database.
    - /Delete.hs - Function for deleting essence in database.
    - /Edit.hs - Function for editing essence in database.
    - /Exception.hs - Functions for handling errors when working with a database.
    - /Get.hs - Instead of one function, there are many of them, since in the case of news, nested entities need to be returned to the request. Also, several functions play a secondary role when you need to get an entity from the database, for example, to check the correct relationship between the author and the draft.
    - /Test.hs - Functions that are only needed to test functions that create, modify, get, and delete essences.
* src/Data
    - /Base.hs - Functions that can be used in any module. For greater convenience, the functions are divided into modules in src/Data/Base/ and all are imported into Base.hs.
        - /Aeson.hs - Functions with Value from aeson package.
        - /Interface.hs - ask, get and other functions from Control.Monad ... that are needed to work with the architecture. It is better to change the names for a clearer use. For example, ask is used for UnderApp, which is ReaderT Config IO, so it's clearer to call it askUnderApp.
        - /Lift.hs - lift functions which i rename for better understanding.
        - /Path.hs - Functions for parsing FilePath.
        - /Prelude.hs - Functions for which Prelude imports are sufficient.
        - /Random.hs - Functions that give random values.
        - /Response.hs - Functions for creating a response to a request.
        - /Text.hs - Functions for Text from package text.
        - /Time.hs - Functions that require import from package time.
    - /Essence.hs - Essence data family with instances.
        - /GetFields.hs - class GetFields for getting fields from Essence Description and Required fields.
        - /Methods.hs - Functions for working with instances of family Essence.
        - /Parse.hs - Functions for parsing essence from json.
            - /Clause.hs - Functions for parsing clauses, which are needed for a more specialized query to the database.
        - /RelationsTree.hs - data RelationsTree with instances.
            - /Methods.hs - Functions for processing a tree of relations. Basically, these are functions for collecting fields from a database, based on entity relationships and the available data obtained from the query.
    - /Request.hs - Functions for handling request to get specific values.
        - /Access.hs - data Access with instances. It is necessary for the separation of rights.
            -/IsRight.hs - Function for checking access.
        - /Control.hs - Query validation functions: value types, required fields, required access, and more.
        - /Handling.hs - One of the main modules, which handle request and choose database method, and getting response.
        - /Method.IsRight.hs - Function for checking right method of essence.
        - /Params.Methods.hs - Functions for working with params of request.
    - /Required.hs - data Required and instances.
        - /Methods.hs - Functions for working with Required fields.
    - /SQL.hs - data SqlQuery, data family Clause and instances.
        - /ShowSql.hs - class ShowSql for translate SqlQuery, Essence List and others to SqlRequest.
        - /ToValue.hs - Functions for translating SqlValue from HDBC package to Value from aeson package.
    - /Empty.hs - class Empty for parsing value to correct format for database and instances.
    - /MyValue.hs - data MyValue with instances and functions for converting from string, to string, from Value, to Value and others.
    - /Value.hs - Functions for unpacking Value from aeson package.

### Adding new essences

First, it is worth examining the existing relationship between entities. And then, repeating the structure of other entities in the src/Setup/<essence>.json, add your own.

## Packages

* [base](https://hackage.haskell.org/package/base) - >= 4.7 && < 5
* [random](https://hackage.haskell.org/package/random) - For random names in testing.
* [aeson](https://hackage.haskell.org/package/aeson) - For unpacking and packaging json, as well as for tests.
* [bytestring](https://hackage.haskell.org/package/bytestring) - For the correct reading of json from the file and receiving requests.
* [http-types](https://hackage.haskell.org/package/http-types) - For status response and types.
* [text](https://hackage.haskell.org/package/text) - For wrapping strings to use functions from aeson.
* [HUnit](https://hackage.haskell.org/package/HUnit) - It’s easy to write tests with this.
* [vector](https://hackage.haskell.org/package/vector), [unordered-containers](https://hackage.haskell.org/package/unordered-containers) - For constructing Value from aeson.
* [scientific](https://hackage.haskell.org/package/scientific) - For translation to integer.
* [transformers](https://hackage.haskell.org/package/transformers) - For architecture.
* [directory](https://hackage.haskell.org/package/directory) - To read the config regardless of the position of the repository on disk.
* [time](https://hackage.haskell.org/package/time) - Naming logs
* [HDBC](https://hackage.haskell.org/package/HDBC) - For working with database.
* [HDBC-postgresql](https://hackage.haskell.org/package/HDBC-postgresql) - For working with PostgreSQL.
* [parsec](https://hackage.haskell.org/package/parsec) - For avoiding read function. This is mainly used to translate an entity from json into a SQL query.

## Authors

* **Misha Snisarenko**
My email - mishasnis@gmail.com
