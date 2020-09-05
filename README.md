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

#### src/Local.json.

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

#### Telegram

For telegrams of the bot, after starting the bot with the command below. You just need to write to the bot. To find it, you need to use the name you gave it when you created it.

## Run

You can start the application from git bash with this line.
```git
$ stack exec bot-exe
```
To stop an application launched in this way, first select the git bash window as the active one and press crtl + c.
If you closed git bash before the application stopped, then it will continue to work until you shut it down, for example, through the task manager.
Also after the assembly bot-exe.exe appeared, it is located in bot\\.stack-work\install\<something>\bin.

## For followers

### Architecture

Basically it is StateT Config IO (). Since the config has to be changed in accordance with each message.

ReaderT appears at the message processing level.

### Modules

* src/Session.hs - business logic.
* src/Base.hs - Functions that can be used in any module.
* src/Request.hs - requests for logic, the bot accepts 3 requests: 1 - initial, 2 - asking, 3 - responding.
    - src/Request/Modify.hs - functions for modify request.
* src/Config.hs - Handle, App types, testConfig and testHandle.
    - src/Config/Get.hs - Functions for getting certain values ​​from the config.
    - src/Config/Set.hs - Setting config from Config.json and <Bot>.json.
    - src/Config/Update.hs - Functions for changing the config..
* src/Bot/<Bot>.hs - a certain functionality peculiar to this bot is prescribed.
* src/Log.hs - imports all Log's module from /src/Log.
    - src/Log/Level.hs - data type.
    - src/Log/Console.hs - Functions for logging.
    - src/Log/File.hs - Functions for writing logs in file.
    - src/Log/Handle.hs - data type.
        - src/Log/Handle/Builder.hs - For building Log.Handle
* src/Tests.hs - a function that collects tests that are written in src/Tests.

### Adding new bots

Let the bot's name be Li. You need to create a folder in src/Bot/Li and the file src/Bot/Li/Li.json in it. And fill it in according to other <Bot>.json.

## Packages

* [base](https://hackage.haskell.org/package/base) - >= 4.7 && < 5
* [random](https://hackage.haskell.org/package/random) - Almost all requests require a random identifier.
* [aeson](https://hackage.haskell.org/package/aeson) - For unpacking and packaging json, as well as for tests.
* [bytestring](https://hackage.haskell.org/package/bytestring) - For the correct reading of json from the file and sending requests.
* [http-client](https://hackage.haskell.org/package/http-client),[http-conduit](https://hackage.haskell.org/package/http-conduit) - Libraries for http requests, [Tutorial](https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md)
* [text](https://hackage.haskell.org/package/text) - For wrapping strings to use functions from aeson.
* [HUnit](https://hackage.haskell.org/package/HUnit) - It’s easy to write tests with this.
* [vector](https://hackage.haskell.org/package/vector), [unordered-containers](https://hackage.haskell.org/package/unordered-containers) - For constructing Value from aeson.
* [transformers](https://hackage.haskell.org/package/transformers) - For architecture.
* [directory](https://hackage.haskell.org/package/directory) - To read the config regardless of the position of the repository on disk.
* [time](https://hackage.haskell.org/package/time) - Naming logs

## Authors

* **Misha Snisarenko**
My email - mishasnis@gmail.com
