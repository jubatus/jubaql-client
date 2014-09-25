JubaQL Client
=============

How to compile and start the JubaQL Client
------------------------------------------

* Get the source code:  
  `git clone https://github.com/jubatus/jubaql-client.git`
* Compile it:  
  `cd jubaql-client && sbt start-script && cd ..`
* Start the JubaQLClient:  
  `./jubaql-client/target/start`

Other usage instructions
------------------------

* When you see the prompt `jubaql>` in the shell, you are able to type your commands there, but until the JubaQLProcessor is up and running correctly, you will get the message "Unexpected response status: 503".
* To use a remote server, pass `-h hostname` as a parameter.
* When the `SHUTDOWN` command is issued, the JubaQL processor instance is stopped and the session closed. Therefore, the client will also terminate after that.
* When the client starts up, it will output a message such as

    > Using session id "u10nxrrkft29h51vb17u"

    This session id is a 1:1 mapping to your running instance of the JubaQL processor. Even after you leave the client using the `exit` command or the `Ctrl+D` shortcut, you can re-enter that session by giving it as a parameter to the client:  
    `./jubaql-client/target/start u10nxrrkft29h51vb17u`

