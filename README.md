# General description
The main propose of this system is to work as a temporaly on-memory storage system, working as an high performance shared memory cache, reducing the number of reads / writes to the permanent storage system, the Amazon S3, and avoiding the eventual consistency problem of this.<br />
This system is designed to work together with another systems on the same front servers.<br />
In order to obtain the best performance, and the max reduction on the network traffic, the load balancer who distributes the requests between the front servers should be configured as "sticky".<br />
<br/>
# Installation
Config the systems (see the config section), and run:<br />
* Managers: manager/run.sh
* Nodes: nodes/run.sh

Copy the code to all the front servers and to the two manager servers.<br />
[Supervisor](http://pypi.python.org/pypi/supervisor) is recomended in order to guaranty all the process running, and the logs centralized.<br />
<br />
# Configuration
The system is designed to work with Erlang V5.9.2 or higher. Check the [deployment diagram](https://github.com/alonsovidales/Brain/blob/master/docs/brain_deployment_diagram.png).<br />
Each front server should to run an erlang node, use the nodes/run.sh command in order to launch it. The system is designed to work together with the server applications, all the applications on the front server should to communicate with the local node in order to reduce the network load, and the system load.<br />
Two instances should to run the manager code.<br />
On the [config file](https://github.com/alonsovidales/Brain/blob/master/shared/config.hrl) the MASTER_MANAGER and SLAVE_MANAGER consts should to indicate how to connect with each manager on each front server, and you should to set the params to connect with S3.<br />
All the front servers needs read / write access to S3.<br />
A file called .erlang.cookie  and located on the home directory of the user who will run the node should to contain the same string for all the servers, this file will be used as shared secret in order to keep the security of the system.<br />
<br />
# Communcation protocol
The system uses the Erlang messages protocol, for PHP you can use the [Peb](http://code.google.com/p/mypeb/) extension. The allowed messages are:<br />
* {s, &lt;object_id&gt;, &lt;value&gt;} :

>    Create or update the content of an object<br />
>        <b>object_id:</b> string The id of the object to be created / updated<br />
>        <b>value:</b> mixed The content of the object to be setted
* {g, &lt;consistency&gt;, &lt;object_id&gt;, &lt;pid&gt;, null}
    Sends the value of the object if exists to the pid specified as forth parameter of the tupple.<br />
        <b>consistency:</b> true|false Specify true if consistency is neecesary, or false if you only needs eventual consistency<br />
        <b>object_id:</b> string The id of the object to be created / updated<br/>
        <b>pid:</b> The pid of the process who needs the object, use self() if the process is the current process<br />
    Values are sent to the process with the pid specified after the call:<br />
        <b>ko:</b> The object was not found<br />
        <b>{ok, &lt;value&gt;}:</b> The object was found. Value of the value of the object with id object_id<br />
    <br />
    <b>IMPORTANT:</b> Specify a timeout for the response, during the period when one of the nodes crashes, and the nodes ring is rebuilt, the system doesn't retuns anything, this should to be considered a read error.<br />
* {p, &lt;object_id&gt;}
    Dumps an object to S3 immediately without wait.<br />

# License
This is a dual licensed software:<br />
* [MIT](http://opensource.org/licenses/MIT)
* [GPL](http://www.gnu.org/licenses/gpl.html)
