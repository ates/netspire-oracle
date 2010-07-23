Oracle database driver for Netspire
===================================

1. Overview
1. Required software
1. Compilation
1. Configuration


1. Overview
-----------
Be aware that this software is experimental and may contain bugs and not implemented features.  
The core of the driver is written in C language, using Oracle OCI library, and provides the NIF interface for the Erlang.  
Now the driver has some limitation, such as not supporting some data types and so on.

2. Required software
--------------------
1. The Erlang version, which is not less than R14A
2. The OCI shared libraries and the header files

3. Compilation
--------------
1. Download InstantClient and InstantClient-SDK from Oracle site
1. Unzip it to /usr/lib/ for the shared libraries and to /usr/include/ for the header files
1. Run ldconfig
1. Go to the netspire-oracle folder and perform make
1. Sources will be compiled without errors/warnings and netspire_oracle_drv.so will be copied to ../netspire-core/priv/lib folder.  
The beam files will be copied to ../netspire-core/ebin too.

4. Configuration
----------------
The following line needs to be added to the **netspire.conf** file:

    {mod_oracle, ["username", "password", "connection_string"]}

It is possible to use two types of the connection string:

1. Short name like *db1* or *my_database*
1. Full name like:

    (DESCRIPTION = (ADDRESS_LIST = (ADDRESS =(PROTOCOL = TCP)(HOST = hostname)(PORT = 1521))) (CONNECT_DATA = (SERVICE_NAME = my_service_name) (SERVER=DEDICATED)))"

To use short name you need to setup the ORACLE\_HOME variable and create the $ORACLE_HOME/network/admin/tnsnames.ora file
