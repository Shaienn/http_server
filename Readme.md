* `http_server` - http_server application itself
* `http_server_test_stand` - the stand to do benchmark for the http_server
  
## User Guide

### HTTP server

Simple `HTTP` server which (by default) uses `localhost` on port `8000`. Any `HTTP` request to the server starts reading file (/tmp/download_test by default).
If job is done during of alowed time slot (5 seconds by default) then the server sends `HTTP` response with the status `200` and some `html` content.
If file has not been read during of the time slot then server sends `HTTP` response with the status code `503`.
If reading is interrupted due to some other reason then server sends `HTTP` response with the status `500`.

#### How to use

Go to the `http_server` folder and do `make run`. When application is compiled and started the functionality can be tested from any `http` client including standard web-browser
by request to the `http://localhost:8000/` address. The file `/tmp/download_test` will be created during of compilation (it is defined in Makefile).

### HTTP server test stand

The test stand for the `http_server` which allows to create specified number of connections to the server with specified rate per second.

#### How to use

Go to the `http_server_test_stand` folder and do `make run`. The stand will start `http_server` automatically. 
When application is compiled and started the `http_server` can be tested using the following  API:

`http_server_test_stand_app:run_load(<X>, <Y>).`

Where:

  X - is total number of connections to be created;
  
  Y - number of connections to be created per second.
  
Example:

`http_server_test_stand_app:run_load(10, 1).` - will start 1 connection per second until 10 connections are not started.

When test is complete then report is generated:

```
--------- REPORT START ---------
Started at: 30 Jun 2021 13:14:58
Ended at: 30 Jun 2021 13:15:08
--------------------------------
Counters:
   Total requests sent: 10
   Successfull requests: 10
   Failed requests: 0
   Requests with status code 200: 10
   Requests with status code 500: 0
   Requests with status code 503: 0
---------- REPORT END ----------
```

It shows time range of the test and corresponding counter values. 
It helps to understand how much requests were sent, how much were successfull and how much have done the job.

