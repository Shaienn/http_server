PROJECT = http_server
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
PROJECT_ENV = [{port, 8000}, {job_file, "/tmp/download_test"}, {job_timeout, 5000}]

include erlang.mk

all:: deps app rel job_file

job_file:
	dd if=/dev/zero of=/tmp/download_test bs=1M count=500