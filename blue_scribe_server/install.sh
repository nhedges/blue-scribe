#!/bin/bash

cp -a _build/default/rel/blue_scribe_server /opt/
cp blue_scribe.service /etc/systemd/system
