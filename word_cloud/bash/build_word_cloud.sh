#!/usr/bin/env bash

cp -f ../word_cloud/rt_nectar_word_cloud.R ../private/word-cloud

sed -i.bak s/%db_key%/xxxxxxxxxxxxxxxx/ ../private/word_cloud/rt_nectar_word_cloud.R
