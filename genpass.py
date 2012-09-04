#!/usr/bin/python

import time
import base64
import hashlib

hash_out = base64.urlsafe_b64encode(hashlib.md5(str(time.time())).digest())
print hash_out[:-2].replace('-', ',').replace('_', '.')
