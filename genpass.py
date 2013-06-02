#!/usr/bin/python

import time
import base64
import hashlib
import os

hash_out = base64.urlsafe_b64encode(hashlib.md5(str(os.urandom(36))).digest())
print hash_out[:-2].replace('-', ',').replace('_', '.')
