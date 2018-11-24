#!/bin/bash
#ipython notebook
xvfb-run -a jupyter-notebook --no-browser --ip=127.0.0.1 --port=23400 --certfile=/home/hackyeah/mycert.pem 
#--certfile=~/workspace/certbot/mycert.pem 
