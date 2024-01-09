FROM ghcr.io/chris-ch/docker-ghcup:v1.0.9

#RUN /bin/echo -e "\nsource ~/.bashrc\n" >> ${HOME}/.bash_profile

RUN /usr/bin/curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash

SHELL ["/bin/bash", "--login", "-i", "-c"]

RUN source ${HOME}/.bashrc && nvm install node

CMD [ "sleep", "infinity" ]
