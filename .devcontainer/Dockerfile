FROM public.ecr.aws/lambda/provided:al2023

ARG GHCUP_DWN_URL=https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup
ARG VERSION_GHC=9.4.8
ARG VERSION_CABAL=latest
ARG VERSION_STACK=latest

ARG USER_NAME=haskell
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN \
    dnf install --assumeyes findutils \
        cmake \
        gcc \
        g++ \
        gmp-devel \
        zlib-devel \
        vim \
        sudo \
        git

RUN \
    /usr/bin/curl ${GHCUP_DWN_URL} > /usr/bin/ghcup && \
    chmod +x /usr/bin/ghcup

# Creating the workspace user
RUN /usr/sbin/groupadd --gid ${USER_GID} ${USER_NAME} \
    && /usr/sbin/useradd --uid ${USER_UID} --gid ${USER_GID} --no-log-init --create-home -m ${USER_NAME} -s /usr/bin/bash \
    && /bin/echo ${USER_NAME} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${USER_NAME} \
    && chmod 0440 /etc/sudoers.d/${USER_NAME}

USER ${USER_NAME}

WORKDIR /home/${USER_NAME}

RUN /usr/bin/curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
    && unzip awscliv2.zip \
    && sudo ./aws/install --bin-dir /usr/local/bin --install-dir /usr/local/aws-cli --update \
    && rm -fr awscliv2.zip \
    && rm -fr ./aws

# install GHC, cabal and stack
RUN \
    ghcup -v install ghc --force ${VERSION_GHC} && \
    ghcup -v install cabal --force ${VERSION_CABAL} && \
    ghcup -v install stack --force ${VERSION_STACK} && \
    ghcup set ghc ${VERSION_GHC} && \
    ghcup install hls

RUN /bin/echo -e "\nexport PATH=$PATH:/home/${USER_NAME}/.ghcup/bin:/home/${USER_NAME}/.local/bin/\n" >> /home/${USER_NAME}/.bashrc

SHELL ["/usr/bin/bash", "--login", "-i", "-c"]

CMD [ "sleep", "infinity" ]
