FROM ghcr.io/chris-ch/docker-haskell-aws-lambda:v1.3.0

ARG USER_NAME = haskell

USER ${USER_NAME}

RUN /bin/echo -e "export TF_VAR_aws_stage=test\n" >> /home/${USER_NAME}/.bashrc
RUN /bin/echo -e "export TF_VAR_aws_region=$(aws configure get region)\n" >> /home/${USER_NAME}/.bashrc
RUN /bin/echo -e "export TF_VAR_aws_account_id=$(aws sts get-caller-identity | jq -r '.Account')\n" >> /home/${USER_NAME}/.bashrc

RUN /bin/echo -e "alias terraform=\"~/.local/bin/terraform -chdir=terraform\"\n" >> /home/${USER_NAME}/.bashrc

CMD [ "sleep", "infinity" ]
