#!/usr/bin/env bash
echo STARTUP SCRIPT EXECUTING...

USER_NAME=haskell

/bin/echo -e "export TF_VAR_aws_stage=test\n" >> /home/${USER_NAME}/.bashrc
/bin/echo -e "export TF_VAR_aws_region=us-east-1\n" >> /home/${USER_NAME}/.bashrc
/bin/echo -e "export TF_VAR_aws_account_id=$(aws sts get-caller-identity | jq -r '.Account')\n" >> /home/${USER_NAME}/.bashrc

/bin/echo -e "alias terraform=\"~/.local/bin/terraform -chdir=terraform\"\n" >> /home/${USER_NAME}/.bashrc
