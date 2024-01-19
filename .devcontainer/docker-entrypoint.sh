#!/usr/bin/env bash

echo "setting Terraform environment variables"
export TF_VAR_aws_stage=test
export TF_VAR_aws_region=$(aws configure get region)
export TF_VAR_aws_account_id=$(aws sts get-caller-identity | jq -r '.Account')

exec "$@"
