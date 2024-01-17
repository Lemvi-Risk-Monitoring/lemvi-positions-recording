# lemvi-positions-recording
Gathering positions from various exchanges

## Deploying to AWS Custom Lambda

### Basic setup

```shell
aws configure
export TF_VAR_aws_lambda_function_name=hal-example-2
export TF_VAR_aws_stage=test
export TF_VAR_aws_region=$(aws configure get region)
export TF_VAR_aws_account_id=$(aws sts get-caller-identity | jq -r '.Account')
```

### Function deployment

```shell
terraform -chdir=terraform apply
```

## Testing deployment

```shell
aws apigateway test-invoke-method \
    --rest-api-id $(terraform output --raw rest_api_id) \
    --resource-id $(terraform output --raw ressource_id) \
    --http-method GET \
    --path-with-query-string /greet?person=Mary | jq '.body'
```

### Testing locally
