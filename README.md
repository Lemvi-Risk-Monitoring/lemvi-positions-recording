# lemvi-positions-recording

Gathering positions from various exchanges

## Deploying to AWS Custom Lambda

The deployment is performed with `terraform`.

### Basic setup

The access and secret keys are generated from the AWS account (top left, ""Security credentials" and then "Create access key").

```shell
aws configure
```

Environment variables for `terraform` are set by calling a script at the end of the Dockerfile:

```shell
export TF_VAR_aws_stage=test
export TF_VAR_aws_region=$(aws configure get region)
export TF_VAR_aws_account_id=$(aws sts get-caller-identity | jq -r '.Account')
```

### Building

```shell
cabal build
```

### Function deployment

```shell
terraform -chdir=terraform apply
```

## Testing deployment

```shell
aws apigateway test-invoke-method \
    --rest-api-id $(terraform -chdir=terraform output --raw rest_api_id) \
    --resource-id $(terraform -chdir=terraform output --raw ressource_id) \
    --http-method GET \
    --path-with-query-string /greet?person=Mary | jq -r '.body'
```

### Testing locally

Starting the Warp server:

```shell
cabal run local-app
```

```shell
curl 127.0.0.1:8080/greet?person=John
```
