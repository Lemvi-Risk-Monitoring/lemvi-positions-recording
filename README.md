# lemvi-positions-recording

Gathering positions from various exchanges

## Deploying to AWS Custom Lambda

The deployment is performed with `terraform`.

The follwing environment variables can be defined within the GitHub repository secrets:

- AWS_ACCESS_KEY_ID
- AWS_DEFAULT_REGION
- AWS_SECRET_ACCESS_KEY
- DERIBIT_CLIENT_ID
- DERIBIT_CLIENT_SECRET
- IB_FLEX_QUERY_ID
- IB_FLEX_REPORT_TOKEN

### Basic setup

The access and secret keys are generated from the AWS account (top left, "Security credentials" and then "Create access key").

Environment variables for `terraform`:

```shell
export TF_VAR_aws_stage=test
export TF_VAR_aws_region=$(aws configure get region)
export TF_VAR_aws_account_id=$(aws sts get-caller-identity | jq -r '.Account')
```

AWS settings can be overriden using:

```shell
aws configure
```

### Building

```shell
cabal build
```

### Function deployment

```shell
terraform -chdir=terraform apply -var "ib_flex_report_token=$IB_FLEX_REPORT_TOKEN"
```

## Testing deployment

```shell
aws apigateway test-invoke-method \
    --rest-api-id $(terraform -chdir=terraform output --raw rest_api_id) \
    --resource-id $(terraform -chdir=terraform output --raw ressource_id) \
    --http-method GET \
    --path-with-query-string /greet?person=Mary | jq -r '.body'
```

```shell
aws aws lambda invoke \
    --function-name ibrokers-app \
    --payload '{ "ib_flex_query_id": "906041" }' \
    --cli-binary-format raw-in-base64-out \
    out.log

```

### Testing locally

Starting the Warp server:

```shell
cabal run local-app
```

```shell
curl 127.0.0.1:8080/greet?person=John
```

```shell
cabal run deribit-local

sudo rm -fr /tmp/http.log && sudo tcpdump -i any -w /tmp/http.log

curl -X POST -H "Content-Type: application/json" -d '{"currencies":["dummy"]}' http://localhost:8080/local


sudo tcpdump -A -r /tmp/http.log tcp port 80
```
