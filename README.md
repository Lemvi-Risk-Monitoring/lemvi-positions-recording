# lemvi-positions-recording

Gathering positions from various exchanges

## Deploying to AWS Custom Lambda

The deployment is performed with `terraform`.

The following environment variables can be defined within the GitHub repository secrets:

- AWS_ACCESS_KEY_ID
- AWS_DEFAULT_REGION
- AWS_SECRET_ACCESS_KEY
- DERIBIT_CLIENT_ID
- DERIBIT_CLIENT_SECRET
- IB_FLEX_QUERY_ID
- IB_FLEX_REPORT_TOKEN
- IB_PGP_PASS_KEY

### Basic setup

The access and secret keys are generated from the AWS account (top left, "Security credentials" and then "Create access key").

Environment variables for `terraform`:

```shell
export TF_VAR_aws_stage=test
export TF_VAR_aws_region=$(aws configure get region)
export TF_VAR_aws_account_id=$(aws sts get-caller-identity | jq -r '.Account')
export TF_VAR_aws_region="$AWS_DEFAULT_REGION"
export TF_VAR_deribit_client_id="$DERIBIT_CLIENT_ID"
export TF_VAR_deribit_client_secret="$DERIBIT_CLIENT_SECRET"
export TF_VAR_ib_flex_report_token="$IB_FLEX_REPORT_TOKEN"
export TF_VAR_ib_pgp_pass_key="$IB_PGP_PASS_KEY"
export TF_VAR_ib_ftp_username="$IB_FTP_USERNAME"
export TF_VAR_ib_ftp_password="$IB_FTP_PASSWORD"
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

```shell
aws lambda invoke \
    --function-name test-ibrokers-request-lambda \
    --payload '{ "flexQueryId": "906041" }' \
    --cli-binary-format raw-in-base64-out \
    out.log

```

### Testing locally

Starting the Warp server:

```shell
cabal run app-local
```

```shell
curl 127.0.0.1:8080/greet?person=John
```

```shell
cabal run deribit-local

sudo rm -fr /tmp/http.log && sudo tcpdump -i any -w /tmp/http.log

curl -X POST -H "Content-Type: application/json" -d '{"currencies":["BTC", "ETH", "USDC"]}' http://localhost:8080/local


sudo tcpdump -A -r /tmp/http.log tcp port 80
```

Straight scripts:

```shell
IBROKERS_BUCKET_POSITIONS=test-ibrokers-positions-857848589999 IB_FTP_SERVER=ftp2.interactivebrokers.com IB_PGP_PRIVATE_KEY_PATH=dummy cabal run ibrokers-ftp-local
```

### Unit testing

```shell
HSPEC_COLOR=yes cabal test --test-show-details="streaming" --keep-going --test-option=--match --test-option="AWSEvent"
```
