# lemvi-positions-recording
Gathering positions from various exchanges

## Deploying to AWS Custom Lambda

### Basic setup

```shell
aws configure
AWS_REGION=us-east-1
AWS_LAMBDA_FUNC_NAME=hal-example-2
```

### Function deployment

```shell
terraform apply
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
