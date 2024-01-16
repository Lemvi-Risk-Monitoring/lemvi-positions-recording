# lemvi-positions-recording
Gathering positions from various exchanges

## Deploying to AWS Custom Lambda

### Basic setup

```shell
aws configure
AWS_REGION=us-east-1
AWS_LAMBDA_FUNC_NAME=hal-example-2
```

### Creating a zip containing a bootstrap executable
Rename executable to `bootstrap` and add it to a zip file `bootstrap.zip`:
```shell
cp dist-newstyle/build/x86_64-linux/ghc-9.4.8/lemvi-positions-recording-0.1.0.0/x/lemvi-positions-recording/build/lemvi-positions-recording/lemvi-positions-recording /tmp/bootstrap
zip -j /tmp/bootstrap.zip /tmp/bootstrap
```

### Function deployment (TODO: use terraform!)

```shell
EXEC_ROLE_ARN=$(aws iam create-role --role-name lambda-exec --assume-role-policy-document '{"Version": "2012-10-17","Statement": [{ "Effect": "Allow", "Principal": {"Service": "lambda.amazonaws.com"}, "Action": "sts:AssumeRole"}]}' | jq -r '.Role.Arn')
aws iam attach-role-policy --role-name lambda-exec --policy-arn arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole
API_GATEWAY_ROLE_ARN=$(aws iam create-role --role-name api-gateway --assume-role-policy-document '{"Version": "2012-10-17","Statement": [{ "Effect": "Allow", "Principal": {"Service": "apigateway.amazonaws.com"}, "Action": "sts:AssumeRole"}]}' | jq -r '.Role.Arn')
aws iam put-role-policy --role-name api-gateway --policy-name LambdaInvokePolicy --policy-document '{
        "Version": "2012-10-17",
        "Statement": [
            {
                "Effect": "Allow",
                "Action": "lambda:InvokeFunction",
                "Resource": "*"
            }
        ]
    }'

FUNCTION_ARN=$(aws lambda create-function --region ${AWS_REGION} --function-name ${AWS_LAMBDA_FUNC_NAME} --runtime provided.al2023 --handler handler --role ${EXEC_ROLE_ARN} --zip-file fileb:///tmp/bootstrap.zip | jq -r '.FunctionArn')

# For the function to be easily callable you will need to make it accessible through the API Gateway:

REST_API_ID=$(aws apigateway create-rest-api --endpoint-configuration '{ "types": ["REGIONAL"] }' --region ${AWS_REGION} --name api-lambda-${AWS_LAMBDA_FUNC_NAME} | jq -r '.id')
ROOT_RESOURCE_ID=$(aws apigateway get-resources --region ${AWS_REGION} --rest-api-id ${REST_API_ID} | jq -r '.items[] | select(.path == "/").id')
RESOURCE_ID=$(aws apigateway create-resource --region ${AWS_REGION} --rest-api-id ${REST_API_ID} --parent-id ${ROOT_RESOURCE_ID} --path-part 'greet' | jq -r '.id')

aws apigateway put-method --rest-api-id ${REST_API_ID} \
       --resource-id ${RESOURCE_ID} \
       --http-method ANY \
       --authorization-type NONE \
       --region ${AWS_REGION}

aws apigateway put-integration --rest-api-id ${REST_API_ID} \
      --region ${AWS_REGION} \
      --resource-id ${RESOURCE_ID} \
      --http-method ANY \
      --type AWS_PROXY \
      --integration-http-method POST \
      --uri "arn:aws:apigateway:${AWS_REGION}:lambda:path/2015-03-31/functions/${FUNCTION_ARN}/invocations"

aws apigateway put-method-response --rest-api-id ${REST_API_ID} \
      --resource-id ${RESOURCE_ID} \
      --http-method ANY \
      --status-code 200 \
      --response-models '{"application/json": "Empty" }'

aws apigateway create-deployment --rest-api-id ${REST_API_ID} --stage-name test --region ${AWS_REGION} 
```

## Testing locally


