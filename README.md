# lemvi-positions-recording
Gathering positions from various exchanges

## Deploying to AWS Custom Lambda

### Basic setup

```shell
aws configure
```

### Preparing a role for execution
 - Roles page in the IAM console --> Create role
 - Select trusted entity: AWS Service, Use case – Lambda
 - Permissions – AWSLambdaBasicExecutionRole

Write down the ARN from Role summary page.

### Creating a zip containing a bootstrap executable
Rename executable to `bootstrap` and add it to a zip file `bootstrap.zip`:
```shell
cp dist-newstyle/build/x86_64-linux/ghc-9.4.8/lemvi-positions-recording-0.1.0.0/x/lemvi-positions-recording/build/lemvi-positions-recording/lemvi-positions-recording /tmp/bootstrap
zip -j /tmp/bootstrap.zip /tmp/bootstrap
```

### Creating the Lambda function
```shell
aws lambda create-function --function-name person-validate --runtime provided.al2023 --handler handler --role arn:aws:iam::857848589999:role/lemvi-positions-recording --zip-file fileb:///tmp/bootstrap.zip 
```

### Testing from AWS console
Configure test event with content:
```json
{
  "personName": "Bob",
  "personAge": 12,
  "other": "sample"
}
```

## Calling from CLI
```shell
AWS_LAMBDA_FUNCTION_NAME=lemvi-positions-recording \
AWS_LAMBDA_FUNCTION_VERSION="2015-03-31" \
AWS_LAMBDA_LOG_STREAM_NAME="root" \
AWS_LAMBDA_LOG_GROUP_NAME="lemvi" \
AWS_LAMBDA_FUNCTION_MEMORY_SIZE=512  \
AWS_LAMBDA_RUNTIME_API=provided cabal run
```
