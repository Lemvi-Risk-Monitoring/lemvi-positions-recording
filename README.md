# lemvi-positions-recording
Gathering positions from various exchanges

## Calling from CLI
```shell
AWS_LAMBDA_FUNCTION_NAME=lemvi-positions-recording \
AWS_LAMBDA_FUNCTION_VERSION="$LATEST" \
AWS_LAMBDA_LOG_STREAM_NAME="root" \
AWS_LAMBDA_LOG_GROUP_NAME="lemvi" \
AWS_LAMBDA_FUNCTION_MEMORY_SIZE=512  \
AWS_LAMBDA_RUNTIME_API=provided cabal run
```
