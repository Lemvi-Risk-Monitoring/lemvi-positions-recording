variable "function_name" {
  type = string
  default = "hal-example-2"
}

variable "aws_region" {
  type = string
  default = "us-east-1"
}

data "archive_file" "lambda_package" {
  type = "zip"
  source_file = "./dist-newstyle/build/x86_64-linux/ghc-9.4.8/lemvi-positions-recording-0.1.0.0/x/aws-app/build/aws-app/aws-app"
  output_path = "/tmp/bootstrap.zip"
}

resource "aws_lambda_function" "hal_lambda" {
  filename = "/tmp/bootstrap.zip"
  function_name = var.function_name
  role = aws_iam_role.lambda_role.arn
  handler = "handler"
  runtime = "provided.al2023"
  source_code_hash = data.archive_file.lambda_package.output_base64sha256
}

resource "aws_iam_role" "lambda_role" {
  name = "lambda-exec"

  assume_role_policy = jsonencode({
    Version = "2012-10-17",
    Statement = [
        {
            Action = "sts:AssumeRole",
            Effect = "Allow",
            Principal = {
                Service = "lambda.amazonaws.com"
            }
        }
    ]
    })
}

resource "aws_api_gateway_rest_api" "hal_api" {
  name = "api-lambda-${var.function_name}"
  description = "HAL API Gateway"

  endpoint_configuration {
    types = ["REGIONAL"]
  }
}

resource "aws_api_gateway_resource" "root" {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  parent_id = aws_api_gateway_rest_api.hal_api.root_resource_id
  path_part = "greet"
}

resource "aws_api_gateway_method" "proxy" {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = "ANY"
  authorization = "NONE"
}

resource "aws_api_gateway_integration" "lambda_integration" {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = aws_api_gateway_method.proxy.http_method
  uri = aws_lambda_function.hal_lambda.invoke_arn
  integration_http_method = "POST"
  type = "AWS_PROXY"
}

resource "aws_api_gateway_method_response" "proxy" {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = aws_api_gateway_method.proxy.http_method
  status_code = "200"
  response_models = {"application/json": "Empty" }
}

resource "aws_api_gateway_integration_response" "proxy" {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = aws_api_gateway_method.proxy.http_method
  status_code = aws_api_gateway_method_response.proxy.status_code

  depends_on = [
    aws_api_gateway_method.proxy,
    aws_api_gateway_integration.lambda_integration
  ]
}

resource "aws_api_gateway_deployment" "deployment" {
  depends_on = [
    aws_api_gateway_integration.lambda_integration
  ]

  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  stage_name = "test"
}
