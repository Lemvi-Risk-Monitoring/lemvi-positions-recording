
locals {
  project_name_version = "lemvi-positions-recording-0.1.0.0"
  rest_api_name        = "aws-app"
  dist_path            = "dist-newstyle/build/x86_64-linux/ghc-9.4.8"
  lambda_exe           = "${path.cwd}/${local.dist_path}/${local.project_name_version}/x/${local.rest_api_name}/build/${local.rest_api_name}/${local.rest_api_name}"
  api_description      = "HAL API Gateway"
  api_url_path         = "greet"
}

resource "null_resource" "prepare_bootstrap" {
  provisioner "local-exec" {
    command = "mkdir -p /tmp/${local.rest_api_name} && cp ${local.lambda_exe} /tmp/${local.rest_api_name}/bootstrap && strip /tmp/${local.rest_api_name}/bootstrap"
  }
}

data "archive_file" "lambda_package" {
  type        = "zip"
  output_path = "/tmp/bootstrap-${local.rest_api_name}.zip"
  source_file = "/tmp/${local.rest_api_name}/bootstrap"

  depends_on = [
    resource.null_resource.prepare_bootstrap
  ]
}

resource "aws_lambda_function" "hal_lambda" {
  filename         = data.archive_file.lambda_package.output_path
  function_name    = var.aws_lambda_function_name
  role             = aws_iam_role.lambda_role.arn
  handler          = "handler"
  runtime          = "provided.al2023"
  source_code_hash = data.archive_file.lambda_package.output_base64sha256
}

# IAM
data "aws_iam_policy_document" "assume_role_lambda" {
  statement {
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }

    actions = ["sts:AssumeRole"]
  }
}

resource "aws_iam_role" "lambda_role" {
  name               = "lambda-exec"
  assume_role_policy = data.aws_iam_policy_document.assume_role_lambda.json
}

data "aws_iam_policy" "lambda_logs_policy" {
  arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}

resource "aws_iam_role_policy_attachment" "lambda_logs_policy_attachment" {
  role       = aws_iam_role.lambda_role.name
  policy_arn = data.aws_iam_policy.lambda_logs_policy.arn
}

resource "aws_api_gateway_rest_api" "rest_api" {
  name        = "api-lambda-${var.aws_lambda_function_name}"
  description = local.api_description

  endpoint_configuration {
    types = ["REGIONAL"]
  }
}

resource "aws_lambda_permission" "api_gateway_invoke" {
  statement_id  = "AllowExecutionFromAPIGateway"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.hal_lambda.function_name
  principal     = "apigateway.amazonaws.com"
  # DANGER ZONE: USE FULL * permissions, otherwise getting error
  # "Execution failed due to configuration error: Invalid permissions on Lambda function"
  source_arn = "${aws_api_gateway_rest_api.rest_api.execution_arn}/*/*"
  #source_arn = "arn:aws:execute-api:${var.aws_region}:${var.aws_account_id}:${aws_api_gateway_rest_api.rest_api.id}/*/*" 
}

resource "aws_api_gateway_resource" "root" {
  rest_api_id = aws_api_gateway_rest_api.rest_api.id
  parent_id   = aws_api_gateway_rest_api.rest_api.root_resource_id
  path_part   = local.api_url_path
}

resource "aws_api_gateway_method" "proxy" {
  rest_api_id   = aws_api_gateway_rest_api.rest_api.id
  resource_id   = aws_api_gateway_resource.root.id
  http_method   = "ANY"
  authorization = "NONE"
}

resource "aws_api_gateway_integration" "lambda_integration" {
  rest_api_id             = aws_api_gateway_rest_api.rest_api.id
  resource_id             = aws_api_gateway_resource.root.id
  http_method             = aws_api_gateway_method.proxy.http_method
  uri                     = aws_lambda_function.hal_lambda.invoke_arn
  integration_http_method = "POST"
  type                    = "AWS_PROXY"
}

resource "aws_api_gateway_method_response" "proxy" {
  rest_api_id     = aws_api_gateway_rest_api.rest_api.id
  resource_id     = aws_api_gateway_resource.root.id
  http_method     = aws_api_gateway_method.proxy.http_method
  status_code     = "200"
  response_models = { "application/json" : "Empty" }
}

resource "aws_api_gateway_integration_response" "proxy" {
  rest_api_id = aws_api_gateway_rest_api.rest_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = aws_api_gateway_method.proxy.http_method
  status_code = aws_api_gateway_method_response.proxy.status_code

  depends_on = [
    aws_api_gateway_method.proxy,
    aws_api_gateway_integration.lambda_integration
  ]
}

resource "aws_api_gateway_deployment" "deployment" {
  rest_api_id = aws_api_gateway_rest_api.rest_api.id
  stage_name  = var.aws_stage

  depends_on = [
    aws_api_gateway_integration.lambda_integration
  ]
}
