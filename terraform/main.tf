variable aws_lambda_function_name {
  type = string
}

variable aws_region {
  type = string
}

variable aws_stage {
  type = string
}

provider aws {
  region = var.aws_region
}

locals {
    lambda_exe = "${path.cwd}/dist-newstyle/build/x86_64-linux/ghc-9.4.8/lemvi-positions-recording-0.1.0.0/x/aws-app/build/aws-app/aws-app"
}

resource null_resource copy_file {
  provisioner "local-exec" {
    command = "cp ${local.lambda_exe} /tmp/bootstrap"
  }
}

data archive_file lambda_package {
  type        = "zip"
  output_path = "/tmp/bootstrap.zip"
  source_file = "/tmp/bootstrap"
  depends_on = [
    resource.null_resource.copy_file
  ]
}

resource aws_lambda_function hal_lambda {
  filename = data.archive_file.lambda_package.output_path
  function_name = var.aws_lambda_function_name
  role = aws_iam_role.lambda_role.arn
  handler = "handler"
  runtime = "provided.al2023"
  source_code_hash = data.archive_file.lambda_package.output_base64sha256
}

# IAM
data aws_iam_policy_document assume_role_lambda {
  statement {
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }

    actions = ["sts:AssumeRole"]
  }
}

resource aws_iam_role lambda_role {
  name = "lambda-exec"
  assume_role_policy = data.aws_iam_policy_document.assume_role_lambda.json
}

resource aws_api_gateway_rest_api hal_api {
  name = "api-lambda-${var.aws_lambda_function_name}"
  description = "HAL API Gateway"

  endpoint_configuration {
    types = ["REGIONAL"]
  }
}

resource aws_lambda_permission api_gateway_invoke {
  statement_id  = "AllowExecutionFromAPIGateway"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.hal_lambda.function_name
  principal     = "apigateway.amazonaws.com"
  # DANGER ZONE: USE FULL * permissions, otherwise getting error
  # "Execution failed due to configuration error: Invalid permissions on Lambda function"
  source_arn = "${aws_api_gateway_rest_api.hal_api.execution_arn}/*/*"
  #source_arn = "arn:aws:execute-api:${var.aws_region}:${var.aws_account_id}:${aws_api_gateway_rest_api.hal_api.id}/*/*" 
}

resource aws_api_gateway_resource root {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  parent_id = aws_api_gateway_rest_api.hal_api.root_resource_id
  path_part = "greet"
}

resource aws_api_gateway_method proxy {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = "ANY"
  authorization = "NONE"
}

resource aws_api_gateway_integration lambda_integration {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = aws_api_gateway_method.proxy.http_method
  uri = aws_lambda_function.hal_lambda.invoke_arn
  integration_http_method = "POST"
  type = "AWS_PROXY"
}

resource aws_api_gateway_method_response proxy {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = aws_api_gateway_method.proxy.http_method
  status_code = "200"
  response_models = {"application/json": "Empty" }
}

resource aws_api_gateway_integration_response proxy {
  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  resource_id = aws_api_gateway_resource.root.id
  http_method = aws_api_gateway_method.proxy.http_method
  status_code = aws_api_gateway_method_response.proxy.status_code

  depends_on = [
    aws_api_gateway_method.proxy,
    aws_api_gateway_integration.lambda_integration
  ]
}

resource aws_api_gateway_deployment deployment {
  depends_on = [
    aws_api_gateway_integration.lambda_integration
  ]

  rest_api_id = aws_api_gateway_rest_api.hal_api.id
  stage_name = var.aws_stage
}

output rest_api_id {
  value = aws_api_gateway_rest_api.hal_api.id
}

output ressource_id {
  value = aws_api_gateway_resource.root.id
}
