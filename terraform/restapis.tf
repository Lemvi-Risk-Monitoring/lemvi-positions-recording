
locals {
  api_description      = "HAL API Gateway"
  api_url_path         = "greet"
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
  function_name = aws_lambda_function.lambda_function.function_name
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
  uri                     = aws_lambda_function.lambda_function.invoke_arn
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
