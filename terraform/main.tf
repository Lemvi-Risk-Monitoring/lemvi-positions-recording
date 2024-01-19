provider "aws" {
  region = var.aws_region
}

locals {
  project_name_version       = "lemvi-positions-recording-0.1.0.0"
  ghc_dist_path              = "dist-newstyle/build/x86_64-linux/ghc-9.4.8"
  hello_rest_lambda_dir_name = "aws-app"
  echo_lambda_dir_name       = "echo-app"
  dist_path                  = "${path.cwd}/${local.ghc_dist_path}/${local.project_name_version}/x/"
  exe_path_hello_rest_lambda = "${local.dist_path}/${local.hello_rest_lambda_dir_name}/build/${local.hello_rest_lambda_dir_name}/${local.hello_rest_lambda_dir_name}"
  exe_path_echo_lambda       = "${local.dist_path}/${local.echo_lambda_dir_name}/build/${local.echo_lambda_dir_name}/${local.echo_lambda_dir_name}"

  lambda_functions = {
      "${var.aws_stage}-hello-rest-lambda" = local.exe_path_hello_rest_lambda,
      "${var.aws_stage}-echo-lambda"       = local.exe_path_echo_lambda
  }
  
  ibrokers_bucket_name = "${var.aws_stage}-ibrokers-positions"
}

module "lambda_function" {
  for_each = local.lambda_functions
  source = "./aws-lambda"

  function_name = each.key
  exe_path = each.value
}

module "gateway_proxy_integration" {
  source = "./gateway-proxy-integration"

  api_description      = "HAL API Gateway"
  api_url_path         = "greet"
  function_name        = "${var.aws_stage}-hello-rest-lambda"
  function_invoke_arn  = module.lambda_function["${var.aws_stage}-hello-rest-lambda"].invoke_arn
  aws_stage            = var.aws_stage
}

resource "aws_s3_bucket" "ibrokers_bucket" {
  bucket = local.ibrokers_bucket_name
}
