provider "aws" {
  region = var.aws_region
}

locals {
  project_name_version       = "lemvi-positions-recording-0.1.0.0"
  ghc_dist_path              = "dist-newstyle/build/x86_64-linux/ghc-9.4.8"
  echo_lambda_dir_name       = "echo-app"
  ibrokers_app_dir_name      = "ibrokers-app"
  dist_path                  = "${path.cwd}/${local.ghc_dist_path}/${local.project_name_version}/x/"
  exe_path_echo_lambda       = "${local.dist_path}/${local.echo_lambda_dir_name}/build/${local.echo_lambda_dir_name}/${local.echo_lambda_dir_name}"
  exe_path_ibrokers_app      = "${local.dist_path}/${local.ibrokers_app_dir_name}/build/${local.ibrokers_app_dir_name}/${local.ibrokers_app_dir_name}"

  lambda_functions = {
      "${var.aws_stage}-echo-lambda"       = local.exe_path_echo_lambda,
      "${var.aws_stage}-ibrokers-lambda"   = local.exe_path_ibrokers_app,
  }
  
  ibrokers_bucket_name = "${var.aws_stage}-ibrokers-positions"
  deribit_bucket_name = "${var.aws_stage}-deribit-positions"
}

module "lambda_function" {
  for_each = local.lambda_functions
  source = "./aws-lambda"

  function_name = each.key
  exe_path = each.value
  environment_variables = { "IB_FLEX_REPORT_TOKEN": var.ib_flex_report_token }
}

module "gateway_proxy_integration" {
  source = "./gateway-proxy-integration"

  api_description      = "HAL API Gateway"
  api_url_path         = "greet"
  function_name        = "${var.aws_stage}-echo-lambda"
  function_invoke_arn  = module.lambda_function["${var.aws_stage}-echo-lambda"].invoke_arn
  aws_stage            = var.aws_stage
}

resource "aws_s3_bucket" "ibrokers_bucket" {
  bucket = local.ibrokers_bucket_name
}

resource "aws_s3_bucket" "deribit_bucket" {
  bucket = local.deribit_bucket_name
}
