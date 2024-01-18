provider "aws" {
  region = var.aws_region
}

locals {
  project_name_version = "lemvi-positions-recording-0.1.0.0"
  lambda_dir_name      = "aws-app"
  dist_path            = "dist-newstyle/build/x86_64-linux/ghc-9.4.8"
  lambda_exe_path      = "${path.cwd}/${local.dist_path}/${local.project_name_version}/x/${local.lambda_dir_name}/build/${local.lambda_dir_name}/${local.lambda_dir_name}"
}

module "lambda_function" {
  source = "./aws-lambda"

  lambda_dir_name = local.lambda_dir_name
  lambda_exe_path = local.lambda_exe_path
  aws_lambda_function_name = var.aws_lambda_function_name
}
