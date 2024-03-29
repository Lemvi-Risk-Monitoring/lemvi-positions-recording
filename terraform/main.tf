provider "aws" {
  region = var.aws_region
}

data "aws_caller_identity" "current" {}

locals {

  workspaces = {
    default = {
      aws_stage = "test"
    }
    prod = {
      aws_stage = "prod"
    }
  }
  ws = local.workspaces[terraform.workspace]

  queue_ibrokers_report         = "ibrokers-reporting"
  queue_decryption              = "decryption"
  project_name_version          = "lemvi-positions-recording-0.1.0.0"
  ghc_dist_path                 = "dist-newstyle/build/x86_64-linux/ghc-9.4.8"
  echo_lambda_dir_name          = "echo-app"
  ibrokers_request_app_dir_name = "ibrokers-request-app"
  ibrokers_fetch_app_dir_name   = "ibrokers-fetch-app"
  decryption_app_dir_name       = "decryption-app"
  ibrokers_ftp_app_dir_name     = "ibrokers-ftp-app"
  deribit_app_dir_name          = "deribit-app"
  dist_path                     = "${path.cwd}/${local.ghc_dist_path}/${local.project_name_version}/x"
  exe_path_echo_lambda          = "${local.dist_path}/${local.echo_lambda_dir_name}/build/${local.echo_lambda_dir_name}/${local.echo_lambda_dir_name}"
  exe_path_ibrokers_request_app = "${local.dist_path}/${local.ibrokers_request_app_dir_name}/build/${local.ibrokers_request_app_dir_name}/${local.ibrokers_request_app_dir_name}"
  exe_path_ibrokers_fetch_app   = "${local.dist_path}/${local.ibrokers_fetch_app_dir_name}/build/${local.ibrokers_fetch_app_dir_name}/${local.ibrokers_fetch_app_dir_name}"
  exe_path_decryption_app       = "${local.dist_path}/${local.decryption_app_dir_name}/build/${local.decryption_app_dir_name}/${local.decryption_app_dir_name}"
  exe_path_ibrokers_ftp_app     = "${local.dist_path}/${local.ibrokers_ftp_app_dir_name}/build/${local.ibrokers_ftp_app_dir_name}/${local.ibrokers_ftp_app_dir_name}"
  exe_path_deribit_app          = "${local.dist_path}/${local.deribit_app_dir_name}/build/${local.deribit_app_dir_name}/${local.deribit_app_dir_name}"

  lambda_functions = {
      "echo-lambda"       = {
        exe_path = local.exe_path_echo_lambda
        timeout = 60
        description = <<-EOT
          Testing lambda function
          EOT
        environment_variables = {
          }
        }
      "ibrokers-request-lambda"   = { 
        exe_path = local.exe_path_ibrokers_request_app
        timeout = 60
        description = <<-EOT
          Requesting IBrokers report
          Example test: { "flexQueryId": "906041" }
          EOT
        environment_variables = {
          "IB_FLEX_REPORT_TOKEN": var.ib_flex_report_token,
          "IBROKERS_QUEUE_REPORT_URL": aws_sqs_queue.queue_ibrokers_report.url
          }
      },
      "ibrokers-fetch-lambda"   = { 
        exe_path = local.exe_path_ibrokers_fetch_app
        timeout = 60
        description = <<-EOT
          Retrieving IBrokers report
          EOT
        environment_variables = {
          "IB_FLEX_REPORT_TOKEN": var.ib_flex_report_token,
          "IBROKERS_BUCKET_POSITIONS": aws_s3_bucket.ibrokers_bucket.bucket
          }
      },
      "decryption-lambda"   = { 
        exe_path = local.exe_path_decryption_app
        timeout = 60
        description = <<-EOT
          GPG Decryption
          EOT
        environment_variables = {
        }
      },
      "ibrokers-ftp-lambda"   = { 
        exe_path = local.exe_path_ibrokers_ftp_app
        timeout = 60
        description = <<-EOT
          Retrieving IBrokers report using FTP
          EOT
        environment_variables = {
          "IB_PGP_PRIVATE_KEY_PATH": "${aws_s3_bucket.lambda_resources_bucket.bucket}/private-ibrokers-reporting.pgp",
          "IB_PGP_PASS_KEY": var.ib_pgp_pass_key,
          "IB_FTP_SERVER": var.ib_ftp_server,
          "IB_FTP_USERNAME": var.ib_ftp_username,
          "IB_FTP_PASSWORD": var.ib_ftp_password,
          "IBROKERS_BUCKET_POSITIONS": aws_s3_bucket.ibrokers_bucket.bucket,
          "DECRYPTION_QUEUE_URL": aws_sqs_queue.queue_decryption.url
          }
      },
      "deribit-lambda"    =  { 
        exe_path = local.exe_path_deribit_app
        timeout = 60
        description = <<-EOT
          Requesting Deribit positions
          EOT
        environment_variables = {
          "DERIBIT_CLIENT_ID": var.deribit_client_id,
          "DERIBIT_CLIENT_SECRET": var.deribit_client_secret,
          "DERIBIT_BUCKET_POSITIONS": aws_s3_bucket.deribit_bucket.bucket
          }
      }
  }
  
  ibrokers_bucket_name = "${local.ws.aws_stage}-ibrokers-positions-${data.aws_caller_identity.current.account_id}"
  deribit_bucket_name = "${local.ws.aws_stage}-deribit-positions-${data.aws_caller_identity.current.account_id}"
  lambda_resources_bucket_name = "${local.ws.aws_stage}-lambda-resources-${data.aws_caller_identity.current.account_id}"
}

module "lambda_function" {
  for_each = local.lambda_functions
  source = "./aws-lambda"

  function_name = "${local.ws.aws_stage}-${each.key}"
  exe_path = each.value.exe_path
  timeout = each.value.timeout
  description = each.value.description
  environment_variables = each.value.environment_variables
}

module "gateway_proxy_integration" {
  source = "./gateway-proxy-integration"

  api_description      = "HAL API Gateway"
  api_url_path         = "greet"
  function_name        = module.lambda_function["echo-lambda"].name
  function_invoke_arn  = module.lambda_function["echo-lambda"].invoke_arn
  aws_stage            = local.ws.aws_stage
}

resource "aws_s3_bucket" "ibrokers_bucket" {
  bucket = local.ibrokers_bucket_name
}

resource "aws_s3_bucket" "deribit_bucket" {
  bucket = local.deribit_bucket_name
}

resource "aws_s3_bucket" "lambda_resources_bucket" {
  bucket = local.lambda_resources_bucket_name
}

resource "aws_sqs_queue" "queue_ibrokers_report" {
      name = "${local.ws.aws_stage}-${local.queue_ibrokers_report}"
      visibility_timeout_seconds = 60
      delay_seconds              = 30
      message_retention_seconds  = 3600
}

resource "aws_lambda_event_source_mapping" "queue_trigger_ibrokers_report" {
  batch_size        = 1
  scaling_config {
    maximum_concurrency = 2
  }
  event_source_arn  = aws_sqs_queue.queue_ibrokers_report.arn
  function_name     = module.lambda_function["ibrokers-fetch-lambda"].arn
  enabled           = true
}

resource "aws_sqs_queue" "queue_decryption" {
      name = "${local.ws.aws_stage}-${local.queue_decryption}"
      visibility_timeout_seconds = 60
      delay_seconds              = 30
      message_retention_seconds  = 3600
}

resource "aws_lambda_event_source_mapping" "queue_trigger_decryption" {
  batch_size        = 1
  scaling_config {
    maximum_concurrency = 2
  }
  event_source_arn  = aws_sqs_queue.queue_decryption.arn
  function_name     = module.lambda_function["decryption-lambda"].arn
  enabled           = true
}

module "lambda_posting" {
  source = "./aws-lambda-posting"
  aws_stage = local.ws.aws_stage
  target_queues = {
    (local.queue_ibrokers_report): {
      queue_url = aws_sqs_queue.queue_ibrokers_report.url,
      queue_arn = aws_sqs_queue.queue_ibrokers_report.arn,
      function_role_name = module.lambda_function["ibrokers-request-lambda"].lambda_role.name,
      function_arn = module.lambda_function["ibrokers-request-lambda"].arn
    },
    (local.queue_decryption): {
      queue_url = aws_sqs_queue.queue_decryption.url,
      queue_arn = aws_sqs_queue.queue_decryption.arn,
      function_role_name = module.lambda_function["ibrokers-ftp-lambda"].lambda_role.name,
      function_arn = module.lambda_function["ibrokers-ftp-lambda"].arn
    }
  }
}
