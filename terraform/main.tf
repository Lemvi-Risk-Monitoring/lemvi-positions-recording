provider "aws" {
  region = var.aws_region
}

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

  queue_ibrokers_report      = "ibrokers-reporting"
  project_name_version       = "lemvi-positions-recording-0.1.0.0"
  ghc_dist_path              = "dist-newstyle/build/x86_64-linux/ghc-9.4.8"
  echo_lambda_dir_name       = "echo-app"
  ibrokers_request_app_dir_name      = "ibrokers-request-app"
  ibrokers_fetch_app_dir_name      = "ibrokers-fetch-app"
  deribit_app_dir_name       = "deribit-app"
  dist_path                  = "${path.cwd}/${local.ghc_dist_path}/${local.project_name_version}/x/"
  exe_path_echo_lambda       = "${local.dist_path}/${local.echo_lambda_dir_name}/build/${local.echo_lambda_dir_name}/${local.echo_lambda_dir_name}"
  exe_path_ibrokers_request_app      = "${local.dist_path}/${local.ibrokers_request_app_dir_name}/build/${local.ibrokers_request_app_dir_name}/${local.ibrokers_request_app_dir_name}"
  exe_path_ibrokers_fetch_app      = "${local.dist_path}/${local.ibrokers_fetch_app_dir_name}/build/${local.ibrokers_fetch_app_dir_name}/${local.ibrokers_fetch_app_dir_name}"
  exe_path_deribit_app       = "${local.dist_path}/${local.deribit_app_dir_name}/build/${local.deribit_app_dir_name}/${local.deribit_app_dir_name}"

  lambda_functions = {
      "echo-lambda"       = {
        exe_path = local.exe_path_echo_lambda
        description = <<-EOT
          Testing lambda function.
          EOT
        environment_variables = {
          }
        }
      "ibrokers-request-lambda"   = { 
        exe_path = local.exe_path_ibrokers_request_app
        description = <<-EOT
          Requesting IBrokers report.
          Example test: { "flexQueryId": "906041" }
          EOT
        environment_variables = {
          "IB_FLEX_REPORT_TOKEN": var.ib_flex_report_token,
          "IBROKERS_QUEUE_REPORT_URL": aws_sqs_queue.queue_ibrokers_report.url
          }
      },
      "ibrokers-fetch-lambda"   = { 
        exe_path = local.exe_path_ibrokers_fetch_app
        description = <<-EOT
          Retrieving IBrokers report.
          EOT
        environment_variables = {
          "IB_FLEX_REPORT_TOKEN": var.ib_flex_report_token,
          "IBROKERS_BUCKET_POSITIONS": aws_s3_bucket.ibrokers_bucket.bucket
          }
      },
      "deribit-lambda"    =  { 
        exe_path = local.exe_path_deribit_app
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
  
  ibrokers_bucket_name = "${local.ws.aws_stage}-ibrokers-positions"
  deribit_bucket_name = "${local.ws.aws_stage}-deribit-positions"
}

module "lambda_function" {
  for_each = local.lambda_functions
  source = "./aws-lambda"

  function_name = "${local.ws.aws_stage}-${each.key}"
  exe_path = each.value.exe_path
  timeout = 60
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

resource "aws_cloudwatch_event_rule" "schedule_snapshot_positions" {
  name        = "${local.ws.aws_stage}-schedule-snapshot-positions"
  description = "triggering lambda every day at 8AM"
  schedule_expression = "cron(0 8 * * ? *)"
}

resource "aws_cloudwatch_event_target" "cron_deritbit_positions" {
  arn   = module.lambda_function["deribit-lambda"].arn
  rule  = aws_cloudwatch_event_rule.schedule_snapshot_positions.name
  input = jsonencode({
    currencies = ["BTC", "ETH", "USDC", "USDT", "EURR"]
  })
}

resource "aws_lambda_permission" "allow_eventbridge" {
  statement_id  = "AllowExecutionFromEventBridge"
  action        = "lambda:InvokeFunction"
  function_name = module.lambda_function["deribit-lambda"].name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.schedule_snapshot_positions.arn
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

module "lambda_posting" {
  source = "./aws-lambda-posting"
  lambda_function_role_name = module.lambda_function["ibrokers-request-lambda"].lambda_role.name
  lambda_function_arn = module.lambda_function["ibrokers-request-lambda"].arn
  aws_stage = local.ws.aws_stage
  target_queue_urls = {
    (local.queue_ibrokers_report): aws_sqs_queue.queue_ibrokers_report.url
  }
  target_queue_arns = {
    (local.queue_ibrokers_report): aws_sqs_queue.queue_ibrokers_report.arn
  }
}
