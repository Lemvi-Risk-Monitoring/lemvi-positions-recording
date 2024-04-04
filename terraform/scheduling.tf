
#  Schedule for Deribit positions
resource "aws_cloudwatch_event_rule" "schedule_nightly" {
  name        = "${local.ws.aws_stage}-schedule-nightly"
  description = "triggering lambda every day at 4AM UTC"
  schedule_expression = "cron(0 4 * * ? *)"
}

#  Schedule for IB positions (FTP)
resource "aws_cloudwatch_event_rule" "schedule_hourly" {
  name        = "${local.ws.aws_stage}-schedule-hourly"
  description = "triggering lambda every hour"
  schedule_expression = "cron(12 * * * ? *)"
}

#  Schedule for IB positions (Flex Query)
resource "aws_cloudwatch_event_rule" "schedule_multiple_daily" {
  name        = "${local.ws.aws_stage}-schedule-multiple-daily"
  description = "triggering lambda multiple times in the day"
  schedule_expression = "cron(5 6,10,14,18 * * ? *)"
}

#  Deribit
resource "aws_cloudwatch_event_target" "cron_deribit_positions" {
  arn   = module.lambda_function["deribit-lambda"].arn
  rule  = aws_cloudwatch_event_rule.schedule_nightly.name
  input = jsonencode({
    currencies = ["BTC", "ETH", "USDC", "USDT", "EURR"]
  })
}

resource "aws_lambda_permission" "allow_eventbridge_deribit" {
  statement_id  = "AllowExecutionFromEventBridge"
  action        = "lambda:InvokeFunction"
  function_name = module.lambda_function["deribit-lambda"].name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.schedule_nightly.arn
}

#  InteractiveBrokers FTP
resource "aws_cloudwatch_event_target" "cron_ibrokers_positions_ftp" {
  arn   = module.lambda_function["ibrokers-ftp-lambda"].arn
  rule  = aws_cloudwatch_event_rule.schedule_hourly.name
  input = jsonencode({})
}

resource "aws_lambda_permission" "allow_eventbridge_ibrokers_ftp" {
  statement_id  = "AllowExecutionFromEventBridge"
  action        = "lambda:InvokeFunction"
  function_name = module.lambda_function["ibrokers-ftp-lambda"].name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.schedule_hourly.arn
}

#  InteractiveBrokers FlexQuery Request
resource "aws_cloudwatch_event_target" "cron_ibrokers_positions_flex" {
  arn   = module.lambda_function["ibrokers-request-lambda"].arn
  rule  = aws_cloudwatch_event_rule.schedule_multiple_daily.name
  input = jsonencode({ "flexQueryId": "906041" })
}

resource "aws_lambda_permission" "allow_eventbridge_ibrokers_flex" {
  statement_id  = "AllowExecutionFromEventBridge"
  action        = "lambda:InvokeFunction"
  function_name = module.lambda_function["ibrokers-request-lambda"].name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.schedule_multiple_daily.arn
}
