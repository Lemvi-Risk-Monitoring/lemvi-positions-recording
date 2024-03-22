
#  Schedule for Deribit positions
resource "aws_cloudwatch_event_rule" "schedule_nightly" {
  name        = "${local.ws.aws_stage}-schedule-nightly"
  description = "triggering lambda every day at 4AM UTC"
  schedule_expression = "cron(0 4 * * ? *)"
}

#  Schedule for IB positions
resource "aws_cloudwatch_event_rule" "schedule_hourly" {
  name        = "${local.ws.aws_stage}-schedule-hourly"
  description = "triggering lambda every hour"
  schedule_expression = "cron(12 * * * ? *)"
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

#  InteractiveBrokers
resource "aws_cloudwatch_event_target" "cron_ibrokers_positions" {
  arn   = module.lambda_function["ibrokers-ftp-lambda"].arn
  rule  = aws_cloudwatch_event_rule.schedule_hourly.name
  input = jsonencode({})
}

resource "aws_lambda_permission" "allow_eventbridge_ibrokers" {
  statement_id  = "AllowExecutionFromEventBridge"
  action        = "lambda:InvokeFunction"
  function_name = module.lambda_function["ibrokers-ftp-lambda"].name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.schedule_hourly.arn
}
