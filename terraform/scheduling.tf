
#  Schedule for positions
resource "aws_cloudwatch_event_rule" "schedule_snapshot_positions" {
  name        = "${local.ws.aws_stage}-schedule-snapshot-positions"
  description = "triggering lambda every day at 1PM UTC"
  schedule_expression = "cron(0 13 * * ? *)"
}

#  Deribit
resource "aws_cloudwatch_event_target" "cron_deribit_positions" {
  arn   = module.lambda_function["deribit-lambda"].arn
  rule  = aws_cloudwatch_event_rule.schedule_snapshot_positions.name
  input = jsonencode({
    currencies = ["BTC", "ETH", "USDC", "USDT", "EURR"]
  })
}

resource "aws_lambda_permission" "allow_eventbridge_deribit" {
  statement_id  = "AllowExecutionFromEventBridge"
  action        = "lambda:InvokeFunction"
  function_name = module.lambda_function["deribit-lambda"].name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.schedule_snapshot_positions.arn
}

#  InteractiveBrokers
resource "aws_cloudwatch_event_target" "cron_ibrokers_positions" {
  arn   = module.lambda_function["ibrokers-request-lambda"].arn
  rule  = aws_cloudwatch_event_rule.schedule_snapshot_positions.name
  input = jsonencode({})
}

resource "aws_lambda_permission" "allow_eventbridge_ibrokers" {
  statement_id  = "AllowExecutionFromEventBridge"
  action        = "lambda:InvokeFunction"
  function_name = module.lambda_function["ibrokers-request-lambda"].name
  principal     = "events.amazonaws.com"
  source_arn    = aws_cloudwatch_event_rule.schedule_snapshot_positions.arn
}
