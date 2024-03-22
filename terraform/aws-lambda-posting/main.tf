data "aws_iam_policy_document" "send_message_lambda_policy" {
  statement {
    effect = "Allow"
    actions = ["sqs:SendMessage"]
    resources = [ for name, queue in var.target_queues : queue.queue_arn ]
  }
}

resource "aws_iam_policy" "send_message_lambda" {
  name        = "${var.aws_stage}-send-message-lambda"
  description = "IAM policy allowing Lambda to send messages to SQS"
  policy = data.aws_iam_policy_document.send_message_lambda_policy.json
}

resource "aws_iam_role_policy_attachment" "lambda_sqs_policy_attachment" {
  for_each = var.target_queues
  role       = var.target_queues[each.key].function_role_name
  policy_arn = aws_iam_policy.send_message_lambda.arn
}

resource "aws_sqs_queue_policy" "queue_policy" {
  for_each = var.target_queues
  queue_url = var.target_queues[each.key].queue_url
  policy    = jsonencode({
    Version    = "2012-10-17",
    Id         = "AllowLambdaToPostMessages",
    Statement  = [{
      Sid       = "AllowLambdaToPostMessages",
      Effect    = "Allow",
      Principal = "*",
      Action    = "sqs:SendMessage",
      Resource  = var.target_queues[each.key].queue_arn,
      Condition = {
        ArnEquals: {
          "aws:SourceArn": var.target_queues[each.key].function_arn
        }
      }
    }]
  })
}
