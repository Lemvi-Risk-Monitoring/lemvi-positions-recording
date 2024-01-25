locals {
  deployment_staging = ".deploy"
}

resource "null_resource" "prepare_bootstrap" {
  triggers = {
    script_hash = sha256("${local.deployment_staging}/${var.function_name}/bootstrap")
  }
  provisioner "local-exec" {
    command = "mkdir -p ${local.deployment_staging}/${var.function_name} && cp ${var.exe_path} ${local.deployment_staging}/${var.function_name}/bootstrap && strip ${local.deployment_staging}/${var.function_name}/bootstrap"
  }
}

data "archive_file" "lambda_package" {
  type        = "zip"
  output_path = "${local.deployment_staging}/bootstrap-${var.function_name}.zip"
  source_file = "${local.deployment_staging}/${var.function_name}/bootstrap"

  depends_on = [
    resource.null_resource.prepare_bootstrap
  ]
}

resource "aws_lambda_function" "lambda_function" {
  filename         = data.archive_file.lambda_package.output_path
  function_name    = var.function_name
  role             = aws_iam_role.lambda_role.arn
  handler          = "handler"
  runtime          = "provided.al2023"
  source_code_hash = data.archive_file.lambda_package.output_base64sha256
  timeout          = var.timeout
  environment {
    variables = var.environment_variables
  }

  depends_on = [
    data.archive_file.lambda_package
  ]
}

# IAM
data "aws_iam_policy_document" "assume_role_lambda" {
  statement {
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }

    actions = ["sts:AssumeRole"]
  }
}

resource "aws_iam_role" "lambda_role" {
  name               = "lambda-exec-${var.function_name}"
  assume_role_policy = data.aws_iam_policy_document.assume_role_lambda.json
}

data "aws_iam_policy" "lambda_logs_policy" {
  arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}

resource "aws_iam_role_policy_attachment" "lambda_logs_policy_attachment" {
  role       = aws_iam_role.lambda_role.name
  policy_arn = data.aws_iam_policy.lambda_logs_policy.arn
}
