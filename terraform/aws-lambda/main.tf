resource "null_resource" "prepare_bootstrap" {
  provisioner "local-exec" {
    command = "mkdir -p /tmp/${var.lambda_dir_name} && cp ${var.lambda_exe_path} /tmp/${var.lambda_dir_name}/bootstrap && strip /tmp/${var.lambda_dir_name}/bootstrap"
  }
}

data "archive_file" "lambda_package" {
  type        = "zip"
  output_path = "/tmp/bootstrap-${var.lambda_dir_name}.zip"
  source_file = "/tmp/${var.lambda_dir_name}/bootstrap"

  depends_on = [
    resource.null_resource.prepare_bootstrap
  ]
}

resource "aws_lambda_function" "lambda_function" {
  filename         = data.archive_file.lambda_package.output_path
  function_name    = var.aws_lambda_function_name
  role             = aws_iam_role.lambda_role.arn
  handler          = "handler"
  runtime          = "provided.al2023"
  source_code_hash = data.archive_file.lambda_package.output_base64sha256
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
  name               = "lambda-exec-${var.aws_lambda_function_name}"
  assume_role_policy = data.aws_iam_policy_document.assume_role_lambda.json
}

data "aws_iam_policy" "lambda_logs_policy" {
  arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}

resource "aws_iam_role_policy_attachment" "lambda_logs_policy_attachment" {
  role       = aws_iam_role.lambda_role.name
  policy_arn = data.aws_iam_policy.lambda_logs_policy.arn
}
