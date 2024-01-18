output "name" {
  value = aws_lambda_function.lambda_function.function_name
}

output "arn" {
  value = aws_lambda_function.lambda_function.arn
}

output "invoke_arn" {
  value = aws_lambda_function.lambda_function.invoke_arn
}
