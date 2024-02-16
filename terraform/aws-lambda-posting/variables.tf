variable "lambda_function_arn" {
  type = string
}

variable "lambda_function_role_name" {
  type = string
}

variable "target_queue_arns" {
  type = map(string)
}

variable "target_queue_urls" {
  type = map(string)
}

variable "aws_stage" {
  type = string
}
