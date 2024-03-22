variable "target_queues" {
  type = map(object({
    queue_url = string
    queue_arn = string
    function_role_name = string
    function_arn = string
  }))
}

variable "aws_stage" {
  type = string
}
