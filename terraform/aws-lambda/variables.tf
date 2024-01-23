variable "exe_path" {
  type = string
}

variable "function_name" {
  type = string
}

variable "environment_variables" {
  type = map(string)
}

variable "timeout" {
  type = number
}
