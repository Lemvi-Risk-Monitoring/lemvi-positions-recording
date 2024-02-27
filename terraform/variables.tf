variable "aws_region" {
  type = string
}

variable "ib_flex_report_token" {
  type = string
}

variable "deribit_client_id" {
  type = string
}

variable "deribit_client_secret" {
  type = string
}

variable "ib_pgp_pass_key" {
  type = string
}

variable "ib_ftp_server" {
  type = string
  default = "ftp2.interactivebrokers.com"
}

variable "ib_ftp_username" {
  type = string
}

variable "ib_ftp_password" {
  type = string
}
