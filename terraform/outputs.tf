
output rest_api_id {
  value = aws_api_gateway_rest_api.rest_api.id
}

output ressource_id {
  value = aws_api_gateway_resource.root.id
}
