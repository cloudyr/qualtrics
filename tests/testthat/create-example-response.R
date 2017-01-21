# Normal
response <- request("GET", "surveys")
saveRDS(response, "response.Rds")

# "Request proxied" notice
response_with_notice <- suppressWarnings(request("GET", "surveys", subdomain =
    "az1"))
saveRDS(response_with_notice, "response_with_notice.Rds")
