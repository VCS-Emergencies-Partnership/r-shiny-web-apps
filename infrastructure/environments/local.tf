locals {
  app_name = "webapps"
  location = substr(var.location, 0, 3) # short version of the location
  lock_delete = "CanNotDelete"
  lock_none = null # Doesn't add tag
  lock_readonly = "ReadOnly"
  name_format = join("-", [
    var.prefix,
    local.location,
    "%s", # name
    local.workspace_name
  ])
  web_app_subdomain = terraform.workspace == "prod" ? "app" : "${terraform.workspace}-app"
  web_app_count = length(var.web_apps)
  workspace_name = replace(terraform.workspace, "/[\\W\\-]/", "") # alphanumeric workspace name
}
