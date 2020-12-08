/*
  Misc
 */

output "app-name" {
  description = "The application name - used for identifying resource groups"
  value = local.app_name
}

/*
  Web Apps
 */

output "web-apps" {
  description = "List of web apps"
  value = try([ for item in var.web_apps :
    {
      name = item.name
      url = "http://${azurerm_app_service.web-apps[item.name].default_site_hostname}"
    }
  ], [])
}
