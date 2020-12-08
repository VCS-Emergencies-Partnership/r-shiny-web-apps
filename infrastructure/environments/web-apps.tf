resource "azurerm_resource_group" "web-apps" {
  location = var.location
  name = format(local.name_format, "webapps")

  tags = {
    app = local.app_name
    env = terraform.workspace
    lock = local.lock_delete
  }
}

data "azurerm_container_registry" "global" {
  name = var.container_registry_name
  resource_group_name = var.container_registry_rg_name
}

resource "azurerm_app_service_plan" "web-apps" {
  name = format(local.name_format, "webapps")
  resource_group_name = azurerm_resource_group.web-apps.name
  location = azurerm_resource_group.web-apps.location
  kind = "Linux"
  reserved = true

  sku {
    size = var.web_apps_size
    tier = var.web_apps_tier
  }
}

resource "azurerm_app_service" "web-apps" {
  for_each = { for item in var.web_apps: item.name => item }

  # Diverges from convention as this becomes the URL
  name = "${var.prefix}-${each.value.name}${local.web_app_subdomain}"
  location = azurerm_resource_group.web-apps.location
  resource_group_name = azurerm_resource_group.web-apps.name
  app_service_plan_id = azurerm_app_service_plan.web-apps.id

  https_only = true

  app_settings = {
    WEBSITES_ENABLE_APP_SERVICE_STORAGE = false
    DOCKER_REGISTRY_SERVER_URL = data.azurerm_container_registry.global.login_server
    DOCKER_REGISTRY_SERVER_USERNAME = data.azurerm_container_registry.global.admin_username
    DOCKER_REGISTRY_SERVER_PASSWORD = data.azurerm_container_registry.global.admin_password
    DOCKER_ENABLE_CI = true
  }

  site_config {
    always_on = each.value.always_on
    linux_fx_version = "DOCKER|${data.azurerm_container_registry.global.login_server}/${each.value.img}:${each.value.tag}"
    ftps_state = "Disabled"
  }
}

resource "azurerm_container_registry_webhook" "web-apps" {
  for_each = { for item in var.web_apps: item.name => item }

  actions = [
    "push"
  ]
  name = replace("${each.value.img}push", "-", "")
  location = data.azurerm_container_registry.global.location
  registry_name = data.azurerm_container_registry.global.name
  resource_group_name = data.azurerm_container_registry.global.resource_group_name
  service_uri = "https://${azurerm_app_service.web-apps[each.value.name].site_credential.0.username}:${azurerm_app_service.web-apps[each.value.name].site_credential.0.password}@${azurerm_app_service.web-apps[each.value.name].name}.scm.azurewebsites.net/docker/hook"
  scope = "${each.value.img}:${each.value.tag}"
  status = "enabled"
}
