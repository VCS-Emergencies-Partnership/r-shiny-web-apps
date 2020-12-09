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

resource "azurerm_dns_cname_record" "web-apps" {
  for_each = { for item in var.web_apps: item.name => item }

  name = each.value.url
  resource_group_name = var.web_apps_dns_resource_group
  ttl = 300
  zone_name = var.web_app_domain
  record = "${var.prefix}-webapps-${each.value.img}.azurefd.net"
}

resource "azurerm_frontdoor" "web-apps" {
  for_each = { for item in var.web_apps: item.name => item }

  depends_on = [
    azurerm_dns_cname_record.web-apps
  ]

  name = "${var.prefix}-webapps-${each.value.img}"
  enforce_backend_pools_certificate_name_check = false
  resource_group_name = azurerm_resource_group.web-apps.name

  backend_pool {
    health_probe_name = format(local.name_format, "webapps-health${each.value.img}")
    load_balancing_name = format(local.name_format, "webapps-loadbalancer${each.value.img}")
    name = format(local.name_format, "webapps-backend${each.value.img}")
    backend {
      address = azurerm_app_service.web-apps[each.key].default_site_hostname
      host_header = azurerm_app_service.web-apps[each.key].default_site_hostname
      http_port = 80
      https_port = 443
    }
  }

  backend_pool_health_probe {
    name = format(local.name_format, "webapps-health${each.value.img}")
  }

  backend_pool_load_balancing {
    name = format(local.name_format, "webapps-loadbalancer${each.value.img}")
  }

  frontend_endpoint {
    custom_https_provisioning_enabled = false
    host_name = "${var.prefix}-webapps-${each.value.img}.azurefd.net"
    name = format(local.name_format, "webapps-${each.value.img}")
  }

  frontend_endpoint {
    custom_https_provisioning_enabled = true
    host_name = "${each.value.url}.${var.web_app_domain}"
    name = format(local.name_format, "webapps-custom-${each.value.img}")
    custom_https_configuration {
      certificate_source = "FrontDoor"
    }
  }

  routing_rule {
    accepted_protocols = ["Http", "Https"]
    frontend_endpoints = [
      format(local.name_format, "webapps-${each.value.img}"),
      format(local.name_format, "webapps-custom-${each.value.img}")
    ]
    name = format(local.name_format, "webapps-routing${each.value.img}")
    patterns_to_match = ["/*"]
    forwarding_configuration {
      backend_pool_name = format(local.name_format, "webapps-backend${each.value.img}")
    }
  }
}
