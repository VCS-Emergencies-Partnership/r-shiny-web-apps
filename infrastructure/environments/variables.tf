/*
  Common
 */

variable "location" {
  description = "Default location for resources"
  default = "uksouth"
  type = string
}

variable "prefix" {
  description = "Resource prefix"
  default = "vcsep-wa"
  type = string
}

/*
  Container Registry
 */

variable "container_registry_name" {
  description = "Name of the container registry"
  type = string
}

variable "container_registry_rg_name" {
  description = "Name of the registry's resource group"
  type = string
}


/*
  Web Apps
 */

variable "web_apps" {
  description = "Web app configuration"
  type = list(object({
    always_on = bool
    img = string
    name = string
    tag = string
  }))
  default = []
}

variable "web_apps_size" {
  description = "Web app plan size"
  default = "S1"
}

variable "web_apps_tier" {
  description = "Web app plan tier"
  default = "Standard"
}
