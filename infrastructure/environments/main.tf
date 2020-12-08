/**
 * # Environments
 *
 * Infrastructure which the applications are deployed to
 */

terraform {
  backend "azurerm" {
    resource_group_name = "terraform"
    storage_account_name = "vcsepterraform"
    container_name = "tfstate"
    key = "webapps.tfstate"
  }
}

provider "azurerm" {
  features {}
}
