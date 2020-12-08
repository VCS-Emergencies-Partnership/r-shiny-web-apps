# Environments

Infrastructure which the applications are deployed to

## Requirements

No requirements.

## Providers

| Name | Version |
|------|---------|
| azurerm | n/a |

## Inputs

| Name | Description | Type | Default | Required |
|------|-------------|------|---------|:--------:|
| container\_registry\_name | Name of the container registry | `string` | n/a | yes |
| container\_registry\_rg\_name | Name of the registry's resource group | `string` | n/a | yes |
| location | Default location for resources | `string` | `"uksouth"` | no |
| prefix | Resource prefix | `string` | `"vcsep-wa"` | no |
| web\_apps | Web app configuration | <pre>list(object({<br>    always_on = bool<br>    img = string<br>    name = string<br>    tag = string<br>  }))</pre> | <pre>[<br>  {<br>    "always_on": true,<br>    "img": "hello-world",<br>    "name": "hello-world",<br>    "tag": "latest"<br>  }<br>]</pre> | no |
| web\_apps\_size | Web app plan size | `string` | `"S1"` | no |
| web\_apps\_tier | Web app plan tier | `string` | `"Standard"` | no |

## Outputs

| Name | Description |
|------|-------------|
| app-name | The application name - used for identifying resource groups |
| web-apps | List of web apps |

