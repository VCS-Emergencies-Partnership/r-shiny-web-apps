#!/usr/bin/env bash

set -e

ACTION="${1}" # Can be either "lock" or "unlock"
ENV_NAME="${2}"
APP_NAME="${3}"

LOCK_NAME="SYSTEM_LOCK"

echo "Apply ${ACTION} command for app=${APP_NAME} and env=${ENV_NAME}"

groupList=$(az group list | jq \
  --arg APP_NAME "${APP_NAME}" \
  --arg ENV_NAME "${ENV_NAME}" \
  'map(select((.tags.app==$APP_NAME) and (.tags.env==$ENV_NAME)))')

for rg in $(echo "${groupList}" | jq -r '.[].name')
do
  case "${ACTION}" in
    lock )
      lockType=$(echo "${groupList}" | jq -r \
        --arg NAME "${rg}" \
        '.[] | select(.name==$NAME) | .tags.lock')

      if [ "${lockType}" == "null" ];
      then
        echo "Lock type not set - not locking: ${rg}"
        continue
      fi

      echo "Locking resource group ${rg} with type ${lockType}"

      az lock create \
        --lock-type "${lockType}" \
        --name="${LOCK_NAME}" \
        --resource-group "${rg}" > /dev/null
      ;;
    unlock )
      echo "Unlocking ${rg}"

      az lock delete \
        --name="${LOCK_NAME}" \
        --resource-group "${rg}" > /dev/null
      ;;

    * )
      echo "Unknown command: ${ACTION}"
      exit 1
      ;;
  esac

done

echo "All groups:"
az group list | jq -r '.[].name'
