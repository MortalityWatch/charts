#!/usr/bin/env bash

# Require IP argument or environment variable
IP="${1:-${IP}}"

if [ -z "$IP" ]; then
  echo "❌ Error: IP is required."
  echo "Usage: IP=1.2.3.4 ./deploy.sh"
  echo "   or: ./deploy.sh 1.2.3.4"
  exit 1
fi

# Auto-detect repositories as all directories next to deploy.sh
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPOS=()

for dir in "$SCRIPT_DIR"/*/; do
  [ -d "$dir" ] && REPOS+=("$(basename "$dir")")
done

echo "Deploying to IP: $IP"
echo "Repositories: ${REPOS[*]}"

# Deploy each app, but only if there are changes.
for dir in "${REPOS[@]}"; do
  if cd "$dir" 2>/dev/null; then
    echo "--------------------------------------------------"
    echo "$dir"
    echo "--------------------------------------------------"

    sanitized_dir="${dir//./-}"
    export APP_NAME="$sanitized_dir"

    if ! git remote | grep -q "^dokku$"; then
      git remote add dokku "dokku@$IP:$sanitized_dir"
    else
      git remote set-url dokku "dokku@$IP:$sanitized_dir"
    fi

    git fetch dokku

    git checkout main 2>/dev/null || git checkout master
    CURRENT_BRANCH=$(git symbolic-ref --short HEAD)

    REMOTE_COMMIT=$(
      git rev-parse "dokku/$CURRENT_BRANCH" 2>/dev/null || echo "none"
    )
    LOCAL_COMMIT=$(git rev-parse HEAD)

    if [ "$REMOTE_COMMIT" = "none" ] ||
      [ "$REMOTE_COMMIT" != "$LOCAL_COMMIT" ]; then

      ssh co "dokku apps:create $APP_NAME || true"
      ssh co "dokku domains:add $APP_NAME $dir || true"

      if [ -f ./deploy_prepare.sh ]; then
        chmod +x ./deploy_prepare.sh
        echo "Running prepare script.."
        ./deploy_prepare.sh
      fi

      echo "Deploying.."
      git push dokku HEAD:refs/heads/"$CURRENT_BRANCH" -u -f

      if [ -f ./deploy_finalize.sh ]; then
        chmod +x ./deploy_finalize.sh
        echo "Running finalize script.."
        ./deploy_finalize.sh
      fi
    fi

    cd ..
  else
    echo "Directory $dir not found, skipping."
  fi
done
