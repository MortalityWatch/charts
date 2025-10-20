#!/usr/bin/env sh

# Most config now in deployments/config.json
# Secrets now in deployments/.env/cron.mortality.watch

# One-time server setup - set hostname
ssh co 'echo "ubuntu-s-1vcpu-512mb-10gb-nyc1-01" | sudo tee /etc/hostname > /dev/null'
ssh co 'sudo hostname -F /etc/hostname'

# Note: All secrets should be set in deployments/.env/cron.mortality.watch
# The deploy script will automatically load them as both runtime env vars and build args
