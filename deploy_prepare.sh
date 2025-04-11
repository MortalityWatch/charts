#!/usr/bin/env sh

# Create app if it doesn't exist
ssh co "dokku apps:exists $APP_NAME || dokku app:create $APP_NAME"

# Set hostname
ssh co 'echo "ubuntu-s-1vcpu-512mb-10gb-nyc1-01" | sudo tee /etc/hostname > /dev/null'
ssh co 'sudo hostname -F /etc/hostname'

# Prompt user for sensitive values
read -sp "Enter S3_SECRET: " S3_SECRET
echo
read -sp "Enter CRONICLE mail auth user: " CRONICLE_MAIL_USER
echo
read -sp "Enter CRONICLE mail auth password: " CRONICLE_MAIL_PASS
echo
read -sp "Enter AWS Secret Access Key: " AWS_SECRET_ACCESS_KEY
echo
read -sp "Enter ZIP Password: " ZIP_PASSWORD
echo

ssh co "dokku docker-options:add ${APP_NAME} build --build-arg S3_SECRET=$S3_SECRET"
ssh co "dokku ports:set ${APP_NAME} http:80:3012"

# Install plugin
ssh co 'dokku plugin:install https://github.com/michaelshobbs/dokku-hostname.git dokku-hostname'

# Set configuration using SSH
ssh co "dokku config:set $APP_NAME --no-restart \
  AWS_ACCESS_KEY_ID=minio \
  AWS_DEFAULT_REGION=s3 \
  AWS_S3_ENDPOINT=mortality.watch \
  AWS_SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY \
  CI=1 \
  CRONICLE_email_from=cron@mortality.watch \
  CRONICLE_job_memory_max=0 \
  CRONICLE_mail_options__auth__user=$CRONICLE_MAIL_USER \
  CRONICLE_mail_options__auth__pass=$CRONICLE_MAIL_PASS \
  CRONICLE_mail_options__host=smtp-relay.sendinblue.com \
  CRONICLE_smtp_hostname=smtp-relay.sendinblue.com \
  CRONICLE_smtp_port=587 \
  CRONICLE_smtp_secure=false \
  DOKKU_DOCKERFILE_PORTS=3012 \
  DOKKU_PROXY_PORT_MAP='http:80:3012 http:3012:3012' \
  HOSTNAME=ubuntu-s-1vcpu-512mb-10gb-nyc1-01 \
  ZIP_PASSWORD=$ZIP_PASSWORD"
