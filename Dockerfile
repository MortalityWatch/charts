# Stage 1: Build Cronicle
FROM cronicle/base-alpine AS cronicle-build
RUN apk add --no-cache git npm python3 alpine-sdk && \ 
git clone --depth=1 https://github.com/cronicle-edge/cronicle-edge \ 
/build/cronicle-edge

WORKDIR /build/cronicle-edge

# Ensure the bundle script is executable and build Cronicle
RUN chmod +x bundle && ./bundle /dist --s3 --tools

# Stage 2: Final Image Based on R2U for R Dependencies
FROM eddelbuettel/r2u:24.04

WORKDIR /opt/cronicle
RUN chown -R ubuntu:ubuntu /opt/cronicle

# Copy and install system dependencies
COPY dependencies.txt .
RUN echo "Updating deps... 2/14/2025" && \ 
apt-get update && \ 
apt-get install -y $(cat dependencies.txt) && \ 
rm -rf /var/lib/apt/lists/*

# Copy Cronicle from the build stage
COPY --from=cronicle-build /dist .

# Create a non-root user for Cronicle
RUN chown -R docker:docker /opt/cronicle

# Define build-time arguments
ARG S3_SECRET

# Set environment variables for Cronicle
ENV PATH="/opt/cronicle/bin:/opt/cronicle/minio-binaries:${PATH}" \
  CRONICLE_foreground=1 \
  CRONICLE_echo=1 \
  TZ=America/New_York \
  LANG="en_US.UTF-8" \
  LANGUAGE="en_US.UTF-8" \
  LC_ALL="en_US.UTF-8" \
  CRONICLE_Storage__engine=S3 \
  CRONICLE_Storage__S3__params__Bucket=cronicle \
  CRONICLE_Storage__AWS__endpoint=http://s3-gate.mortality.watch \
  CRONICLE_Storage__AWS__forcePathStyle=true \
  CRONICLE_Storage__AWS__region=us-east-1 \
  CRONICLE_Storage__AWS__credentials__secretAccessKey="${S3_SECRET}" \
  CRONICLE_Storage__AWS__credentials__accessKeyId=minio

# Protect sensitive folders
RUN mkdir -p /opt/cronicle/data /opt/cronicle/conf && chmod 0700 \ 
/opt/cronicle/data /opt/cronicle/conf

# SSL configuration
COPY openssl.cnf .
ENV OPENSSL_CONF=/opt/cronicle/openssl.cnf

# Install MinIO Client
RUN mkdir -p minio-binaries && \ 
curl -fsSL https://dl.min.io/client/mc/release/linux-amd64/mc \ 
-o minio-binaries/mc && chmod +x minio-binaries/mc && \ 
minio-binaries/mc alias set minio http://s3-gate.mortality.watch \ 
minio $S3_SECRET

# Install R dependencies
RUN apt-get update && apt-get install -y r-base r-base-dev
COPY dependencies_r.txt install_r_deps.sh .
RUN chmod +x install_r_deps.sh && ./install_r_deps.sh

ENV NODE_VERSION="22.x"
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VERSION} | bash -
RUN apt-get install -y nodejs

# Expose Cronicle Manager Port
EXPOSE 3012

# Set default command
ENTRYPOINT ["/usr/bin/tini", "-s", "--"]
CMD ["manager"]
