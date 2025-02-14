# Mortality.Watch Charts
Collection of R scripts, that download, combine, and display several data sources.
It's main purpose is the generation of mortality data for mortality.watch.

# S3
Data and Charts are stored on MinIO/S3 at s3.mortality.watch.

# Usage
Sample usage:
```
./install_r_deps.sh
Rscript update_datasets.r
Rscript population/world.r
```

See https://www.mortality.watch/ for the frontend.

# Configuration

## MinIO/S3
Set build args:
```
dokku docker-options:add cronicle build '--build-arg S3_SECRET=...'
```

## Cronicle/Job Scheduler
Runs on a cronicle job scheduler, daily.

Dokku config:
```
dokku config:set cronicle AWS_ACCESS_KEY_ID=minio
dokku config:set cronicle AWS_DEFAULT_REGION=s3
dokku config:set cronicle AWS_S3_ENDPOINT=mortality.watch
dokku config:set cronicle AWS_SECRET_ACCESS_KEY=your_secret_access_key_here
dokku config:set cronicle CRONICLE_job_memory_max=0
dokku config:set cronicle CRONICLE_mail_options__auth__pass=your_email_password_here
dokku config:set cronicle CRONICLE_mail_options__auth__user=benmarten1986@gmail.com
dokku config:set cronicle ZIP_PASSWORD=your_zip_password_here
```

Each cronicle job is setup like this:
```sh
#!/usr/bin/env bash

[ ! -d \"charts\" ] && git clone https://github.com/USMortality/charts.git
cd charts
git fetch
git reset origin/master --hard
Rscript script.r
```

Backup is in cronicle.txt.

# Data Format
This is the data format used in the world_dataset.r script.

Example:
   iso3c date       age_group deaths population  type n_age_groups source
   <chr> <date>     <chr>      <int>      <dbl> <dbl>        <dbl> <chr> 
 1 USA   1999-01-01 0-9         3094   39742004     2            9 cdc   
 2 USA   1999-01-01 10-19       1413   40297966     2            9 cdc   
 3 USA   1999-01-01 20-29       2813   38166486     2            9 cdc   
