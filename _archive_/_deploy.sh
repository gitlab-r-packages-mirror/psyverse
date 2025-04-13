### In Plesk, execute with:
### bash _deploy.sh >> deployment.log 2>&1
### Cran job for executing every hour with logging
### @hourly bash ~/git_clone_hpss/deploy.sh >> ~/git_clone_hpss/deployment.log 2>&1
### https://www.cyberciti.biz/faq/how-do-i-add-jobs-to-cron-under-linux-or-unix-oses/
### To edit the cron file with nano instead of vim:
### export VISUAL=nano; crontab -e

echo ----------
echo $(date)

### Go to directory with cloned git repo
cd ~/deploy_psyverse.opens.science

### Delete old 'public' directory if it exists
#rm -rf public

pwd
echo $PATH
echo Calling PkgDown

### Render the site
#/usr/local/bin/quarto render --to all
/usr/local/bin/R -e "pkgdown::build_site();"

echo Finished PkgDown

### Copy image
mkdir public/img
cp img/hex-logo.png public/img/hex-logo.png

### Delete all contents in public HTML directory
rm -rf ~/psyverse.opens.science/*.*
rm -rf ~/psyverse.opens.science/*
rm -f ~/psyverse.opens.science/.htaccess

### Copy website
cp -RT public ~/psyverse.opens.science

### Copy .htaccess
cp -f .htaccess ~/psyverse.opens.science

echo ----------
