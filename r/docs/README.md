SHEDS Stream Temperature Model Documentation
============================================

Jeffrey D Walker, PhD  
[Walker Environmental Research, LLC](https://walkerenvres.com)

## About

This folder contains source code for generating the SHEDS stream temperature model documentation. This documentation should be updated with each new version of the model.

The documentation website is built using **bookdown** (https://github.com/rstudio/bookdown). See the page "[Get Started](https://bookdown.org/yihui/bookdown/get-started.html)" at https://bookdown.org/yihui/bookdown/ for how to compile this code, which was adapted from the "getting started" demo at https://bookdown.org/yihui/bookdown-demo/.

## Compiling

Open the R project file (`docs.Rproj`) within the R Studio IDE.

Click the `Build Book` button under the `Build` menu.

The output files will be generated in the `_book` sub-directory.

## Updating

When a new version of the stream temperature model is released, follow this checklist to update the documentation.

- [ ] Update version number and date in the YAML frontmatter of index.Rmd
- [ ] Update Change Log in index.Rmd
- [ ] Make any necessary changes to the underlying theory, processing, etc.
- [ ] Regenerate all pages to update the tables and figures
- [ ] Assign new tag on Github after final push

## Deploying

After completing an update, deploy the compiled files to ecosheds.org.

- [ ] Deploy the output (`_book/`) to the web server under the new version number
```txt
scp -r _book/* jeff@ecosheds.org:/conte/www/static/models/temp-model/<vX.Y.Z>/
```
- [ ] Update the stream temperature model homepage to list the newest version
- [ ] Update the `/latest` redirect in the web server to point to the newest version
- [ ] Add tag in github

