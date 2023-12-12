git config --list
git config --global user.email "arz1@geneseo.edu"
git config --global user.name "1Aaronz"
git config --global user.password "4PK9KeWRhyW-6Em"

install.packages("usethis")
usethis::create_github_token()

install.packages("gitcreds")
gitcreds::gitcreds_set()

install.packages(c("hrtbrthemes, ggthemes")) #not installed yet


# In terminal -------------------------------------------------------------

git add .
git commit -m “ANY_MASSAGE”
git push -u origin main
quarto render

execute:
  echo:TRUE # Shows code, set to true
  eval:TRUE #Set to true, run code  typed
  warning:FALSE #Set to false if dont want to see warnings
  message:FALSE 
  
# getwd() to make sure working directory is correct

ggsave("diamonds.png") # to save ggplot as a png file.
write_csv(diamonds, "diamonds.csv") # to save data.frame as a csv file


# to create new blog post: copy in blog


# Quarto ------------------------------------------------------------------

quarto render
