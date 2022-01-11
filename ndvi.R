usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::use_github()


ndiff <- function(b1,b2){
  return((b1-b2)/(b1+b2))
}
