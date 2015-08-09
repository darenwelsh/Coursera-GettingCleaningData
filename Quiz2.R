getRepoInitDate <- function(){
    myapp <- oauth_app("github"
        , key = "fb7276f0d1e4a43bc892"
        , secret = "96a04b7849088f2ed5baf826c658b67fb18a7a8d"
        )
#     sig = sign_oauth2.0(myapp
#         #, token = "7b5d8dc94b07ac63f524bdb914d17761c5b68a9b"
#         #, token_secret = "96a04b7849088f2ed5baf826c658b67fb18a7a8d"
#         )
    
    #myapp <- oauth_app("github",key="add_your_registered_key",secret = "add_your_secret") 
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp) 
    gtoken <- config(token = github_token)
    
    #data <- GET("https://api.github.com/users/jtleek/repos", gtoken)
    #jsonData <- toJSON(data)
    
    request <- with_config(gtoken, GET("https://api.github.com/users/jtleek/repos"))
    stop_for_status(request)
    repocontent <- content(request)
    
    repocontent <- fromJSON(toJSON(repocontent))
    
    datasharingrepo <- repocontent[repocontent$name == "datasharing",]
    datasharingcreation <- datasharingrepo$created_at
    
    datasharingcreation
    
}

