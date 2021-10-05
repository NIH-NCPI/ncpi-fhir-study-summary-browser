## FHIR server helper functions
#Create some reading functions to query the fhir server and extract resource responses. 

simple_get = function(request) {
  res=GET(request,add_headers(Authorization=sprintf("Bearer %s",gar_token()$auth_token$credentials$access_token)))
  con=content(res,as="parsed",type="application/json")
  #If it fails unauthorized, refresh token and try again
  if(!is.null(con$error) && con$error$code==401) {
    gar_token()$auth_token$refresh()
    res=GET(request,add_headers(Authorization=sprintf("Bearer %s",gar_token()$auth_token$credentials$access_token)))
    con=content(res,as="parsed",type="application/json")
  }
  con
}

default_get=function(request) {
  simple_get(paste0(input$server,request))
}


get_all=function(request) {
  next_request=paste0(input$server,request)
  all_content=list()
  while(next_request!="") {
    my_content=simple_get(next_request)
    if(my_content$resourceType=="Bundle") {
      all_content=append(all_content,lapply(my_content$entry,function(x){x$resource}))
      next_request=paste0(sapply(my_content$link,function(x){ifelse(x$relation=="next",x$url,"")}),collapse = "")
    }
    else {
      all_content=append(all_content,list(my_content))
      next_request=""
    }
  }
  all_content
}

#Note this does not handle errors, could check each return for an $issue
get_resource_list=function(request_list) {
  all_items=lapply(request_list,default_get)
}

parse_extensions = function(resource){
  tibble(ext = resource$extension) %>% 
    unnest_wider(ext) %>% 
    unnest_longer(extension) %>% 
    hoist(extension, key=list("url")) %>% 
    hoist(extension, value=1)
}


parse_groups = function(groups) {
  bind_rows(lapply(groups, parse_group))
}

parse_group = function(group) {
  data.frame(group_id=group$id,
             group_name=group$name,
             n_participants=group$quantity,
             complete_group=grepl("-complete",group$name)
  )
}


#This function pulls out a nice table of summary stats from componetnts based on a given set of resources
extract_stats = function(resources) {
  tibble(resources=resources) %>% 
    hoist("resources", term_name=list("valueCodeableConcept","text")) %>% 
    hoist("resources", group=list("subject","reference")) %>% 
    hoist("resources", component=list("component")) %>% 
    unnest_longer(component) %>% 
    unnest_wider(component) %>% 
    unnest_wider(code) %>% 
    transmute(term_name, component_text=text, component_value=valueInteger, group_reference=group)
  
}
parse_components = function(resource){
  tibble(ext = resource$component) %>% 
    unnest_wider(ext) %>% 
    unnest_longer(code) %>% 
    filter(code_id=="text") %>% 
    hoist(code,key=list(1))
}