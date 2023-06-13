#Mateusz Wiejak mw117894
library(pryr)
library(igraph)

listAllEnvs <- function(){
  
  # Function for getting standard searchpath
  listEnvs <- function(envs = list(globalenv())) {
    last <- function(s) { s[[length(s)]] }
    
    if(identical(last(envs), emptyenv())) {
      return(envs)
    }
    
    listEnvs(c(envs, list(parent.env(last(envs)))))
  }
  
  # Convenience function
  index <- function(element, list){
    indx <- -1
    for (i in 1:length(list)){
      if (identical(element, list[[i]])){
        indx <- i
        break
      }
    }
    if(indx == -1){
      for (i in 1:length(list)){
        if (identical(element, attributes(list[[i]])[[1]])){
          indx <- i
          break
        }
      }
    }
    return (indx)
  }
  
  # Output list of environemnts
  env_list <- listEnvs()
  
  #Creating dataframe of edges 
  environments_names <- c()
  for(i in 1:length(env_list)){
    env_name <- environmentName(env_list[[i]])
    
    if(isNamespace(env_list[i])){
      env_name <- paste("namespace:",env_name)
    }
    
    environments_names <- c(environments_names,env_name)
  }
  
  env_df <- data.frame(edge1 = environments_names[1:length(environments_names)-1], edge2 = environments_names[2:length(environments_names)])
  
  # List of all environments that will be found from this moment. Used to track if an environemnt was already accessed before.
  big_env_list <- list()
  

  main_env = environment()

  # Function responsible for searching for all accessible environments.
  # Main idea: iterating over elements of ls() function, and running environemnt() function on them  
  
  deep_env_search <- function(envir){
    
    elements_vec <- ls(envir = envir, all.names = T)
    local_env_list <- list()  
    
    for (element_char in elements_vec){
      
      element <- get(element_char, envir = envir)
      already_checked <- F
      
      if(is.environment(element)){
        
        for (ref_env in get('big_env_list', envir = main_env)){
          if(identical(element, ref_env)){
            already_checked <- T
            
          }
        }
        
        if(!already_checked){
          local_env_list <- append(local_env_list, list(listEnvs(list(element))))
          assign("big_env_list", append(main_env$big_env_list, element), envir = main_env)
        }
      }
      
      else{
        
        def_env <- environment(element)
        
        if(!is.null(def_env)){
          
          for (ref_env in get('env_list', envir = main_env)){
            if(identical(def_env, ref_env)){
              already_checked <- T
              
            }
          }
          
          if(!already_checked){
            local_env_list <- append(local_env_list, list(listEnvs(envs = list(def_env))))
            assign("big_env_list", append(main_env$big_env_list, def_env), envir = main_env)
          }
        }
      }
    }
    return(local_env_list)
  }
  
  big_list <- list()
  
  for (env in env_list) {
    big_list <- append(big_list, deep_env_search(env))
  }
  
  
  
  # Code for adding environemnts to output env_list, with checking for duplicates
  add_to_env_list <- function(local_env_list){
    for (i in 1:length(local_env_list)){
      
      already_in_list <- F
      env <- local_env_list[[i]]

      
      if(i < length(local_env_list)){
        parent <- local_env_list[[i+1]]  
      }
      
      for (listed_env in env_list){
        if(identical(env,listed_env)){
          
          already_in_list <- T
        }
        
      }
      
      if(!already_in_list){

        assign("env_list",  append(main_env$env_list, env), envir = main_env)
        env_name <- environmentName(env)
        parent_name <- environmentName(parent)

        if(env_name == ""){

          env_name <- address(env)
        }
        
        if(parent_name == ""){

          parent_name <- address(parent)
        }
        if(isNamespace(env)){
          env_name <- paste("namespace:",env_name)
        }
        if(isNamespace(parent)){
          parent_name <- paste("namespace:",parent_name)
        }

        env_df <- main_env$env_df
        env_df[nrow(env_df)+1,] <- list(env_name, parent_name)
        assign("env_df", env_df, envir = main_env)
      }
    }
  }
  
  for (l in big_list){
    add_to_env_list(l)
  }
  
  rm(big_list)
  env_graph <- graph_from_data_frame(d = env_df)
  
  #Rearrange environemnts in output list, so numbers on graph indicate environments properly.
  v_set <- vertices(env_graph)[[1]]
  list_emptyenv_index <- index(emptyenv(), env_list)
  v_emptyenv_index <- index(environmentName(emptyenv()), v_set)
  env_list <- env_list[-list_emptyenv_index]
  env_list[[v_emptyenv_index]] <- emptyenv()

  structure(list(env_graph = env_graph, env_list  = env_list), class = "envgraph")
}



#Plotting  function
plot.envgraph <- function(env_graph, ...){
  plot.igraph(env_graph$env_graph, ...)
}

#Printing function
print.envgraph <- function(envgraph, ...){
  print("--> envgraph object")
  print(envgraph$env_list)
}

#Example of use
envgraph_example <- listAllEnvs()
print(envgraph_example)
plot(envgraph_example$env_graph, vertex.color = "yellow", vertex.label.cex = .5, edge.arrow.size=0.07,vertex.label = 1:length(envgraph_example$env_graph))


e <- new.env(parent = emptyenv())
attr(e, "name") <- "new environment"
e

w <- listAllEnvs()
w

plot(w, vertex.color = "yellow", vertex.label.cex = .7,edge.arrow.size=0.07,vertex.label = 1:length(envgraph_example$env_graph))