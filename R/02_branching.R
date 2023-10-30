
#' Create node branch list breath first function
#' @param graph_layout active graph layout that is used, at this stage with unique x positions per unique branch
#' @param edges edge list
#' @param edge_num_size_filter numeric, give the number of cells that must be surpassed to draw edge, has to be set for each data set as its highly dependent on nr of cells within data
#' @param edge_prop_size_filter numeric, give the proportion of cells that must move between nodes to draw edge
#' @param reduce logical, TRUE to walk as few paths as possible while still visiting every node, FALSE to walk any possible path passing the edge_filters; warning memory expensive, adjust branches_overflow parameter! Only relevant if suggest_cut is TRUE.
#' @param silent logical, FALSE to suppress messages and warnings
#' @param debugg logical, if TRUE prints mesage regarding skipping branches, default to false
#' @param branches_overflow numeric giving the max number of paths through the graph that is checked for each resolution step to protect memory overflow

#' @import tibble rlang dplyr stringr



create_node_branch_list_bf <- function(graph_layout = graph_layout,
                                       edges = edges,
                                       edge_num_size_filter = 0,
                                       edge_prop_size_filter = 0.05,
                                       reduce = T, debugg = T, silent = F,
                                       branches_overflow = 5000){


  graph_str <- graph_layout %>% as_tibble() %>%
    arrange(.data$resolution) %>%
    select(x,y,resolution, cluster, id)
  visits_list <- list()

  res_steps <- unique(graph_str$resolution)
  for (n in seq_along(res_steps)){
    ##For each reoslution step:-----
    if (debugg) print(c(n, res_steps[n]))

    #is a graph present?
    if (n == 1) {
      #if its the first resolution step no graph can be present; initiate:
      c_step <- graph_str %>% filter(resolution == res_steps[n])
      branch_str <- matrix(nrow = length(res_steps), ncol = nrow(c_step),
                           dimnames = list(res_steps, pull(c_step, .data$id)))
      branch_str[n,] <- pull(c_step, id)

      if (debugg) print("Branch Structure created")
    } else {
      #any other situation should expand the existing branch_str

      if (!reduce & !silent){
        print(paste("Current Iteration: ", n ," of ", length(res_steps),
                    "  counting ", formatC(ncol(branch_str), width = 4), " individual branches"))
      }

      if ((ncol(branch_str) > branches_overflow) & !reduce) {
        warning(paste("Branch creation is overflowing!
                Algorithm is not optimized to handle too many branches.
                      Setting reduce_branchlist = TRUE at resolution step ", n, " with ",
                      ncol(branch_str), " nr of branches.
                      Tip: Either raise branch_overflow to create more total recombinations,
                or edge_prop_size filter to take only most important paths through the graph."))
        reduce = TRUE
        # n = n-1 #set n one back as current level is not computed anymore
        # break
      }

      if (debugg) print(paste("Resolution Level: ",n))
      ####For a reduced graph:
      # have as little visits to from_visits as possible
      # while still reaching every node at least once

      ####For a full graph:
      # have all edges visited which pass edge_num/prop_filter arguments

      #setup Previous nodes (n-1)
      #check from which nodes current nodes can derive from (from_node)
      p_step <- graph_str %>% filter(resolution == res_steps[n-1]) #previous step
      p_nodes <- p_step$id
      #trace how often each prev_node was used
      from_visits <- setNames(vector(mode = "numeric", length = nrow(p_step)),
                      nm = p_nodes)

      #Current Nodes (n) are in current step c_step, they all must be visited
      c_step <- graph_str %>% filter(resolution == res_steps[n]) #current step
      #Ids of current nodes, which are to be visited
      cur_nodes <- pull(c_step, id)
      to_visits <- setNames(vector(mode = "numeric", length = length(cur_nodes)),
                            nm = cur_nodes)


      #now attach each cur_node of c_step to existing structure:
      ###For loop v per node of previous resolution ----
      for (v in seq_along(p_nodes)){

        #get how many edges go out from p_node[v]
        p_edges <- edges[edges$from == p_nodes[v],]
        # p_edges <- edges %>% filter(.data$from == p_nodes[v])
        p_edge_to <- p_edges$to

        if (length(p_edge_to) == 1) {
          #only one choice of input edge for current node
          p_edge_from <- p_edges$from #get where the edge comes from and attach it to branch_str
          branch_col <- branch_str[n-1,] == p_edge_from #which columns of branch_str to overwrite at n
          branch_str[n,branch_col] <- p_edge_to #write which edge followed soley out of previous node

          #check progress
          from_visits[p_edge_from] <- from_visits[p_edge_from]+1
          to_visits[p_edge_to] <- to_visits[p_edge_to]+1
        } else if (length(p_edge_to) > 1) {
          #multiple choices to reach current node, either reduce or walk all paths
          ##Two events possible:
          #A split of a previous node
          #A merge of two previous nodes (cycle)

          if (reduce) {
            #reduce == TRUE

            #Only take strongest edges from previous layers:
            p_strong_edge <- ungroup(p_edges) %>%
              slice_max(order_by = .data$e_prop_size,
                        n = 1, with_ties = FALSE)

            p_edge_from <- p_strong_edge$from #get where the edge comes from and attach it to branch_str
            branch_col <- branch_str[n-1,] == p_strong_edge$from #which columns of branch_str to overwrite at n
            branch_str[n,branch_col] <- p_strong_edge$to #write which edge followed soley out of previous node

            #check progress
            from_visits[p_edge_from] <- from_visits[p_edge_from]+1
            to_visits[p_strong_edge$to] <- to_visits[p_strong_edge$to]+1

          } else {
            #reduce == FALSE
            #apply filter for search of valid edges:
            p_edges_choices <- edges %>% filter(.data$from == p_nodes[v] &
                                          .data$e_prop_size >= edge_prop_size_filter &
                                          .data$e_size >= edge_num_size_filter)
            if (nrow(p_edges_choices) == 0) {
            #filter would cutout all edges derived by from_node, in this case, take the strongest node by prop:
            p_edges_choices <- ungroup(p_edges) %>%
              slice_max(order_by = e_prop_size,
                        n = 1, with_ties = FALSE)
            }
            p_edges_to <- p_edges_choices$to

            ##loop through all test passing edges
            for (passing_edge in seq_along(p_edges_to)){
              #first edge takes current structure, followups will duplicate existing branches
              #to create new branch paths:

              if (passing_edge == 1){

                branch_col <- branch_str[n-1,] == p_nodes[v] #save configuration of selected columns
                branch_str[n, branch_col] <- p_edges_to[passing_edge]

                #check progress
                from_visits[p_nodes[v]] <- from_visits[p_nodes[v]]+1
                to_visits[p_edges_to[passing_edge]] <- to_visits[p_edges_to[passing_edge]]+1
              } else {
                # any other edge path: duplicate old set of branches and append new path node to it
                tmp_branches <- as.matrix(branch_str[,which(branch_col)])
                tmp_branches[n,] <- p_edges_to[passing_edge]

                branch_str <- cbind(branch_str,tmp_branches)

                #check progress
                from_visits[p_nodes[v]] <- from_visits[p_nodes[v]]+1
                to_visits[p_edges_to[passing_edge]] <- to_visits[p_edges_to[passing_edge]]+1

              }


            }


          }


        } else {
          #no edge to reach current node, node cannot appear at later resolution,
          #this would mean not all cells are assigned to clusters across resolution change

          stop(paste("Node ", v," of resolution ", n," has no edge towards it.
                     Nodes are intialized at first resolution step and followed through
                     change in resolution and therefore should always contain a population
                     of cells going towards them." ))
        }
      } #end of v loop

        ###now check how many nodes of resolution n have not been reached yet:
        #
        # any
        if (any(to_visits == 0)){
          if (!reduce & debugg) warning("Fixing nodes should not occur during reduce = FALSE")
          #some nodes have not yet been visited and therefore must be placed into the graph now
          #occurs when a node of previous resolution has more than one edge out
          ###Add paths to so far unreached current_nodes of step n
          c_nodes_missing <- to_visits[to_visits == 0]

          for (c_node_miss in seq_along(c_nodes_missing)){
            ##this should mostly appear during reduce = TRUE node splitting

            #get which strong edge leads to this current node:
            p_strong_edge <- ungroup(edges) %>%
              filter(.data$to == names(c_nodes_missing[c_node_miss])) %>%
              slice_max(order_by = e_prop_size, n = 1, with_ties = FALSE)

            #pick the first available branch path for duplication:
            branch_col <- first(which(branch_str[n-1,] == p_strong_edge$from))

            #then duplicate branch to create a new path for the unreachable current node
            tmp_branches <- as.matrix(branch_str[,branch_col])
            tmp_branches[n,] <- names(c_nodes_missing[c_node_miss])

            branch_str <- cbind(branch_str,tmp_branches)

            #check progress
            from_visits[p_strong_edge$from] <- from_visits[p_strong_edge$from]+1
            to_visits[names(c_nodes_missing[c_node_miss])] <- to_visits[names(c_nodes_missing[c_node_miss])]+1



          } #end of c_node_miss loop

          # need_fix <- to_visits[to_visits == 0]
          # print(paste("Node wasn't visited: ",names(need_fix)))

        }

      }


  }#end of n loop


  plot_order <- graph_str[graph_str$resolution == n,] %>% arrange(x) %>% pull(.data$id)

  branch_order <- vector(mode = "numeric", length = 0)

  for (i in seq_along(plot_order)){
  branch_order <- c(branch_order,which(branch_str[n,] == plot_order[i]))
  }
  if(any(branch_order == 0)) stop("Stop, some rbanches could not be ordered!")
  branch_str <- branch_str[,branch_order]
  colnames(branch_str) <- seq_len(ncol(branch_str))
  # branch_df <- as_tibble(branch_str, .name_repair = "minimal")
  # colnames(branch_df) <- as.character(seq_len(ncol(branch_df)))

  return(as_tibble(branch_str,.name_repair = "minimal"))
  # return(branch_str)
}

