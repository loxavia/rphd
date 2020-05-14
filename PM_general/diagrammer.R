library(DiagrammeR)

graph <- create_graph()
class(graph)
get_node_df(graph)

ndf <- create_node_df(n=4, label=1:4, type='lower',style='filled', color='aqua',shape =c('circle','circle','rectangle','rectangle'), data=c(3.5, 2.6, 9.4,2.7))
ndf
graph <- create_graph(nodes_df = ndf)
get_node_df(graph)
graph %>% render_graph()
graph %>% generate_dot() %>% cat()
export_graph(graph, file_name = "E:/PMO/graph.pdf", title = "Simple Graph")

graph <-create_graph() %>%add_path(  n = 5,edge_aes = edge_aes( arrowhead = c("normal", "vee","tee", "dot"),color = c("red", "blue","orange", "purple") ))


graph %>%  export_graph(file_name = "E:/PMO/graph.pdf", title = "Simple Graph")
graph %>%  export_graph( file_name = "mypng.png",file_type = "PNG")
