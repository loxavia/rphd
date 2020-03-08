


graph <-
  create_graph() %>%
  add_path(
    n = 5,
    edge_aes = edge_aes(
      arrowhead = c(
        "normal", "vee",
        "tee", "dot"
      ),
      color = c(
        "red", "blue",
        "orange", "purple"
      )
    )
  )
graph
graph %>% export_graph(file_name = "graph.pdf",title = "Simple Graph")

library(rsvg)

#First install DiagrammeRsvg and rsvg packages. At the end of code add thees lines:
  
export_graph(epm1, file_name = "pic.png",  file_type = "PNG")
list.files()
