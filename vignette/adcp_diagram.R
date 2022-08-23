if (!interactive()) png("adp_workflow.png")

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      node [fontname = Helvetica, shape = rectangle, color=blue]
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab4 -> tab5;
      tab5 -> tab6;
      }

      [1]: 'Get necessary data'
      [2]: 'Compile one adp object from multiple ODF'
      [3]: 'Create a single NetCDF'
      [4]: 'getData()'
      [5]: 'compileOdfToAdp()'
      [6]: 'singleAdpNetCDF()'
      ")

if (!interactive()) dev.off()


