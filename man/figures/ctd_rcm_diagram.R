if (!interactive()) png("ctd_rcm_workflow.png")

library(DiagrammeR)
grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      node [fontname = Helvetica, shape = rectangle, color=blue]
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab7 -> tab8;
      tab8 -> tab9;
      tab9 -> tab10;
      tab10 -> tab11;
      tab11 -> tab12;
      }

      [1]: 'Get necessary data'
      [2]: 'Read ODF data'
      [3]: 'Fix necessary changes'
      [4]: 'Remove derived data/metadata'
      [5]: 'Polish ODF'
      [6]: 'Convert to NetCDF'
      [7]: 'getData()'
      [8]: 'read.oce()'
      [9]: 'nameReplacement()'
      [10]: 'removeDerived()'
      [11]: 'polishODF()'
      [12]: 'convertNetCDF()'
      ")

if (!interactive()) dev.off()

