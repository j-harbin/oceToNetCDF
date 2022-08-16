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
      tab7 [label = '@@7']
      node [fontname = Helvetica, shape = rectangle, color=blue]
      tab8 [label = '@@8']
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab8 -> tab9;
      tab9 -> tab10;
      tab10 -> tab11;
      tab11 -> tab12;
      tab12 -> tab13;
      tab13 -> tab14;
      }

      [1]: 'Get necessary data'
      [2]: 'Read ODF data'
      [3]: 'Check ODF metadata'
      [4]: 'Fix necessary changes'
      [5]: 'Remove derived data/metadata'
      [6]: 'Polish ODF'
      [7]: 'Convert to NetCDF'
      [8]: 'getData()'
      [9]: 'read.oce()'
      [10]: 'odfMetadataCheck()'
      [11]: 'nameReplacement()'
      [12]: 'removeDerived()'
      [13]: 'polishODF()'
      [14]: 'convertNetCDF()'
      ")

if (!interactive()) dev.off()

