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
      tab3 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab8;
      tab8 -> tab9;
      tab2 -> tab4;
      tab4 -> tab10;
      tab10 -> tab11;
      tab10 -> tab12;
      tab12 -> tab14;
      tab11 -> tab13;
      tab13 -> tab6;

      }

      [1]: 'getStandardData()'
      [2]: 'Is the file origin an ODF?'
      [3]: 'Yes'
      [4]: 'No'
      [5]: 'read.oce()'
      [6]: 'nameReplacement()'
      [7]: 'removeDerived()'
      [8]: 'fixMetadata()'
      [9]: 'convertNetCDF()'
      [10]: 'Is the file origin matlab?'
      [11]: 'Yes'
      [12]: 'No'
      [13]: 'matlabToOce()'
      [14]: 'Unable to convert this file type.'
      ")

if (!interactive()) dev.off()

