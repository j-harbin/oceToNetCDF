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
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      tab17 [label = '@@17']
      tab18 [label = '@@18']

      # edge definitions with the node IDs

      tab1 -> tab2;
      tab2 -> tab3;
      tab2 -> tab4;
      tab3 ->tab5;
      tab4 ->tab6;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab9;
      tab9 -> tab10;
      tab10 -> tab11;
      tab11 -> tab12;
      tab12 -> tab13;
      tab6 ->tab8;
      tab8 -> tab14;
      tab14 -> tab15;
      tab15 -> tab17;
      tab17 -> tab10;
      tab14 -> tab16;
      tab16 ->tab18;

      }

      [1]: 'getStandardData()'
      [2]: 'Do you want IOOS standards?'
      [3]: 'Yes'
      [4]: 'No'
      [5]: 'standardMetadata()'
      [6]: 'Is the file origin an ODF?'
      [7]: 'Yes'
      [8]: 'No'
      [9]: 'read.oce()'
      [10]: 'nameReplacement()'
      [11]: 'removeDerived()'
      [12]: 'fixMetadata()'
      [13]: 'convertNetCDF()'
      [14]: 'Is the file origin matlab?'
      [15]: 'Yes'
      [16]: 'No'
      [17]: 'matlabToOce()'
      [18]: 'Unable to convert this file type.'
      ")

if (!interactive()) dev.off()

