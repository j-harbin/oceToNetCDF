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
      tab3 -> tab5;
      tab5 -> tab11;
      tab11 -> tab12;
      tab12 -> tab14;
      tab14 -> tab15;
      tab15 -> tab16;
      tab16 -> tab17;
      tab17 -> tab18;
      tab2 -> tab4;
      tab4 -> tab6;
      tab6 -> tab7;
      tab7 -> tab9;
      tab9 -> tab11;
      tab11 -> tab13;
      tab13 -> tab15;
      tab6 -> tab8;
      tab8 -> tab10;

      }

      [1]: 'getStandardData()'
      [2]: 'Is file ODF origin?'
      [3]: 'Yes'
      [4]: 'No'
      [5]: 'read.oce()'
      [6]: 'Is file matlab origin?'
      [7]: 'Yes'
      [8]: 'No'
      [9]: 'matlabToOce()'
      [10]: 'Unable to convert this file type'
      [11]: 'Do you want IOOS standards'
      [12]: 'Yes'
      [13]: 'No'
      [14]: 'standardMetadata()'
      [15]: 'nameReplacement()'
      [16]: 'removeDerived()'
      [17]: 'fixMetadata()'
      [18]: 'convertNetCDF()'
      ")

if (!interactive()) dev.off()

