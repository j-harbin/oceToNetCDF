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
      node [fontname = Helvetica, shape = rectangle, color=blue]
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']
      tab15 [label = '@@15']
      tab16 [label = '@@16']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab8;
      tab9 -> tab10;
      tab10 -> tab11;
      tab11 -> tab12;
      tab12 -> tab13;
      tab13 -> tab14;
      tab14 -> tab15;
      tab15 -> tab16;
      }

      [1]: 'Check ODF metadata'
      [2]: 'Fix necessary changes'
      [3]: 'Remove computed data'
      [4]: 'Remove computed metadata'
      [5]: 'Convert CR to conductivity'
      [6]: 'Check range of conductivity'
      [7]: 'Fix metadata'
      [8]: 'Convert to NetCDF'
      [9]: 'odfMetadataCheck()'
      [10]: 'nameReplacement()'
      [11]: 'ctdRemoveDerivedData()'
      [12]: 'ctdRemoveDerivedMetadata()'
      [13]: 'convertConductivityRatio()'
      [14]: 'checkCrat()'
      [15]: 'fixMetadata()'
      [16]: 'convertNetCDF()'
      ")

if (!interactive()) dev.off()

