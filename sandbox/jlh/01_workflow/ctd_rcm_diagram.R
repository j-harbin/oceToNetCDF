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
      node [fontname = Helvetica, shape = rectangle, color=blue]
      tab11 [label = '@@11']
      tab12 [label = '@@12']
      tab13 [label = '@@13']
      tab14 [label = '@@14']
      tab15 [label = '@@15']
      tab16 [label = '@@16']
      tab17 [label = '@@17']
      tab18 [label = '@@18']
      tab19 [label = '@@19']
      tab20 [label = '@@20']





      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab8;
      tab8 -> tab9;
      tab9 -> tab10;
      tab11 -> tab12;
      tab12 -> tab13;
      tab13 -> tab14;
      tab14 -> tab15;
      tab15 -> tab16;
      tab16 -> tab17;
      tab17 -> tab18;
      tab18 -> tab19;
      tab19 -> tab20
      }

      [1]: 'Get necessary data'
      [2]: 'Read ODF data'
      [3]: 'Check ODF metadata'
      [4]: 'Fix necessary changes'
      [5]: 'Remove computed data'
      [6]: 'Remove computed metadata'
      [7]: 'Convert CR to conductivity'
      [8]: 'Check range of conductivity'
      [9]: 'Fix metadata'
      [10]: 'Convert to NetCDF'
      [11]: 'getData()'
      [12]: 'read.oce()'
      [13]: 'odfMetadataCheck()'
      [14]: 'nameReplacement()'
      [15]: 'ctdRemoveDerivedData()'
      [16]: 'ctdRemoveDerivedMetadata()'
      [17]: 'convertConductivityRatio()'
      [18]: 'checkCrat()'
      [19]: 'fixMetadata()'
      [20]: 'convertNetCDF()'
      ")

if (!interactive()) dev.off()

