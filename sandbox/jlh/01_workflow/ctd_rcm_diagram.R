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
      node [fontname = Helvetica, shape = rectangle, color=blue]
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
      tab3 -> tab4;
      tab4 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab8;
      tab8 -> tab9;
      tab10 -> tab11;
      tab11 -> tab12;
      tab12 -> tab13;
      tab13 -> tab14;
      tab14 -> tab15;
      tab15 -> tab16;
      tab16 -> tab17;
      tab17 -> tab18;
      }

      [1]: 'Get necessary data'
      [2]: 'Read ODF data'
      [3]: 'Check ODF metadata'
      [4]: 'Fix necessary changes'
      [5]: 'Remove derived data/metadata'
      [6]: 'Convert CR to conductivity'
      [7]: 'Check range of conductivity'
      [8]: 'Fix metadata'
      [9]: 'Convert to NetCDF'
      [10]: 'getData()'
      [11]: 'read.oce()'
      [12]: 'odfMetadataCheck()'
      [13]: 'nameReplacement()'
      [14]: 'removeDerived()'
      [15]: 'convertConductivityRatio()'
      [16]: 'checkCrat()'
      [17]: 'fixMetadata()'
      [18]: 'convertNetCDF()'
      ")

if (!interactive()) dev.off()

