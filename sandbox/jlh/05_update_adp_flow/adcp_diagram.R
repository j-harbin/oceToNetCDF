# Figure 1: Flow Chart of the action taken when a user applies
# useAdjusted(which=ALL)

if (!interactive()) png("diagram.png")

library(DiagrammeR)
grViz("digraph flowchart {
      node [fontname = Helvetica, shape = rectangle]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']
      tab7 [label = '@@7']
      tab8 [label = '@@8']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab2 -> tab4;
      tab3 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab8;
      tab4 -> tab7;

      }

      [1]: 'getCFData()'
      [2]: 'Does the data need to be compiled?'
      [3]: 'Yes'
      [4]: 'No'
      [5]: 'compileOdfToAdp()'
      [6]: 'nameReplacement()'
      [7]: 'structureAdp()'
      [8]: 'singleAdpNetCDF()'
      ")

if (!interactive()) dev.off()

