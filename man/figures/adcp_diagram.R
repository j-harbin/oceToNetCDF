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
      tab9 [label = '@@9']
      tab10 [label = '@@10']
      tab11 [label = '@@11']
      tab12 [label = '@@12']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab5;
      tab5 -> tab6;
      tab6 -> tab7;
      tab7 -> tab9;
      tab9 -> tab10;
      tab10 -> tab11;
      tab11 -> tab12;
      tab2 -> tab4;
      tab4 ->tab6;
      tab6 -> tab8;
      tab8 -> tab10;

      }

      [1]: 'getStandardData()'
      [2]: 'Does the data need to be compiled?'
      [3]: 'Yes'
      [4]: 'No'
      [5]: 'compileOdfToAdp()'
      [6]: 'Do you want IOOS standards?'
      [7]: 'Yes'
      [8]: 'No'
      [9]: 'standardMetadata()'
      [10]: 'nameReplacement()'
      [11]: 'structureAdp()'
      [12]: 'singleAdpNetCDF()'
      ")

if (!interactive()) dev.off()

