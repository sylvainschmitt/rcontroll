## Test environments
* local Ubuntu 16.04 LTS, R 3.3.2
* Travis-CI Ubuntu 12.04 LTS, R 3.3.2

## R CMD check results
There were no ERRORs, WARNINGs.

There was 1 NOTE: .get_control: no visible global function definition for ‘ggplot’ .get_control: no visible
    global function definition for ‘aes’ .get_control: no visible binding for global variable ‘values’ .get_control: no
    visible binding for global variable ‘layer’ .get_control: no visible global function definition for ‘geom_linerange’
    .get_control: no visible global function definition for ‘geom_line’ .get_control: no visible global function
    definition for ‘xlab’ .get_control: no visible global function definition for ‘ylab’ .get_control: no visible global
    function definition for ‘theme_bw’ .ggplot_graph: no visible global function definition for ‘ggplot’ .ggplot_graph:
    no visible global function definition for ‘aes’ .ggplot_graph: no visible binding for global variable ‘values’
    .ggplot_graph: no visible binding for global variable ‘layer’ .ggplot_graph: no visible global function definition
    for ‘geom_point’ .ggplot_graph: no visible global function definition for ‘geom_line’ .ggplot_graph: no visible
    global function definition for ‘theme_bw’ .ggplot_hist: no visible global function definition for ‘ggplot’
    .ggplot_hist: no visible global function definition for ‘aes’ .ggplot_hist: no visible binding for global variable
    ‘values’ .ggplot_hist: no visible binding for global variable ‘layer’ .ggplot_hist: no visible global function
    definition for ‘geom_histogram’ .ggplot_hist: no visible global function definition for ‘scale_x_continuous’
    .ggplot_hist: no visible global function definition for ‘scale_y_continuous’ .ggplot_hist: no visible global
    function definition for ‘theme_bw’ plot,TROLLsimstack-missing: no visible global function definition for ‘ggplotly’
    Undefined global functions or variables: aes geom_histogram geom_line geom_linerange geom_point ggplot ggplotly
    layer scale_x_continuous scale_y_continuous theme_bw values xlab ylab
    
This is due to suggested dependency use with requireNamespace.
 