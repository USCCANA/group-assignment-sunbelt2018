# Simulations

1.  Generate the data by running `dat.r`

2.  Then, run the following scripts:

    *   leaders-girvan-newman.r
    
    *   leaders-group-assignment.r
    
    *   leaders-indegree.r
    
    *   leaders-keyplayer.r
    
    *   leaders-mentor-match.r
    
    *   leaders-random-groups.r
    
    
    This will run the different algorithms to indentify leaders.

3.  Then, you can run (in any order) the following scripts:
    
    *   `diffnet.r`: Which will simulate diffusion networks in all combinations
        of `leader`+`model` (threshold) + `type of network`.
    
    *   `overlap.r`: Which computes the overlap of leaders (jaccard index).
    
    *   `group-overlap.r`: Which computes the a group overlap statistic based
        on hamming distances.
    
