# GAM.PELT
GAM.PELT has been designed to utilise generalised additive models (GAMs) in conjunction with the Pruned Linear Exact Time (PELT) changepoint detection algorithm to detect spatio-temporal changepoints in datasets.

This a spatiotemporal changepoint method that utilises a generalised additive model (GAM) dependent on the 2D spatial location and the observation time to account for the underlying spatio-temporal process. The full likelihood of the GAM is used in conjunction with the Pruned Linear Exact Time (PELT) changepoint search algorithm to detect multiple changepoints across spatial locations in a computationally efficient manner. Although the method was conceived to detect changepoints in spatio temporal datasets it can be run using any GAM model structure as input. 
