# Initialization methods for the k-means clustering algorithm

Simple shiny app to visualize several initialization methods for the k-means algorithm. Application is hosted here: <a href="https://baldvine.shinyapps.io/initializeKmeans/" target="_blank">https://baldvine.shinyapps.io/initializeKmeans/</a>

The default dataset is the ruspini dataset (from the *cluster* package), which has 4 distinct clusters. There are multiple data sets to choose from. We investigate and visualize four initialization methods:

- **"Random"**: Randomly assign data points as intial clusters (best of 25 runs)
- **"KA method"**: From Kaufman & Roussef [1], and described in detail in [2]. This produces great results, but it is very computationally expensive.
- **"Extreme"**: First center furthest from origin. Subsequent centers are the furthest from their nearest cluster center. See reference [3].
- **"k-means++"**: First center chosen at random. Subsequent centers randomly, but weighted based on distance to nearest cluster center (best of 25 runs). See reference [4].

Note that all methods use data points as inital cluster centers. Both "KA method", and "Extreme" are deterministic.

## References

1. Leonard Kaufman and Peter J Rousseeuw. Finding Groups in Data. *Wiley
  Online Library*, 1990.

2. Jos&eacute; Manuel Pe&ntilde;a, Jose Antonio Lozano, and Pedro Larra&ntilde;aga. An empirical
  comparison of four initialization methods for the k-means algorithm.
  *Pattern recognition letters*, 20(10):1027-1040, 1999.
  
3. Ioannis Katsavounidis, C-C Jay Kuo, and Zhen Zhang. A new initialization
  technique for generalized Lloyd iteration. *IEEE Signal processing letters*,
  1(10):144-146, 1994.
  
4. David Arthur and Sergei Vassilvitskii. k-means++: the advantages of careful
  seeding. In *Proceedings of the eighteenth annual ACM-SIAM symposium
  on Discrete algorithms*, pages 1027-1035, Philadelphia, 2007.
  
See also:

5. Jain, Anil K. Data clustering: 50 years beyond K-means. 
   *Pattern recognition letters*, 31(8), pp.651-666, 2010.
