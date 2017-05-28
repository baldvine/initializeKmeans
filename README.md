# Initialization methods for the k-means clustering algorithm

Simple shiny app to visualize several initialization methods for the k-means algorithm.

The dataset is the ruspini dataset (from the *cluster* package), which has 4 distinct clusters. We investigate and visualize four initialization methods:

- **"Random"**: Randomly assign data points as intial clusters (best of 25 runs)
- **"KA method"**: From Kaufman & Roussef [1], and described in detail in [2].
- **"Extreme"**: First center furthest from origin. Subsequent centers are the furthest from their nearest cluster center. See reference [3].
- **"k-means++"**: First center chosen at random. Subsequent centers randomly, but weighted based on distance to nearest cluster center (best of 25 runs). See reference [4].

Note that all methods use data points as inital cluster centers. Both "KA method", and "Extreme" are deterministic.

## References

1. Leonard Kaufman and Peter J Rousseeuw. Finding Groups in Data. *Wiley
  Online Library*, 1990.

2. José Manuel Peña, Jose Antonio Lozano, and Pedro Larrañaga. An empirical
  comparison of four initialization methods for the k-means algorithm.
  *Pattern recognition letters*, 20(10):1027-1040, 1999.
  
3. Ioannis Katsavounidis, C-C Jay Kuo, and Zhen Zhang. A new initialization
  technique for generalized Lloyd iteration. *IEEE Signal processing letters*,
  1(10):144-146, 1994.
  
4. David Arthur and Sergei Vassilvitskii. k-means++: the advantages of careful
  seeding. In *Proceedings of the eighteenth annual ACM-SIAM symposium
  on Discrete algorithms*, pages 1027-1035, Philadelphia, 2007.