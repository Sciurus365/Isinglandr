# Development version (aimed for Isinglandr 0.2.0)

- Added functions `calculate_stability_se()` and `compare_stability()` for calculating the standard error of the stability metrics and comparing the stability metrics between two networks, respectively.
- Added the `autolayer.stability_2d_Isingland_matrix()` method. The structure of the `stability_2d_Isingland_matrix` objects changes slightly to accommodate the new method. Users may need to re-generate previously `stability_2d_Isingland_matrix` objects
- Bug-fix for `make_3d_Isingland()`:
	- The function now allows character values for the `x` and `y`	arguments.

# Isinglandr 0.1.1

- Bug-fix for the Shiny app (`shiny_Isingland_MDD()`):
	- Load the data to the Shiny app environment;
	- Check if `gifski` is installed, which is required to render the animations 
	(also added `gifski` as a suggested package).

# Isinglandr 0.1.0

- Initial release.
