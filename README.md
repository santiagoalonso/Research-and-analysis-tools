# Research-and-analysis-tools:

* MakeDotArray: (by Elon Gaffin-Cahn and some add-ons by me): Locates the center of many non-overlapping circles. Useful to draw stimuli for visual number tasks e.g. compare two clouds of dots and report which is numerically larger or compare two discrete visual ratios.

* within_sem: Computes means and std. error of mean for within subject designs. Useful to plot error bars because it "washes" away between subject variability.

* VisualDegreePixels: Converts pixels to visual degrees or pixels to visual degrees

* Traj_Norm: Normalizes a single movement trajectory in space or time by overfitting the movement trajectory to a piecewise polynomial, and in the case of space normalization by also finding inverse functions. Based on Gallivan and Chapman (2014)* in http://www.per.ualberta.ca/acelab/?page_id=372, but in my opinion this is a simplified version. *"Three-dimensional reach trajectories as a probe of real-time decision-making between multiple competing targets".

* Perp_rot: Perpendicular rotation of a normal vector of a 3D plane so that it lies on the plane surface. The rotation is further constrained to be perpendicular to another vector on the plane (i.e otherwise there are infinitely many)

* twoDpixels_to_3Dmm: Useful application of Perp_rot. This function will transform 2D pixels coordinates to 3D coordinates(mm). You may want to do this to make figures in a paper and show where in 3D space the stimuli on-screen was located. Or to make a display touchable with infrared apparatus (e.g. optotrak).

* inpaint_nans: in-paints over nans in a 2D array by interpolation (e.g. a movement vector). But see a more fancy approach applicable to higher dimensional objects by John D'Errico: https://www.mathworks.com/matlabcentral/fileexchange/4551-inpaint-nans. 

