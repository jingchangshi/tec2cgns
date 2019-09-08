This repo contains 2 versions. One is Fortran and the other is Python. Fortran version uses the CGNS native library compiled locally. Python version uses pyCGNS.

Fortran works as expected while Python version shows strange behaviors. In Python, the solution matrix including the coordinates is consistent with the Tecplot solution file. However the output CGNS file seems like it transposes the original Tecplot solution matrix. No idea why.

So use the Fortran version.

Fortran version is created for my own needs. It is not a real general version.

The tecplot2cgns shipped in the CGNS source codes is a bad version with limited feature and seems like limited to 3D. It cannot be used for 2D data. This is what I know after I have a look at its source codes. So I create this repo.

