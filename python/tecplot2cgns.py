#############################
# This script requires that the tecplot ascii file renders its header
# in a specific format.
# The 1st line is the variables declaration
# The 2nd line contains ZONETYPE, SOLUTIONTIME, DATPACKING, NODES, ELEMENTS
# The 3rd line is the data type precision.
#############################
import numpy as np

tec_fpath = "../fortran/src/sol_tec.dat"

#############################
# Read tecplot file header
# Assume that each line gives info of each section
#   1st line is variables
#   2nd line is zone info
#   3rd line is datatype
#############################

tec_f = open(tec_fpath,'r')

line1 = tec_f.readline().upper()
line2 = tec_f.readline().upper()
line3 = tec_f.readline().upper()

tec_f.close()

line1_list = line1.strip().split("=")
assert(line1_list[0] == 'VARIABLES')
var_list = line1_list[1].split(',')

line2_list = line2.strip().split(",")

idx = [i for i, s in enumerate(line2_list) if 'ZONETYPE' in s]
assert(len(idx)==1)
zonetype_str = line2_list[idx[0]].strip().split('=')[1]

idx = [i for i, s in enumerate(line2_list) if 'SOLUTIONTIME' in s]
assert(len(idx)==1)
sol_time_str = line2_list[idx[0]].strip().split('=')[1]
sol_time = float(sol_time_str)

idx = [i for i, s in enumerate(line2_list) if 'DATAPACKING' in s]
assert(len(idx)==1)
datapacking_str = line2_list[idx[0]].strip().split('=')[1]
assert(datapacking_str == 'POINT')

idx = [i for i, s in enumerate(line2_list) if 'NODES' in s]
assert(len(idx)==1)
n_node = line2_list[idx[0]].strip().split('=')[1]
n_node = int(n_node)

idx = [i for i, s in enumerate(line2_list) if 'ELEMENTS' in s]
assert(len(idx)==1)
n_elem = line2_list[idx[0]].strip().split('=')[1]
n_elem = int(n_elem)
#############################
# Read the variables data
#############################

var_mat = np.genfromtxt(tec_fpath,skip_header=3,max_rows=n_node)
x_arr = var_mat[:,0]
y_arr = var_mat[:,1]
cell_id_arr = var_mat[:,2]
p_order_arr = var_mat[:,3]
cpu_id_arr = var_mat[:,4]
rho_arr = var_mat[:,5]
u_arr = var_mat[:,6]
v_arr = var_mat[:,7]
p_arr = var_mat[:,8]

# For now, skip the elem connectivity section. Because I do not need it.

##############################
# Write to CGNS file
##############################
import CGNS.PAT.cgnsutils as CGU
import CGNS.PAT.cgnslib as CGL
import CGNS.PAT.cgnskeywords as CK
import CGNS.MAP as CGM

T=CGL.newCGNSTree()
B=CGL.newBase(T,'hpMusic_base',2,2)
# The shape (3,1) is critical
zone_size = np.array([[n_node, n_elem, 0]])
Z=CGL.newZone(B,'Solution',zone_size,CK.Unstructured_s,'')
GC=CGL.newGridCoordinates(Z,name='GridCoordinates')
FS=CGL.newFlowSolution(Z,name='FlowSolution',gridlocation='Vertex')
GL=CGU.getNodeByPath(FS,'GridLocation')
CGU.nodeDelete(FS,GL)

coordinatex_node = CGL.newDataArray(GC,'CoordinateX',value=x_arr)
coordinatey_node = CGL.newDataArray(GC,'CoordinateY',value=y_arr)
density_node = CGL.newDataArray(FS,'Density',value=rho_arr)
velocityx_node = CGL.newDataArray(FS,'VelocityX',value=u_arr)
velocityy_node = CGL.newDataArray(FS,'VelocityY',value=v_arr)
p_node = CGL.newDataArray(FS,'Pressure',value=p_arr)

CGM.save("sol_tec.cgns",T)
