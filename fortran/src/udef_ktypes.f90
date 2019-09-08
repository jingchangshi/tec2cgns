module udef_ktypes_mod
  !
  !.. Use Statements ..
  use iso_fortran_env, only : int32,int64, &
                              real32,real64,real128, &
                              output_unit,logical_kinds
  !
! #include <mpi_defs.h>
!   use _MPI_MODULE_
  !
  implicit none
  !
  public
  !
  ! KIND definitions for 32-bit or 64-bit integer variables (IEEE 754)
  !
  integer, parameter :: i4 = int32
  integer, parameter :: i8 = max( int32 , int64 )
  !
  ! KIND definitions for single or double precision real variables (IEEE 754)
  !
  integer, parameter :: r4 = real32
  integer, parameter :: r8 = real64
  integer, parameter :: r16 = max( real64 , real128 )
  !
  ! KIND definitions for logical variables
  !
  integer, parameter, private :: lb_lk = lbound(logical_kinds,dim=1)
  integer, parameter, private :: ub_lk = ubound(logical_kinds,dim=1)
  integer, parameter :: l1k = min(logical_kinds(min(lb_lk+0,ub_lk)), &
                                          logical_kinds(min(lb_lk+1,ub_lk)), &
                                          logical_kinds(min(lb_lk+2,ub_lk)), &
                                          logical_kinds(min(lb_lk+3,ub_lk)), &
                                          logical_kinds(min(lb_lk+4,ub_lk)))
  integer, parameter :: ldk = kind(.true.)
  !
  ! Unit for I/O to standard output
  !
  integer, parameter :: iout = output_unit
  !
  ! Aliases for kind type parameters
  !
  integer, parameter :: lk = l1k
  integer, parameter :: sp = r4
  integer, parameter :: dp = r8
  integer, parameter :: qp = r16
  integer, parameter :: wp = r8
  !
  integer, parameter :: wip = i4
  ! integer, parameter :: int_mpi = kind(mpi_integer_kind)
  ! integer, parameter :: int_metis = i4
  !
end module udef_ktypes_mod
