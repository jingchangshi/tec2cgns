program tec2cgns

  use udef_ktypes_mod

  use cgns

  implicit none

  integer :: tec_fn = 1
  ! TODO: get it from cmd arguments
  character(len=256) :: tec_fname = 'sol_tec.dat'
  character(len=256) :: line = ''
  character(len=256) :: iterm_str = ''
  character(len=32), dimension(:), allocatable :: var_list
  character(len=64), dimension(:), allocatable :: zone_term_list
  character(len=64), dimension(:), allocatable :: dat_list
  character(len=32) :: str1,str2
  character(len=32) :: zonetype,dat_pack
  integer :: ios
  !
  integer :: idx,idx_last,i,j
  integer :: n_node,n_elem,n_var
  real(wp) :: sol_time
  real(wp), dimension(:,:), allocatable :: dat_mat
  !
  integer, parameter :: cbt = kind(0)  ! cgns base integer type
  integer, parameter :: cst = cgsize_t ! cgns data integer type
  integer, parameter :: cet = cgenum_t ! cgns enumerator integer type
  integer, parameter :: crt = r8       ! cgns data real type
  integer(kind(realsingle)), parameter :: cgns_kind_real &
       = merge(realdouble, realsingle, crt == r8)

  integer(cbt) :: cgierr
  integer(cbt) :: ifile_n,ibase_n,izone_n,isol_n,ifield_n
  integer(cbt) :: ixyz_n(1:2)
  integer(cst), dimension(1:3) :: zone_size
  integer(cbt) :: cell_dim,phys_dim
  !
  real(crt), dimension(:), allocatable :: var_arr

continue
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read the tecplot file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  open(unit=tec_fn,file=tec_fname,status='old',action='read',iostat=ios)
  !
  ! 1st line
  read(tec_fn, '(A)', iostat=ios) line
  call toUpper(line)
  !
  idx = index(line,'VARIABLES')
  if ( idx == 0 ) then
    stop "ERROR: VARIABLES is not found in the 1st line."
  end if
  idx = index(line,'=')
  !
  ! Split the string to list
  ! Assume that the last var ends without the comma. So the while loop gets
  ! all vars except for the last one.
  call splitString(line(idx+1:),',',var_list)
  n_var = size(var_list)
  !
  ! Parse the header
  read(tec_fn, '(A)', iostat=ios) line
  call splitString(line,',',zone_term_list)
  !
  do i = 1,size(zone_term_list)
    !
    idx = index(zone_term_list(i),'=')
    str1 = zone_term_list(i)(1:idx-1)
    str2 = zone_term_list(i)(idx+1:)
    if ( trim(adjustl(str1)) == 'ZONETYPE' ) then
      zonetype = trim(adjustl(str2))
    else if ( trim(adjustl(str1)) == 'SOLUTIONTIME' ) then
      read(str2, *) sol_time
    else if ( trim(adjustl(str1)) == 'NODES' ) then
      read(str2, *) n_node
    else if ( trim(adjustl(str1)) == 'ELEMENTS' ) then
      read(str2, *) n_elem
    else if ( trim(adjustl(str1)) == 'DATAPACKING' ) then
      dat_pack = trim(adjustl(str2))
    end if
    !
  end do
  !
  ! datatype precision. Just skip this line.
  read(tec_fn, '(A)', iostat=ios) line
  !
  allocate(dat_mat(1:n_node,1:n_var),source=0.0_wp)
  !
  ! Read the data section. n_node lines.
  do i = 1, n_node
    !
    read(tec_fn, '(A)', iostat=ios) line
    call splitString(line,' ',dat_list)
    !
    do j = 1, n_var
      read(dat_list(j), *) dat_mat(i,j)
    end do
    !
  end do
  !
  close(unit=tec_fn)
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Save the data to CGNS file.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !
  call cg_open_f('sol_tec.cgns',cg_mode_write,ifile_n,cgierr)
  !
  cell_dim = 2
  phys_dim = 2
  call cg_base_write_f(ifile_n,'Base',cell_dim,phys_dim,ibase_n,cgierr)
  !
  zone_size(1) = int(n_node,kind=cst)
  zone_size(2) = int(n_elem,kind=cst)
  zone_size(3) = 0
  call cg_zone_write_f(ifile_n,ibase_n,'Solution',zone_size, &
                       unstructured,izone_n,cgierr)
  !
  allocate(var_arr(1:n_node))
  var_arr = real(dat_mat(:,1), kind=crt)
  call cg_coord_write_f(ifile_n,ibase_n,izone_n,cgns_kind_real, &
                        'CoordinateX',var_arr,ixyz_n(1),cgierr)
  !
  var_arr = real(dat_mat(:,2), kind=crt)
  call cg_coord_write_f(ifile_n,ibase_n,izone_n,cgns_kind_real, &
                        'CoordinateY',var_arr,ixyz_n(2),cgierr)
  !
  call cg_sol_write_f(ifile_n,ibase_n,izone_n,'FlowSolution',vertex,isol_n,cgierr)
  !
  var_arr = real(dat_mat(:,6), kind=crt)
  call cg_field_write_f(ifile_n,ibase_n,izone_n,isol_n,cgns_kind_real, &
                        'Density',var_arr,ifield_n,cgierr)
  !
  var_arr = real(dat_mat(:,7), kind=crt)
  call cg_field_write_f(ifile_n,ibase_n,izone_n,isol_n,cgns_kind_real, &
                        'VelocityX',var_arr,ifield_n,cgierr)
  !
  var_arr = real(dat_mat(:,8), kind=crt)
  call cg_field_write_f(ifile_n,ibase_n,izone_n,isol_n,cgns_kind_real, &
                        'VelocityY',var_arr,ifield_n,cgierr)
  !
  var_arr = real(dat_mat(:,9), kind=crt)
  call cg_field_write_f(ifile_n,ibase_n,izone_n,isol_n,cgns_kind_real, &
                        'Pressure',var_arr,ifield_n,cgierr)
  !
  !
  call cg_close_f(ifile_n,cgierr)
  !
  deallocate(var_arr)
  !
contains

subroutine toUpper(str)
  implicit none
  character(*), intent(inout) :: str
  integer :: i

continue

  do i = 1, len_trim(str)
    select case(str(i:i))
      case("a":"z")
        str(i:i) = achar(iachar(str(i:i))-32)
    end select
  end do

end subroutine toUpper

subroutine toLower(str)
  implicit none
  character(*), intent(inout) :: str
  integer :: i

continue

  do i = 1, len_trim(str)
    select case(str(i:i))
      case("A":"Z")
        str(i:i) = achar(iachar(str(i:i))+32)
    end select
  end do

end subroutine toLower

subroutine splitString(lstr,delimeter,sstr_list)
  implicit none
  character(*), intent(in) :: lstr
  character, intent(in) :: delimeter
  character(len=*), dimension(:), allocatable, intent(out) :: sstr_list
  integer :: idx,idx_last,istr,n_sstr
  !
continue
  !
  idx = 0
  idx_last = idx
  idx = index(lstr(idx_last+1:),delimeter) + idx_last
  istr = 0
  do while ( (idx-idx_last) /= 0 )
    !
    istr = istr+1
    !
    idx_last = idx
    idx = index(lstr(idx_last+1:),delimeter) + idx_last
    !
  end do
  n_sstr = istr + 1
  !
  if ( allocated(sstr_list) ) then
    deallocate(sstr_list)
  end if
  allocate(sstr_list(1:n_sstr))
  !
  idx = 0
  do istr = 1,n_sstr-1
    !
    idx_last = idx
    idx = index(lstr(idx_last+1:),delimeter) + idx_last
    sstr_list(istr) = lstr(idx_last+1:idx-1)
    !
  end do
  sstr_list(n_sstr) = lstr(idx+1:)
  !
end subroutine splitString

end program tec2cgns
