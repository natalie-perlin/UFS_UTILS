!> @file
!! @brief Define utility procedures
!! @author Denise.Worthen@noaa.gov
!!
!> This module defines a set of utility procedures
!! @author Denise.Worthen@noaa.gov
module utils_mod

  use netcdf
  use arrays_mod, only : eta
  use init_mod,   only : debug, logunit, vardefs, fsrc

  implicit none

  private

  interface getfield
     module procedure getfield2d
     module procedure getfield3d
  end interface getfield

  interface packarrays
     module procedure packarrays2d
     module procedure packarrays3d
  end interface packarrays

  interface remap
     module procedure remap1d
     module procedure remap2d
     module procedure remap3d
  end interface remap

  interface getvecpair
     module procedure getvecpair2d
     module procedure getvecpair3d
  end interface getvecpair

  interface dumpnc
     module procedure dumpnc1d
     module procedure dumpnc2d
     module procedure dumpnc3d
     module procedure dumpnc3dk
  end interface dumpnc

  public :: getfield
  public :: packarrays
  public :: remap
  public :: dumpnc
  public :: nf90_err
  public :: zero_out_land_ice
  public :: zero_out_phantom_ice

contains
  !> Pack 2D fields into arrays by mapping type
  !!
  !! @param[in]  filesrc  the file containing the fields to retrieve
  !! @param[in]  wgtsdir  the path to the required ESMF regridding weights
  !! @param[in]  cosrot   the cosine of the rotation angle
  !! @param[in]  sinrot   the sine of the rotation angle
  !! @param[in]  vars     the typedefs for this packed array
  !! @param[in]  dims     the dimensions of the fields in the packed array
  !! @param[in]  nflds    the number of fields in the packed array
  !! @param[out] fields   the packed array
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine packarrays2d(filesrc, wgtsdir, cosrot, sinrot, vars, dims, nflds, fields)

    character(len=*), intent(in)  :: filesrc,wgtsdir
    real(kind=8),     intent(in)  :: cosrot(:),sinrot(:)
    type(vardefs),    intent(in)  :: vars(:)
    integer,          intent(in)  :: dims(:)
    integer,          intent(in)  :: nflds
    real(kind=8),     intent(out) :: fields(:,:)

    ! local variables
    integer                   :: n, nn
    real(kind=8), allocatable :: vecpair(:,:)
    character(len=20)         :: subname = 'packarrays2d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    fields=0.0
    ! obtain vector pairs
    do n = 1,nflds
       if (trim(vars(n)%var_grid) == 'Cu' .or. trim(vars(n)%var_grid) == 'Bu_x') then
          allocate(vecpair(dims(1)*dims(2),2)); vecpair = 0.0
          call getvecpair(trim(filesrc), trim(wgtsdir), cosrot, sinrot,  &
               trim(vars(n)%var_name), trim(vars(n)%var_grid(1:2)),      &
               trim(vars(n)%var_pair), trim(vars(n)%var_pair_grid(1:2)), &
               dims=(/dims(1),dims(2)/), vecpair=vecpair)
       end if
    end do

    ! create packed array
    nn = 0
    do n = 1,nflds
       if (len_trim(vars(n)%var_pair) == 0) then
          nn = nn + 1
          call getfield(trim(filesrc), trim(vars(n)%var_name), dims=(/dims(1),dims(2)/), &
               field=fields(nn,:))
       else ! fill with vector pairs
          nn = nn+1
          ! ocn vectors
          if (trim(vars(n)%var_grid) == 'Cu')fields(nn,:) = vecpair(:,1)
          if (trim(vars(n)%var_grid) == 'Cv')fields(nn,:) = vecpair(:,2)
          ! ice vectors
          if (trim(vars(n)%var_grid) == 'Bu_x')fields(nn,:) = vecpair(:,1)
          if (trim(vars(n)%var_grid) == 'Bu_y')fields(nn,:) = vecpair(:,2)
       end if
    end do

    if (debug)write(logunit,'(a)')'exit '//trim(subname)
  end subroutine packarrays2d

  !> Pack 3D fields into arrays by mapping type
  !!
  !! @param[in]  filesrc  the file containing the fields to retrieve
  !! @param[in]  wgtsdir  the path to the required ESMF regridding weights
  !! @param[in]  cosrot   the cosine of the rotation angle
  !! @param[in]  sinrot   the sine of the rotation angle
  !! @param[in]  vars     the typedefs for this packed array
  !! @param[in]  dims     the dimensions of the fields in the packed array
  !! @param[in]  nflds    the number of fields in the packed array
  !! @param[out] fields   the packed array
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine packarrays3d(filesrc, wgtsdir, cosrot, sinrot, vars, dims, nflds, fields)

    character(len=*), intent(in)  :: filesrc,wgtsdir
    real(kind=8),     intent(in)  :: cosrot(:),sinrot(:)
    type(vardefs),    intent(in)  :: vars(:)
    integer,          intent(in)  :: dims(:)
    integer,          intent(in)  :: nflds
    real(kind=8),     intent(out) :: fields(:,:,:)

    ! local variables
    integer                   :: n, nn
    real(kind=8), allocatable :: vecpair(:,:,:)
    character(len=20)         :: subname = 'packarrays3d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    fields=0.0
    ! obtain vector pairs
    do n = 1,nflds
       if (trim(vars(n)%var_grid) == 'Cu') then
          allocate(vecpair(dims(3),dims(1)*dims(2),2)); vecpair = 0.0
          call getvecpair(trim(filesrc), trim(wgtsdir), cosrot, sinrot, &
               trim(vars(n)%var_name), trim(vars(n)%var_grid),          &
               trim(vars(n)%var_pair), trim(vars(n)%var_pair_grid),     &
               dims=(/dims(1),dims(2),dims(3)/), vecpair=vecpair)
       end if
    end do

    ! create packed array
    nn = 0
    do n = 1,nflds
       if (len_trim(vars(n)%var_pair) == 0) then
          nn = nn + 1
          if (trim(vars(n)%var_name) .eq. 'eta') then
             fields(nn,:,:) = eta(:,:)
          else
             call getfield(trim(filesrc), trim(vars(n)%var_name), dims=(/dims(1),dims(2),dims(3)/), &
                  field=fields(nn,:,:))
          end if
       else ! fill with vector pairs
          nn = nn+1
          if (trim(vars(n)%var_grid) == 'Cu')fields(nn,:,:) = vecpair(:,:,1)
          if (trim(vars(n)%var_grid) == 'Cv')fields(nn,:,:) = vecpair(:,:,2)
       end if
    end do

    if (debug)write(logunit,'(a)')'exit '//trim(subname)
  end subroutine packarrays3d

  !> Obtain 2D vector pairs mapped to Ct and rotated to EN
  !!
  !! @param[in]  fname    the file containing the fields to retrieve
  !! @param[in]  wdir     the path to the required ESMF regridding weights
  !! @param[in]  cosrot   the cosine of the rotation angle
  !! @param[in]  sinrot   the sine of the rotation angle
  !! @param[in]  vname1   the variable name of the first vector
  !! @param[in]  vgrid1   the grid location of the first vector
  !! @param[in]  vname2   the variable name of the second vector
  !! @param[in]  vgrid2   the grid location of the second vector
  !! @param[in]  dims     the dimensions of the fields
  !! @param[out] vecpair  the vector pair array
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine getvecpair2d(fname, wdir, cosrot, sinrot, vname1, vgrid1, vname2, vgrid2, dims, vecpair)

    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: wdir
    real(kind=8),     intent(in)  :: cosrot(:), sinrot(:)
    character(len=*), intent(in)  :: vname1, vgrid1, vname2, vgrid2
    integer,          intent(in)  :: dims(:)
    real(kind=8),     intent(out) :: vecpair(:,:)

    ! local variables
    integer :: ii
    real(kind=8), dimension(dims(1)*dims(2)) :: urot, vrot
    character(len=240) :: wgtsfile
    character(len=20) :: subname = 'getvecpair2d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,1), wgts=trim(wgtsfile))
    if (debug)write(logunit,'(a)')'wgtsfile for 2d vector '//trim(vname1)//'   '//trim(wgtsfile)
    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid2//'.to.Ct.bilinear.nc'
    call getfield(fname, vname2, dims=dims, field=vecpair(:,2), wgts=trim(wgtsfile))
    if (debug)write(logunit,'(a)')'wgtsfile for 2d vector '//trim(vname2)//'   '//trim(wgtsfile)

    urot = 0.0; vrot = 0.0
    do ii = 1,dims(1)*dims(2)
       urot(ii) = vecpair(ii,1)*cosrot(ii) + vecpair(ii,2)*sinrot(ii)
       vrot(ii) = vecpair(ii,2)*cosrot(ii) - vecpair(ii,1)*sinrot(ii)
    end do
    vecpair(:,1) = urot(:)
    vecpair(:,2) = vrot(:)

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine getvecpair2d

  !> Obtain 3D vector pairs mapped to Ct and rotated to EN
  !!
  !! @param[in]  fname    the file containing the fields to retrieve
  !! @param[in]  wdir     the path to the required ESMF regridding weights
  !! @param[in]  cosrot   the cosine of the rotation angle
  !! @param[in]  sinrot   the sine of the rotation angle
  !! @param[in]  vname1   the variable name of the first vector
  !! @param[in]  vgrid1   the grid location of the first vector
  !! @param[in]  vname2   the variable name of the second vector
  !! @param[in]  vgrid2   the grid location of the second vector
  !! @param[in]  dims     the dimensions of the fields
  !! @param[out] vecpair  the vector pair array
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine getvecpair3d(fname, wdir, cosrot, sinrot, vname1, vgrid1, vname2, vgrid2, dims, vecpair)

    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: wdir
    real(kind=8),     intent(in)  :: cosrot(:), sinrot(:)
    character(len=*), intent(in)  :: vname1, vgrid1, vname2, vgrid2
    integer,          intent(in)  :: dims(:)
    real(kind=8),     intent(out) :: vecpair(:,:,:)

    ! local variables
    integer :: ii,k
    real(kind=8), dimension(dims(1)*dims(2)) :: urot, vrot
    character(len=240) :: wgtsfile
    character(len=20)  :: subname = 'getvecpair3d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,:,1), wgts=trim(wgtsfile))
    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid2//'.to.Ct.bilinear.nc'
    call getfield(fname, vname2, dims=dims, field=vecpair(:,:,2), wgts=trim(wgtsfile))

    do k = 1,dims(3)
       urot = 0.0; vrot = 0.0
       do ii = 1,dims(1)*dims(2)
          urot(ii)= vecpair(k,ii,1)*cosrot(ii) + vecpair(k,ii,2)*sinrot(ii)
          vrot(ii)= vecpair(k,ii,2)*cosrot(ii) - vecpair(k,ii,1)*sinrot(ii)
       end do
       vecpair(k,:,1) = urot(:)
       vecpair(k,:,2) = vrot(:)
    end do

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine getvecpair3d

  !> Obtain a 2D field and return a 1-D vector array
  !!
  !! @param[in]  fname    the file containing the fields to retrieve
  !! @param[in]  vname    the variable to retrieve
  !! @param[in]  dims     the dimensions of the fields
  !! @param[out] field    the field
  !! @param[in]  wgts     the optional ESMF regridding weights
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine getfield2d(fname, vname, dims, field, wgts)

    character(len=*),           intent(in)  :: fname, vname
    integer,                    intent(in)  :: dims(:)
    real(kind=8),               intent(out) :: field(:)
    character(len=*), optional, intent(in)  :: wgts

    ! local variable
    integer                   :: ncid, varid
    real(kind=8), allocatable :: a2d(:,:)
    real(kind=8), allocatable :: atmp(:)
    character(len=20)         :: subname = 'getfield2d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname

    allocate(a2d(dims(1),dims(2))); a2d = 0.0
    allocate(atmp(dims(1)*dims(2))); atmp = 0.0

    call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
    call nf90_err(nf90_inq_varid(ncid, vname, varid), &
         'get variable ID: '//vname)
    call nf90_err(nf90_get_var(ncid, varid, a2d),     &
         'get variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    atmp(:) = reshape(a2d, (/dims(1)*dims(2)/))
    if(present(wgts)) then
       call remap(trim(wgts), src_field=atmp, dst_field=field)
    else
       field = atmp
    end if

    if (debug) write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine getfield2d

  !> Obtain a 3D field and return a 2-D vector array
  !!
  !! @param[in]  fname    the file containing the fields to retrieve
  !! @param[in]  vname    the variable to retrieve
  !! @param[in]  dims     the dimensions of the fields
  !! @param[out] field    the field
  !! @param[in]  wgts     the optional ESMF regridding weights
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine getfield3d(fname, vname, dims, field, wgts)

    character(len=*),           intent(in)  :: fname, vname
    integer,                    intent(in)  :: dims(:)
    real(kind=8),               intent(out) :: field(:,:)
    character(len=*), optional, intent(in)  :: wgts

    ! local variable
    integer                   :: k, ncid, varid
    real(kind=8), allocatable :: a3d(:,:,:)
    real(kind=8), allocatable :: atmp(:,:)
    character(len=20)         :: subname = 'getfield3d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname

    allocate(a3d(dims(1),dims(2),dims(3))); a3d = 0.0
    allocate(atmp(dims(3),dims(1)*dims(2))); atmp = 0.0

    call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
    call nf90_err(nf90_inq_varid(ncid, vname, varid), &
         'get variable ID: '//vname)
    call nf90_err(nf90_get_var(ncid, varid, a3d),     &
         'get variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    do k = 1,dims(3)
       atmp(k,:) = reshape(a3d(1:dims(1),1:dims(2),k), (/dims(1)*dims(2)/))
    end do
    if(present(wgts)) then
       call remap(trim(wgts), dim2=dims(3), src_field=atmp, dst_field=field)
    else
       field = atmp
    end if

    if (debug) write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine getfield3d

  !> Remap a 1-D vector array from source to destination
  !!
  !! @param[in]  fname       the file containing ESMF regridding weights
  !! @param[in]  src_field   the source field
  !! @param[out] dst_field   the destination field
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine remap1d(fname, src_field, dst_field)

    character(len=*), intent(in)  :: fname
    real(kind=8),     intent(in)  :: src_field(:)
    real(kind=8),     intent(out) :: dst_field(:)

    ! local variables
    integer :: ncid, id
    integer :: i,ii,jj
    integer :: n_a, n_b, n_s
    integer(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8),    allocatable, dimension(:) :: S
    character(len=20) :: subname = 'remap1d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    ! retrieve the weights
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'open: '//fname)
    call nf90_err(nf90_inq_dimid(ncid, 'n_s', id), 'get dimension Id: n_s')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_s), 'get dimension: n_s' )
    call nf90_err(nf90_inq_dimid(ncid, 'n_a', id), 'get dimension Id: n_a')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_a), 'get dimension: n_a' )
    call nf90_err(nf90_inq_dimid(ncid, 'n_b', id), 'get dimension Id: n_b')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_b), 'get dimension: n_b' )

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    call nf90_err(nf90_inq_varid(ncid, 'col', id),'get variable Id: col')
    call nf90_err(nf90_get_var(ncid,     id, col),'get variable: col')
    call nf90_err(nf90_inq_varid(ncid, 'row', id),'get variable Id: row')
    call nf90_err(nf90_get_var(ncid,     id, row),'get variable: row')
    call nf90_err(nf90_inq_varid(ncid,   'S', id),'get variable Id: S')
    call nf90_err(nf90_get_var(ncid,      id,  S),'get variable: S')
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    dst_field = 0.0
    do i = 1,n_s
       ii = row(i); jj = col(i)
       dst_field(ii) = dst_field(ii) + S(i)*src_field(jj)
    enddo

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine remap1d

  !> Remap a 2-D vector array of nflds or nlevs from source to destination
  !!
  !! @param[in]  fname       the file containing ESMF regridding weights
  !! @param[in]  dim2        the number of fields or levels
  !! @param[in]  src_field   the source field
  !! @param[out] dst_field   the destination field
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine remap2d(fname, dim2, src_field, dst_field)

    character(len=*), intent(in)  :: fname
    integer,          intent(in)  :: dim2
    real(kind=8),     intent(in)  :: src_field(:,:)
    real(kind=8),     intent(out) :: dst_field(:,:)

    ! local variables
    integer :: ncid, id
    integer :: i,ii,jj
    integer :: n_a, n_b, n_s
    integer(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8),    allocatable, dimension(:) :: S
    character(len=20) :: subname = 'remap2d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' weights = '//trim(fname)

    ! retrieve the weights
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'open: '//fname)
    call nf90_err(nf90_inq_dimid(ncid, 'n_s', id), 'get dimension Id: n_s')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_s), 'get dimension: n_s')
    call nf90_err(nf90_inq_dimid(ncid, 'n_a', id), 'get dimension Id: n_a')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_a), 'get dimension: n_a')
    call nf90_err(nf90_inq_dimid(ncid, 'n_b', id), 'get dimension Id: n_b')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_b), 'get dimension: n_b')

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    call nf90_err(nf90_inq_varid(ncid, 'col', id),'get variable Id: col')
    call nf90_err(nf90_get_var(ncid,     id, col),'get variable: col')
    call nf90_err(nf90_inq_varid(ncid, 'row', id),'get variable Id: row')
    call nf90_err(nf90_get_var(ncid,     id, row),'get variable: row')
    call nf90_err(nf90_inq_varid(ncid,   'S', id),'get variable Id: S')
    call nf90_err(nf90_get_var(ncid,      id,  S),'get variable: S')
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    dst_field = 0.0
    do i = 1,n_s
       ii = row(i); jj = col(i)
       dst_field(:,ii) = dst_field(:,ii) + S(i)*src_field(:,jj)
    enddo

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine remap2d

  !> Remap a field packed array of nk levels and nflds fields
  !!
  !! @param[in]  fname       the file containing ESMF regridding weights
  !! @param[in]  nk          the number of levels
  !! @param[in]  nflds       the number of fields
  !! @param[in]  src_field   the source field
  !! @param[out] dst_field   the destination field
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine remap3d(fname, nk, nflds, src_field, dst_field)

    character(len=*), intent(in)  :: fname
    integer,          intent(in)  :: nk, nflds
    real(kind=8),     intent(in)  :: src_field(:,:,:)
    real(kind=8),     intent(out) :: dst_field(:,:,:)

    ! local variables
    integer :: ncid, id
    integer :: i,ii,jj
    integer :: n_a, n_b, n_s
    integer(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8),    allocatable, dimension(:) :: S
    character(len=20) :: subname = 'remap3d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' weights = '//trim(fname)

    ! retrieve the weights
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'open: '//fname)
    call nf90_err(nf90_inq_dimid(ncid, 'n_s', id), 'get dimension Id: n_s')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_s), 'get dimension: n_s')
    call nf90_err(nf90_inq_dimid(ncid, 'n_a', id), 'get dimension Id: n_a')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_a), 'get dimension: n_a')
    call nf90_err(nf90_inq_dimid(ncid, 'n_b', id), 'get dimension Id: n_b')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_b), 'get dimension: n_b')

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    call nf90_err(nf90_inq_varid(ncid, 'col', id),'get variable Id: col')
    call nf90_err(nf90_get_var(ncid,     id, col),'get variable: col')
    call nf90_err(nf90_inq_varid(ncid, 'row', id),'get variable Id: row')
    call nf90_err(nf90_get_var(ncid,     id, row),'get variable: row')
    call nf90_err(nf90_inq_varid(ncid,   'S', id),'get variable Id: S')
    call nf90_err(nf90_get_var(ncid,      id,  S),'get variable: S')
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    dst_field = 0.0
    do i = 1,n_s
       ii = row(i); jj = col(i)
       dst_field(:,:,ii) = dst_field(:,:,ii) + S(i)*src_field(:,:,jj)
    enddo

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine remap3d

  !>  Write a bare netcdf file of a 2D packed field
  !!
  !! @param[in]  fname       the file containing ESMF regridding weights
  !! @param[in]  vname       the name of the variable
  !! @param[in]  dims        the variable spatial dimensions
  !! @param[in]  nflds       the number of fields
  !! @param[in]  field       the field to write
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine dumpnc2d(fname, vname, dims, nflds, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    integer,          intent(in) :: nflds
    real(kind=8),     intent(in) :: field(:,:)

    ! local variable
    integer                   :: n, ncid, varid, idimid, jdimid, fdimid
    real(kind=8), allocatable :: a3d(:,:,:)
    character(len=20)         :: subname = 'dumpnc2d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a3d(dims(1),dims(2),nflds)); a3d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_dim(ncid, 'nf', nflds,   fdimid), 'define dimension: nf')
    call nf90_err(nf90_def_var(ncid, vname, nf90_double, (/idimid,jdimid,fdimid/), varid), &
         'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    do n = 1,nflds
       a3d(:,:,n) = reshape(field(n,1:dims(1)*dims(2)), (/dims(1),dims(2)/))
    end do
    call nf90_err(nf90_put_var(ncid, varid, a3d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine dumpnc2d

  !> Write a bare netcdf file of a packed 3D field
  !!
  !! @param[in]  fname       the file containing ESMF regridding weights
  !! @param[in]  vname       the name of the variable
  !! @param[in]  dims        the variable spatial dimensions
  !! @param[in]  nk          the number of levels
  !! @param[in]  nflds       the number of fields
  !! @param[in]  field       the field to write
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine dumpnc3d(fname, vname, dims, nk, nflds, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    integer,          intent(in) :: nk, nflds
    real(kind=8),     intent(in) :: field(:,:,:)

    ! local variable
    integer :: n, k, ncid, varid, idimid, jdimid, kdimid, fdimid
    real(kind=8), allocatable :: a4d(:,:,:,:)
    character(len=20) :: subname = 'dumpnc3d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a4d(dims(1),dims(2),dims(3),nflds)); a4d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_dim(ncid, 'nk', dims(3), kdimid), 'define dimension: nk')
    call nf90_err(nf90_def_dim(ncid, 'nf', nflds,   fdimid), 'define dimension: nf')
    call nf90_err(nf90_def_var(ncid, vname, nf90_double, (/idimid,jdimid,kdimid,fdimid/), varid), &
         'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    do n = 1,nflds
       do k = 1,nk
          a4d(:,:,k,n) = reshape(field(n,k,1:dims(1)*dims(2)), (/dims(1),dims(2)/))
       end do
    end do
    call nf90_err(nf90_put_var(ncid, varid, a4d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine dumpnc3d

  !> Write a bare netcdf file of an unpacked 3D field
  !!
  !! @param[in]  fname       the file containing ESMF regridding weights
  !! @param[in]  vname       the name of the variable
  !! @param[in]  dims        the variable spatial dimensions
  !! @param[in]  field       the field to write
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine dumpnc3dk(fname, vname, dims, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    real(kind=8),     intent(in) :: field(:,:)

    ! local variable
    integer                   :: k, ncid, varid, idimid, jdimid, kdimid
    real(kind=8), allocatable :: a3d(:,:,:)
    character(len=20)         :: subname = 'dumpnc3dk'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a3d(dims(1),dims(2),dims(3))); a3d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'nf90_create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_dim(ncid, 'nk', dims(3), kdimid), 'define dimension: nk')
    call nf90_err(nf90_def_var(ncid, vname, nf90_double, (/idimid,jdimid,kdimid/), varid), &
         'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    do k = 1,dims(3)
       a3d(:,:,k) =  reshape(field(k,1:dims(1)*dims(2)), (/dims(1),dims(2)/))
    end do
    call nf90_err(nf90_put_var(ncid, varid, a3d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname

  end subroutine dumpnc3dk

  !> Write a bare netcdf file of a unpacked 2D field
  !!
  !! @param[in]  fname       the file containing ESMF regridding weights
  !! @param[in]  vname       the name of the variable
  !! @param[in]  dims        the variable spatial dimensions
  !! @param[in]  field       the field to write
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine dumpnc1d(fname, vname, dims, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    real(kind=8),     intent(in) :: field(:)

    ! local variable
    integer                   :: ncid, varid, idimid, jdimid
    real(kind=8), allocatable :: a2d(:,:)
    character(len=20)         :: subname = 'dumpnc1d'
    !----------------------------------------------------------------------------

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a2d(dims(1),dims(2))); a2d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'nf90_create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_var(ncid, vname, nf90_double, (/idimid,jdimid/), varid), &
         'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    a2d(:,:) =  reshape(field(1:dims(1)*dims(2)), (/dims(1),dims(2)/))
    call nf90_err(nf90_put_var(ncid, varid, a2d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname

  end subroutine dumpnc1d

  !> Reset field values to zero on land
  !!
  !! @param[in]     mask   the land mask
  !! @param[inout]  fout   the field value
  !! @param[out]    icnt   the number spatial points reset
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine zero_out_land_ice(mask, fout, icnt)

    integer(kind=4), intent(in)    :: mask(:)
    real(kind=8),    intent(inout) :: fout(:,:)
    integer,         intent(out)   :: icnt

    !local variables
    integer :: ij
    !----------------------------------------------------------------------------

    icnt = 0
    do ij = 1,size(fout,2)
       if ( mask(ij) .eq. 0 .and. sum(fout(:,ij)) .ne. 0.0)  then
          icnt = icnt + 1
          fout(:,ij) = 0.0
       end if
    end do
  end subroutine zero_out_land_ice

  !> Ensure that when fin contains zeros for all ncat, fout will also contain zeros
  !! for all ncat
  !!
  !! @param[in]     fin    the field to test against
  !! @param[inout]  fout   the field tested
  !! @param[out]    icnt   the number spatial points reset
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine zero_out_phantom_ice(fin, fout, icnt)

    real(kind=8), intent(in)    :: fin(:,:)
    real(kind=8), intent(inout) :: fout(:,:)
    integer,      intent(out)   :: icnt

    !local variables
    integer :: ij
    !----------------------------------------------------------------------------

    icnt = 0
    do ij = 1,size(fout,2)
       if (sum(fin(:,ij)) .eq. 0.0 .and. sum(fout(:,ij)) .ne. 0.0 ) then
          icnt = icnt + 1
          fout(:,ij) = 0.0
       end if
    end do
  end subroutine zero_out_phantom_ice

  !> Handle netcdf errors
  !!
  !! @param[in]  ierr        the error code
  !! @param[in]  string      the error message
  !!
  !! @author Denise.Worthen@noaa.gov
  subroutine nf90_err(ierr, string)

    integer ,         intent(in) :: ierr
    character(len=*), intent(in) :: string
    !----------------------------------------------------------------------------

    if (ierr /= nf90_noerr) then
       write(0, '(a)') 'FATAL ERROR: ' // trim(string)// ' : ' // trim(nf90_strerror(ierr))
       ! This fails on WCOSS2 with Intel 19 compiler. See
       ! https://community.intel.com/t5/Intel-Fortran-Compiler/STOP-and-ERROR-STOP-with-variable-stop-codes/m-p/1182521#M149254
       ! When WCOSS2 moves to Intel 2020+, uncomment the next line and remove stop 99
       !stop ierr
       stop 99
    end if
  end subroutine nf90_err

end module utils_mod
