!> @file
!! @brief Routines for applyng land DA increments
!! @author Clara Draper ESRL/PSL

module land_increments

    private

    public gaussian_to_fv3_interp
    public add_increment_soil
    public add_increment_snow
    public calculate_landinc_mask
    public apply_land_da_adjustments_soil
    public apply_land_da_adjustments_snd
    public lsm_noah, lsm_noahmp

    integer, parameter            :: lsm_noah=1      !< flag for NOAH land surface model
    integer, parameter            :: lsm_noahmp=2      !< flag for NOAHMP land surface model
                                                     !! copied from GFS_typedefs.F90

    ! control state for soil analysis:

    real, parameter          :: tfreez=273.16 !< con_t0c  in physcons
contains

 !> Read in gsi file with soil state  increments (on the gaussian
 !! grid), interpolate increments to the cubed-sphere tile
 !! Adapted from adjust_nsst.
 !!
 !! @param[inout] RLA Latitude on the cubed-sphere tile
 !! @param[inout] RLO Longitude on the cubed-sphere tile
 !! @param[in] SOILSNOW_TILE Land mask for increments on the cubed-sphere tile
 !! @param[in] LENSFC Number of land points on a tile
 !! @param[in] LSOIL Number of soil layers
 !! @param[in] LSOIL_INCR Number of soil layers (from top) to apply soil increments to
 !! @param[in] IDIM 'I' dimension of a tile
 !! @param[in] JDIM 'J' dimension of a tile
 !! @param[in] lsm Integer flag indicating which land model is used (1-Noah, 2-Noah-MP)
 !! @param[in] MYRANK MPI rank number
 !! @param[out] stcinc Soil temperature increments on the cubed-sphere tile
 !! @param[out] slcinc Liquid soil moisture increments on the cubed-sphere tile
 !!
 !! @author Clara Draper. @date March 2021
 !! @author Yuan Xue. @date Mar 2024

subroutine gaussian_to_fv3_interp(lsoil_incr,rla,rlo, &
                        stcinc,slcinc,soilsnow_tile,lensfc,lsoil,idim,jdim,lsm, myrank)

    use utils
    use gdswzd_mod
    use read_write_data, only : idim_gaus, jdim_gaus, &
                             stc_inc_gaus, soilsnow_gaus, slc_inc_gaus
    use mpi

    implicit none

    integer, intent(in)      :: lsoil_incr, lensfc, lsoil, idim, jdim, myrank, lsm

    integer, intent(in)      :: soilsnow_tile(lensfc)
    real, intent(inout)      :: rla(lensfc), rlo(lensfc)

    real, intent(out)        :: stcinc(lensfc,lsoil)
    real, intent(out)        :: slcinc(lensfc,lsoil)

    integer                  :: iopt, nret, kgds_gaus(200)
    integer                  :: igaus, jgaus, ij
    integer                  :: mask_tile
    integer                  :: itile, jtile
    integer                  :: j, ierr
    integer                  :: igausp1, jgausp1
    logical                  :: upd_slc, upd_stc
    real                     :: fill

    integer, allocatable     :: id1(:,:), id2(:,:), jdc(:,:)

    real                     :: wsum
    real                     :: stc_inc(lsoil)
    real                     :: slc_inc(lsoil)
    real, allocatable        :: xpts(:), ypts(:), lats(:), lons(:)
    real, allocatable        :: dum2d(:,:), lats_rad(:), lons_rad(:)
    real, allocatable        :: agrid(:,:,:), s2c(:,:,:)

    integer                  :: k
    logical                  :: gaus_has_soil 

    ! this produces the same lat/lon as can be read in from the file

    kgds_gaus     = 0
    kgds_gaus(1)  = 4          ! oct 6 - type of grid (gaussian)
    kgds_gaus(2)  = idim_gaus  ! oct 7-8 - # pts on latitude circle
    kgds_gaus(3)  = jdim_gaus
    kgds_gaus(4)  = 90000      ! oct 11-13 - lat of origin
    kgds_gaus(5)  = 0          ! oct 14-16 - lon of origin
    kgds_gaus(6)  = 128        ! oct 17 - resolution flag
    kgds_gaus(7)  = -90000     ! oct 18-20 - lat of extreme point
    kgds_gaus(8)  = nint(-360000./float(idim_gaus))  ! oct 21-23 - lon of extreme point
    kgds_gaus(9)  = nint((360.0 / float(idim_gaus))*1000.0)
                            ! oct 24-25 - longitude direction incr.
    kgds_gaus(10) = jdim_gaus/2     ! oct 26-27 - number of circles pole to equator
    kgds_gaus(12) = 255        ! oct 29 - reserved
    kgds_gaus(20) = 255        ! oct 5  - not used, set to 255


    if (lsm==lsm_noah) then 
        upd_stc=.true.
        upd_slc=.false. ! not coded
    elseif (lsm==lsm_noahmp) then 
        upd_stc=.true.
        upd_slc=.true.
    endif

    print*,' Start interpolating first ', lsoil_incr, ' surface layers only'
    
    !----------------------------------------------------------------------
    ! call gdswzd to compute the lat/lon of each gsi gaussian grid point.
    !----------------------------------------------------------------------

    iopt = 0
    fill = -9999.
    allocate(xpts(idim_gaus*jdim_gaus))
    allocate(ypts(idim_gaus*jdim_gaus))
    allocate(lats(idim_gaus*jdim_gaus))
    allocate(lons(idim_gaus*jdim_gaus))
    xpts = fill
    ypts = fill
    lats = fill
    lons = fill

    call gdswzd(kgds_gaus,iopt,(idim_gaus*jdim_gaus),fill,xpts,ypts,lons,lats,nret)

    if (nret /= (idim_gaus*jdim_gaus)) then
    print*,'fatal error: problem in gdswzd. stop.'
    call mpi_abort(mpi_comm_world, 12, ierr)
    endif

    deallocate (xpts, ypts)

    allocate(dum2d(idim_gaus,jdim_gaus))
    dum2d = reshape(lats, (/idim_gaus,jdim_gaus/) )
    deallocate(lats)

    allocate(lats_rad(jdim_gaus))

    do j = 1, jdim_gaus
    lats_rad(j) = dum2d(1,jdim_gaus-j+1) * 3.1415926 / 180.0
    enddo

    dum2d = reshape(lons, (/idim_gaus,jdim_gaus/) )
    deallocate(lons)
    allocate(lons_rad(idim_gaus))
    lons_rad = dum2d(:,1) * 3.1415926 / 180.0

    deallocate(dum2d)

    allocate(agrid(idim,jdim,2))
    agrid(:,:,1) = reshape (rlo, (/idim,jdim/) )
    agrid(:,:,2) = reshape (rla, (/idim,jdim/) )
    agrid        = agrid * 3.1415926 / 180.0

    allocate(id1(idim,jdim))
    allocate(id2(idim,jdim))
    allocate(jdc(idim,jdim))
    allocate(s2c(idim,jdim,4))

    !----------------------------------------------------------------------
    ! compute bilinear weights for each model point from the nearest
    ! four gsi/gaussian points.  does not account for mask.  that
    ! happens later.
    !----------------------------------------------------------------------

    call remap_coef( 1, idim, 1, jdim, idim_gaus, jdim_gaus, &
                  lons_rad, lats_rad, id1, id2, jdc, s2c, agrid )

    deallocate(lons_rad, lats_rad, agrid)

    ! initialize matrix
    stcinc = 0.0
    slcinc = 0.0

    ij_loop : do ij = 1, lensfc

        mask_tile    = soilsnow_tile(ij)

        !  get i,j index on array of (idim,jdim) from known ij

        jtile = (ij-1) / idim + 1
        itile = mod(ij,idim)
        if (itile==0) itile = idim

        !----------------------------------------------------------------------
        !  do update to soil temperature grid cells, using bilinear interp
        !----------------------------------------------------------------------

        if (mask_tile == 1) then
           ! these are the four nearest grid cells on the gaus grid
           igaus   = id1(itile,jtile)
           jgaus   = jdc(itile,jtile)
           igausp1 = id2(itile,jtile)
           jgausp1 = jdc(itile,jtile)+1

        ! make sure gaus grid has soil nearby
           gaus_has_soil = .false.
           if (soilsnow_gaus(igaus,jgaus)     == 1 .or. &
               soilsnow_gaus(igausp1,jgaus)   == 1 .or. &
               soilsnow_gaus(igausp1,jgausp1) == 1 .or. &
               soilsnow_gaus(igaus,jgausp1)   == 1)  gaus_has_soil = .true.
           
           if (.not. gaus_has_soil) then
             cycle ij_loop
           endif

           stc_inc = 0.0
           slc_inc = 0.0
           wsum  = 0.0

           if (soilsnow_gaus(igaus,jgaus) == 1) then
             do k = 1, lsoil_incr
                 if (upd_stc) &
                   stc_inc(k)  = stc_inc(k) + (s2c(itile,jtile,1) * stc_inc_gaus(k,igaus,jgaus))
                 if (upd_slc) &
                   slc_inc(k)  = slc_inc(k) + (s2c(itile,jtile,1) * slc_inc_gaus(k,igaus,jgaus))
             enddo
             wsum  = wsum + s2c(itile,jtile,1)
           endif

           if (soilsnow_gaus(igausp1,jgaus) == 1) then
             do k = 1, lsoil_incr
                 if (upd_stc) &
                   stc_inc(k) = stc_inc(k) + (s2c(itile,jtile,2) * stc_inc_gaus(k,igausp1,jgaus))
                 if (upd_slc) &
                   slc_inc(k) = slc_inc(k) + (s2c(itile,jtile,2) * slc_inc_gaus(k,igausp1,jgaus))
             enddo
             wsum  = wsum + s2c(itile,jtile,2)
           endif

           if (soilsnow_gaus(igausp1,jgausp1) == 1) then
             do k = 1, lsoil_incr
                 if (upd_stc) &
                   stc_inc(k) = stc_inc(k) + (s2c(itile,jtile,3) * stc_inc_gaus(k,igausp1,jgausp1))
                 if (upd_slc) &
                   slc_inc(k) = slc_inc(k) + (s2c(itile,jtile,3) * slc_inc_gaus(k,igausp1,jgausp1))
             enddo
             wsum  = wsum + s2c(itile,jtile,3)
           endif

           if (soilsnow_gaus(igaus,jgausp1) == 1) then
             do k = 1, lsoil_incr
                 if (upd_stc) &
                   stc_inc(k) = stc_inc(k) + (s2c(itile,jtile,4) * stc_inc_gaus(k,igaus,jgausp1))
                 if (upd_slc) &
                   slc_inc(k) = slc_inc(k) + (s2c(itile,jtile,4) * slc_inc_gaus(k,igaus,jgausp1))
             enddo
             wsum  = wsum + s2c(itile,jtile,4)
           endif

           ! normalize increments
           do k = 1, lsoil_incr
             stc_inc(k) = stc_inc(k) / wsum
             stcinc(ij,k) = stc_inc(k)
             slc_inc(k) = slc_inc(k) / wsum
             slcinc(ij,k) = slc_inc(k)
           enddo

      endif ! if soil/snow point

    enddo ij_loop

    write(*,'(a,i2)') ' Finish soil increments interpolation for rank : ', myrank

    deallocate(id1, id2, jdc, s2c)

end subroutine gaussian_to_fv3_interp

 !> Read in soil state increments (on the cubed-sphere 
 !! grid),and add to the soil states. Adapted from original add_gsi_increment_soil routine.
 !!
 !! @param[in] SLCINC Liquid soil moisture increments on the cubed-sphere tile
 !! @param[in] STCINC Soil temperature increments on the cubed-sphere tile
 !! @param[inout] STC_STATE Soil temperature state vector
 !! @param[inout] SMC_STATE Soil moisture (liquid plus solid) state vector
 !! @param[inout] SLC_STATE Liquid soil moisture state vector
 !! @param[out] stc_updated Integer to record whether STC in each grid cell was updated 
 !! @param[out] slc_updated Integer to record whether SMC in each grid cell was updated 
 !! @param[in] SOILSNOW_TILE Land mask for increments on the cubed-sphere tile
 !! @param[in] SOILSNOW_FG_TILE First guess land mask for increments on the
 !!             cubed-sphere tile
 !! @param[in] LENSFC Number of land points on a tile
 !! @param[in] LSOIL Number of soil layers
 !! @param[in] LSOIL_INCR Number of soil layers (from top) to apply soil increments to
 !! @param[in] lsm Integer flag indicating which land model is used 
 !! @param[in] MYRANK MPI rank number
 !!
 !! @author Yuan Xue. 11/2023

subroutine add_increment_soil(lsoil_incr,stcinc,slcinc,stc_state,smc_state,slc_state,stc_updated,&
              slc_updated,soilsnow_tile,soilsnow_fg_tile,lensfc,lsoil,lsm,myrank)

    use mpi

    implicit none

    integer, intent(in)      :: lsoil_incr, lensfc, lsoil, myrank, lsm

    integer, intent(in)      :: soilsnow_tile(lensfc), soilsnow_fg_tile(lensfc)
    real, intent(inout)      :: stc_state(lensfc, lsoil)
    real, intent(inout)      :: slc_state(lensfc, lsoil)
    real, intent(inout)      :: smc_state(lensfc, lsoil)
    integer, intent(out)     :: stc_updated(lensfc), slc_updated(lensfc)

    
    integer                  :: ij
    integer                  :: mask_tile, mask_fg_tile
    logical                  :: upd_slc, upd_stc

    real                     :: stcinc(lensfc,lsoil)
    real                     :: slcinc(lensfc,lsoil)

    integer                  :: k, nother, nsnowupd
    integer                  :: nstcupd, nslcupd,  nfrozen, nfrozen_upd
    logical                  :: soil_freeze, soil_ice

    stc_updated=0
    slc_updated=0

    if (lsm==lsm_noah) then
        upd_stc=.true.
        upd_slc=.false. ! not coded
    elseif (lsm==lsm_noahmp) then
        upd_stc=.true.
        upd_slc=.true.
    endif

    print*
    print*,'adjust soil using increments on cubed-sphere tiles'
    print*,'updating soil temps', upd_stc
    print*,'updating soil moisture', upd_slc
    print*,'adjusting first ', lsoil_incr, ' surface layers only'

    ! initialize variables for counts statitics to be zeros
    nother = 0 ! grid cells not land
    nsnowupd = 0  ! grid cells with snow (temperature not yet updated)
    nslcupd = 0 ! grid cells that are updated
    nstcupd = 0 ! grid cells that are updated
    nfrozen = 0 ! not update as frozen soil
    nfrozen_upd = 0 ! not update as frozen soil

    ij_loop : do ij = 1, lensfc

        mask_tile    = soilsnow_tile(ij)
        mask_fg_tile = soilsnow_fg_tile(ij)

        !----------------------------------------------------------------------
        ! mask: 1  - soil, 2 - snow, 0 - land-ice, -1 - not land
        !----------------------------------------------------------------------

        if (mask_tile <= 0) then ! skip if neither soil nor snow
         nother = nother + 1
         cycle ij_loop
        endif

        !----------------------------------------------------------------------
        ! if snow is present before or after snow update, skip soil analysis
        !----------------------------------------------------------------------

        if (mask_fg_tile == 2 .or. mask_tile == 2) then
         nsnowupd = nsnowupd + 1
         cycle ij_loop
        endif

        !----------------------------------------------------------------------
        !  do update to soil temperature grid cells
        !----------------------------------------------------------------------

        if (mask_tile == 1) then

           !----------------------------------------------------------------------
           !  add the interpolated increment to the background
           !----------------------------------------------------------------------

           soil_freeze=.false.
           soil_ice=.false.
           do k = 1, lsoil_incr

             if ( stc_state(ij,k) < tfreez)  soil_freeze=.true.
             if ( smc_state(ij,k) - slc_state(ij,k) > 0.001 )  soil_ice=.true.

             if (upd_stc) then
                stc_state(ij,k) = stc_state(ij,k) + stcinc(ij,k)
                if (k==1) then
                    stc_updated(ij) = 1
                    nstcupd = nstcupd + 1
                endif
             endif

             if ( (stc_state(ij,k) < tfreez) .and. (.not. soil_freeze) .and. (k==1) )&
                   nfrozen_upd = nfrozen_upd + 1

             ! do not do updates if this layer or any above is frozen
             if ( (.not. soil_freeze ) .and. (.not. soil_ice ) ) then
                if (upd_slc) then
                if (k==1) then
                    nslcupd = nslcupd + 1
                    slc_updated(ij) = 1
                endif
                   ! apply zero limit here (higher, model-specific limits are
                   ! later)
                   slc_state(ij,k) = max(slc_state(ij,k) + slcinc(ij,k), 0.0)
                   smc_state(ij,k) = max(smc_state(ij,k) + slcinc(ij,k), 0.0)
                endif
             else
                if (k==1) nfrozen = nfrozen+1
             endif

           enddo

        endif ! if soil/snow point

   enddo ij_loop

   write(*,'(a,i2)') ' statistics of grids number processed for rank : ', myrank
   write(*,'(a,i8)') ' soil grid total', lensfc
   write(*,'(a,i8)') ' soil grid cells slc updated = ',nslcupd
   write(*,'(a,i8)') ' soil grid cells stc updated = ',nstcupd
   write(*,'(a,i8)') ' soil grid cells not updated, frozen = ',nfrozen
   write(*,'(a,i8)') ' soil grid cells update, became frozen = ',nfrozen_upd
   write(*,'(a,i8)') ' (not updated yet) snow grid cells = ', nsnowupd
   write(*,'(a,i8)') ' grid cells, without soil or snow = ', nother

end subroutine add_increment_soil


 !> Add snow depth increment to model snow depth state,
 !! and limit output to be non-negative. JEDI increments are
 !! calculated globally, so must be screened to land-only locations 
 !! here.
 !!
 !! @param[in] lensfc Number of land points on this tile
 !! @param[in] snd_inc Soil depth increments
 !! @param[in] mask Land mask for increments
 !! @param[inout] snd Soil depth background (in), and analysis (out) 
 !! 
 !! @author Clara Draper. @date August 2021

subroutine add_increment_snow(snd_inc,mask,lensfc,snd)

    implicit none

    integer, intent(in)      :: lensfc
    real, intent(in)         :: snd_inc(lensfc)
    integer, intent(in)      :: mask(lensfc)
    real, intent(inout)      :: snd(lensfc)

    integer                  :: i


    do i =1, lensfc
        if (mask(i) > 0) then ! if land
                snd(i) = max( snd(i) + snd_inc(i), 0.)
        endif
    enddo

end subroutine add_increment_snow

!> Calculate soil mask for land on model grid.
!! Output is 1  - soil, 2 - snow-covered, 0 - land ice, -1  not land.
!!
!! @param[in] lensfc  Number of land points for this tile 
!! @param[in] veg_type_landice Value of vegetion class that indicates land-ice
!! @param[in] stype Soil type
!! @param[in] swe Model snow water equivalent
!! @param[in] vtype Model vegetation type
!! @param[out] mask Land mask for increments
!! @author Clara Draper @date March 2021
!! @author Yuan Xue: introduce stype to make the mask calculation more generic
subroutine calculate_landinc_mask(swe,vtype,stype,lensfc,veg_type_landice,mask)
 
    implicit none

    integer, intent(in)           :: lensfc, veg_type_landice
    real, intent(in)              :: swe(lensfc)
    real, intent(in)              :: vtype(lensfc),stype(lensfc)
    integer, intent(out)          :: mask(lensfc)

    integer :: i

    mask = -1 ! not land

    ! land (but not land-ice)
    do i=1,lensfc
        if (nint(stype(i)) .GT. 0) then
          if (swe(i) .GT. 0.001) then ! snow covered land
                mask(i) = 2
          else                        ! non-snow covered land
                mask(i) = 1
          endif
        end if ! else should work here too
        if ( nint(vtype(i)) ==  veg_type_landice  ) then ! land-ice
                mask(i) = 0
        endif
    end do

end subroutine calculate_landinc_mask

!> Make adjustments to dependent variables after applying land increments.
!! These adjustments are model-dependent, and are currently only coded
!! if full for Noah LSM. 
!! For Noah LSM, copy relevent code blocks from model code (same as has
!! been done in sfc_sub).
!! For Noah-MP, the adjustment scheme shown below as of 11/09/2023:
!! Case 1: frozen ==> frozen, recalculate slc following opt_frz=1, smc remains
!! Case 2: unfrozen ==> frozen, recalculate slc following opt_frz=1, smc remains
!! Case 3: frozen ==> unfrozen, melt all soil ice (if any)
!! Case 4: unfrozen ==> unfrozen along with other cases, (e.g., soil temp=tfrz),do nothing
!! Note: For Case 3, Yuan Xue thoroughly evaluated a total of four options and
!! current option is found to be the best as of 11/09/2023
!! @param[in] lsm Integer code for the LSM
!! @param[in] isot Integer code for the soil type data set
!! @param[in] ivegsrc Integer code for the vegetation type data set
!! @param[in] lensfc Number of land points for this tile
!! @param[in] lsoil Number of soil layers
!! @param[in] lsoil_incr Number of soil layers (from top) to apply soil increments to
!! @param[in] rsoiltype Array of input soil types
!! @param[in] mask Mask indicating surface type
!! @param[in] stc_bck Background soil temperature states
!! @param[in] stc_adj Analysis soil temperature states
!! @param[inout] smc_adj Analysis soil moisture states
!! @param[inout] slc_adj Analysis liquid soil moisture states
!! @param[in] stc_updated Integer to record whether STC in each grid cell was updated
!! @param[in] slc_updated Integer to record whether SLC in each grid cell was updated
!! @param[in] zsoil Depth of bottom of each soil layer
!! @author Clara Draper @date April 2021

subroutine apply_land_da_adjustments_soil(lsoil_incr, lsm, isot, ivegsrc,lensfc, &
                 lsoil, rsoiltype, mask, stc_bck, stc_adj, smc_adj, slc_adj, &
                 stc_updated, slc_updated, zsoil)

    use mpi
    use set_soilveg_snippet_mod, only: set_soilveg_noah,set_soilveg_noahmp
    use sflx_snippet,    only: frh2o

    implicit none
 
    integer, intent(in)           :: lsoil_incr, lsm, lensfc, lsoil, isot, ivegsrc
    real, intent(in)              :: rsoiltype(lensfc) ! soil types, as real
    integer, intent(in)           :: mask(lensfc)
    real, intent(in)              :: stc_bck(lensfc, lsoil)
    integer, intent(in)           :: stc_updated(lensfc), slc_updated(lensfc)
    real, intent(inout)           :: smc_adj(lensfc,lsoil), slc_adj(lensfc,lsoil) 
    real, intent(inout)           :: stc_adj(lensfc, lsoil)
    real(kind=4), intent(in)      :: zsoil(lsoil)
    

    logical                       :: frzn_bck, frzn_anl
    logical                       :: soil_freeze, soil_ice

    integer                       :: i, l, n_freeze, n_thaw, ierr
    integer                       :: myrank, soiltype, iret, n_stc, n_slc
    logical                       :: upd_slc, upd_stc

    real                          :: slc_new

    real, parameter               :: tfreez=273.16 !< con_t0c  in physcons
    real, dimension(30)           :: maxsmc, bb, satpsi
    real, dimension(4)            :: dz ! layer thickness

    real, parameter          :: hfus=0.3336e06 !< latent heat of fusion(j/kg)
    real, parameter          :: grav=9.80616   !< gravity accel.(m/s2)
    real                     :: smp !< for computing supercooled water 

    call mpi_comm_rank(mpi_comm_world, myrank, ierr)

    if (lsm==lsm_noah) then 
        upd_stc=.true.
        upd_slc=.false.
    elseif (lsm==lsm_noahmp) then 
        upd_stc=.true.
        upd_slc=.true.
    endif

    select case (lsm ) 
    case(lsm_noah)  
        ! initialise soil properties
        call set_soilveg_noah(isot, ivegsrc, maxsmc, bb, satpsi, iret)
        if (iret < 0) then
            print *, 'FATAL ERROR: problem in set_soilveg_noah'
            call mpi_abort(mpi_comm_world, 10, ierr)
        endif

        print *, 'Adjusting noah model smc after stc DA update'

        n_freeze = 0
        n_thaw = 0
        
        do i=1,lensfc
          if (mask(i) > 0) then ! if soil location
            do l = 1, lsoil
               frzn_bck = (stc_bck(i,l) .LT. tfreez )
               frzn_anl = (stc_adj(i,l) .LT. tfreez )

               if (frzn_bck .eqv. frzn_anl) then
                    cycle
               elseif (frzn_bck .and. .not. frzn_anl) then
                    n_thaw = n_thaw + 1
               else
                    n_freeze = n_freeze + 1
               endif

               ! make adjustment (same routine for both)
               soiltype = nint(rsoiltype(i))
               ! bb and maxsmc are in the namelist_soilveg, need soiltype index
               call frh2o(stc_adj(i,l), smc_adj(i,l),slc_adj(i,l), maxsmc(soiltype), &
                          bb(soiltype), satpsi(soiltype),slc_new)

               slc_adj(i,l) = max( min( slc_new, smc_adj(i,l)), 0.0 )
            enddo
          endif
        enddo
        print *, 'adjusted: ', n_thaw,' thawed,', n_freeze, ' frozen'

    case (lsm_noahmp) 

        if (upd_stc) then

          call set_soilveg_noahmp(isot, ivegsrc, maxsmc, bb, satpsi, iret)
          if (iret < 0) then
               print *, 'FATAL ERROR: problem in set_soilveg_noahmp'
               call mpi_abort(mpi_comm_world, 10, ierr)
          endif

          n_stc = 0
          n_slc = 0

          do i=1,lensfc
            if (stc_updated(i) == 1 ) then ! soil-only location
                n_stc = n_stc+1
                soiltype = nint(rsoiltype(i))
                do l = 1, lsoil_incr
                   !case 1: frz ==> frz, recalculate slc, smc remains
                   !case 2: unfrz ==> frz, recalculate slc, smc remains
                   !both cases are considered in the following if case
                   if (stc_adj(i,l) .LT. tfreez )then
                      !recompute supercool liquid water,smc_anl remain unchanged
                      smp = hfus*(tfreez-stc_adj(i,l))/(grav*stc_adj(i,l)) !(m)
                      slc_new=maxsmc(soiltype)*(smp/satpsi(soiltype))**(-1./bb(soiltype))
                      slc_adj(i,l) = max( min( slc_new, smc_adj(i,l)), 0.0 )
                   endif
                   !case 3: frz ==> unfrz, melt all soil ice (if any)
                   if (stc_adj(i,l) .GT. tfreez )then !do not rely on stc_bck
                      slc_adj(i,l)=smc_adj(i,l)
                   endif
                enddo
            endif
          enddo

        endif  

        if (upd_slc) then

          dz(1) = -zsoil(1)
          do l = 2,lsoil 
              dz(l) = -zsoil(l) + zsoil(l-1) 
          enddo 
          print *, 'Applying soil moisture mins ' 

          do i=1,lensfc
          if (slc_updated(i) == 1 ) then 
              n_slc = n_slc+1
              ! apply SM bounds (later: add upper SMC limit)
              do l = 1, lsoil_incr
                ! noah-mp minimum is 1 mm per layer (in SMC)
                ! no need to maintain frozen amount, would be v. small.
                slc_adj(i,l) = max( 0.001/dz(l), slc_adj(i,l) )
                smc_adj(i,l) = max( 0.001/dz(l), smc_adj(i,l) )
              enddo
           endif
          enddo
        endif

    case default 
        print *, 'FATAL ERROR: unrecognised LSM,', lsm
        call mpi_abort(mpi_comm_world, 10, ierr)
    end select

    write(*,'(a,i2)') 'statistics of grids number processed for rank : ', myrank 
    write(*,'(a,i8)') ' soil grid total', lensfc
    write(*,'(a,i8)') ' soil grid cells with slc update', n_slc
    write(*,'(a,i8)') ' soil grid cells with stc update', n_stc

end subroutine apply_land_da_adjustments_soil

!> Make adjustments to dependent variables after applying land increments.
!! These adjustments are model-dependent, and are currently only coded
!! for Noah LSM.
!! Here: adjust SWE to be consistent with updated SND, using snow density 
!! from the forecast.

!> @param[in] lsm Integer code for the LSM
!! @param[in] lensfc  Number of land points for this tile
!! @param[in] mask Land mask for increments
!! @param[in] swe_bck Background SWE 
!! @param[in] snd_bck Background snow depth
!! @param[in] snd_anl Analysis snow depth 
!! @param[inout] swe_adj SWE to be adjusted 
!! @author Clara Draper @date August 2021

subroutine apply_land_da_adjustments_snd(lsm, lensfc, mask, swe_bck, snd_bck, snd_anl, swe_adj)

    use mpi
    use bulk_snow_module, only: calc_density

    implicit none

    integer, intent(in) :: lsm, lensfc
    integer, intent(in) :: mask(lensfc)
    real, intent(in)    :: swe_bck(lensfc), snd_bck(lensfc)
    real, intent(in)    :: snd_anl(lensfc)
    real, intent(inout)    :: swe_adj(lensfc)

    integer  :: ierr, myrank, i

    real     :: density(lensfc)

    call mpi_comm_rank(mpi_comm_world, myrank, ierr)

    if (lsm .NE. lsm_noah) then
        print *, 'FATAL ERROR: apply_land_da_adjustments not coded for models other than noah', lsm
        call mpi_abort(mpi_comm_world, 10, ierr)
    endif

   ! calculate snow density from forecasts
   call calc_density(lensfc, mask, swe_bck, snd_bck, myrank, density)

   do i =1, lensfc
        if ( mask(i)>0 ) then
                swe_adj(i) = snd_anl(i)*density(i)
        endif
   enddo
   

end subroutine apply_land_da_adjustments_snd

end module land_increments
