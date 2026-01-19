MODULE pyfld
   !!======================================================================
   !!                       ***  MODULE pyfld  ***
   !! Python module :   variables defined in core memory
   !!======================================================================
   !! History :  4.2  ! 2025-11  (A. Barge)  Original code
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   pyfld_alloc : allocation of fields arrays for Python coupling module (pycpl)
   !!----------------------------------------------------------------------
   !!=====================================================
   USE oce            ! ocean fields
   USE dom_oce        ! ocean metrics fields
   USE par_oce        ! ocean parameters
   USE lib_mpp        ! MPP library
   USE pycpl          ! Python coupling module
   USE iom

   IMPLICIT NONE
   PUBLIC

   !!----------------------------------------------------------------------
   !!                    2D Python coupling Module fields
   !!----------------------------------------------------------------------
   !REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)  :: ext_psiu_mle, ext_psiv_mle    !: dummy field to store 2D fields

   !!----------------------------------------------------------------------
   !!                    3D Python coupling Module fields
   !!----------------------------------------------------------------------
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: ext_tf, ext_sf  !: dummy field to store 3D fields

CONTAINS

   SUBROUTINE init_python_fields()
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE init_python_fields  ***
      !!
      !! ** Purpose :   Initialisation of the Python module
      !!
      !! ** Method  :   * Allocate arrays for Python fields
      !!                * Configure Python coupling
      !!----------------------------------------------------------------------
      !
      ! Allocate fields
      ALLOCATE( ext_tf(jpi,jpj,jpk) , ext_sf(jpi,jpj,jpk) )
      !
      ! configure coupling
      CALL init_python_coupling()
      !
   END SUBROUTINE init_python_fields


   SUBROUTINE finalize_python_fields()
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE finalize_python_fields  ***
      !!
      !! ** Purpose :   Free memory used by Python module
      !!
      !! ** Method  :   * deallocate arrays for Python fields
      !!                * deallocate Python coupling
      !!----------------------------------------------------------------------
      !
      ! Free memory
      DEALLOCATE( ext_tf, ext_sf )
      !
      ! terminate coupling environment
      CALL finalize_python_coupling()
      !
   END SUBROUTINE finalize_python_fields


   SUBROUTINE inputs_z25( kt )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE inputs_MLE.C20  ***
      !!
      !! ** Purpose :   send inputs fileds for gz21 model
      !!
      !! ** Method  :   *
      !!                *
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      !REAL(wp), DIMENSION(jpi,jpj) :: Hu, Hv, Db_u, Db_v
      !!----------------------------------------------------------------------
      !
      ! send velocities and masks
      CALL send_to_python( 'Hu', zhu, kt )    ! Send fields to Python models
      CALL send_to_python( 'Hv', zhv, kt )    ! Send fields to Python models
      CALL send_to_python( 'Db_u', dbu, kt )    ! Send fields to Python models
      CALL send_to_python( 'Db_v', dbv, kt )    ! Send fields to Python models
      CALL send_to_python( 'Dt_u', dtu, kt )    ! Send fields to Python models
      CALL send_to_python( 'Dt_v', dtv, kt )    ! Send fields to Python models
      CALL send_to_python( 'Ds_u', dsu, kt )    ! Send fields to Python models
      CALL send_to_python( 'Ds_v', dsv, kt )    ! Send fields to Python models
      !
   END SUBROUTINE inputs_mle_z25

   SUBROUTINE update_from_mle_z25( kt )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE update_from_MLE.C20  ***
      !!
      !! ** Purpose :   update the ocean data with the coupled GZ21 models
      !!
      !! ** Method  :   *
      !!                *
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      INTEGER, INTENT(in) ::   Nrhs          ! time index
      !!----------------------------------------------------------------------
      !
      ! Proceed receptions
      CALL receive_from_python( 't_f', ext_tf, kt )
      CALL receive_from_python( 's_f', ext_sf, kt )
      !
      ! update ocean
      pts(:,:,:,jp_tem,Krhs) = pts(:,:,:,jp_tem,Krhs) + ext_tf(:,:,:)
      pts(:,:,:,jp_sal,Krhs) = pts(:,:,:,jp_sal,Krhs) + ext_sf(:,:,:)
      !
      ! Outputs results
      CALL iom_put( 'ext_tf', ext_tf )
      CALL iom_put( 'ext_sf', ext_sf )
      !
   END SUBROUTINE update_from_mle_z25

END MODULE pyfld
