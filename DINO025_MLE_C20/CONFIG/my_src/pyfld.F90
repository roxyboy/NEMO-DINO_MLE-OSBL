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
   REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:)  :: ext_psiu, ext_psiv    !: dummy field to store 2D fields

   !!----------------------------------------------------------------------
   !!                    3D Python coupling Module fields
   !!----------------------------------------------------------------------
   !REAL(wp), PUBLIC, ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: ext_uf, ext_vf  !: dummy field to store 3D fields

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
      ALLOCATE( ext_psiu(jpi,jpj) , ext_psiv(jpi,jpj) )
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
      DEALLOCATE( ext_psiu, ext_psiv )
      !
      ! terminate coupling environment
      CALL finalize_python_coupling()
      !
   END SUBROUTINE finalize_python_fields


   SUBROUTINE inputs_mle_c20( kt, Hu, Hv, Db_u, Db_v )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE inputs_MLE.C20  ***
      !!
      !! ** Purpose :   send inputs fileds for gz21 model
      !!
      !! ** Method  :   *
      !!                *
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt            ! ocean time step
      ! INTEGER, INTENT(in) ::   Nbb           ! time index
      REAL(wp), DIMENSION(jpi,jpj) :: Hu, Hv, Db_u, Db_v
      REAL(wp), DIMENSION(jpi,jpj) :: e1u, e2v
      !!----------------------------------------------------------------------
      !
      ! send velocities and masks
      CALL send_to_python( 'Hu', Hu, kt )    ! Send fields to Python models
      CALL send_to_python( 'Hv', Hv, kt )    ! Send fields to Python models
      CALL send_to_python( 'Db_u', Db_u, kt )    ! Send fields to Python models
      CALL send_to_python( 'Db_v', Db_v, kt )    ! Send fields to Python models
      CALL send_to_python( 'e1u', e1u, kt )    ! Send fields to Python models
      CALL send_to_python( 'e2v', e2v, kt )    ! Send fields to Python models
      !
      CALL receive_from_python( 'psi_u', ext_psiu, kt )
      CALL receive_from_python( 'psi_v', ext_psiv, kt )
      !
      CALL iom_put( 'ext_psiu_mle', ext_psiu )
      CALL iom_put( 'ext_psiv_mle', ext_psiv )
      !
   END SUBROUTINE inputs_mle_c20

   ! SUBROUTINE update_from_mle_c20( kt )
      !!----------------------------------------------------------------------
      !!             ***  ROUTINE update_from_MLE.C20  ***
      !!
      !! ** Purpose :   update the ocean data with the coupled GZ21 models
      !!
      !! ** Method  :   *
      !!                *
      !!----------------------------------------------------------------------
      ! INTEGER, INTENT(in) ::   kt            ! ocean time step
      ! INTEGER, INTENT(in) ::   Nrhs          ! time index
      !!----------------------------------------------------------------------
      !
      ! Proceed receptions
      ! CALL receive_from_python( 'psi_u', ext_psiu, kt )
      ! CALL receive_from_python( 'psi_v', ext_psiv, kt )
      !
      ! update ocean
      ! uu(:,:,:,Nrhs) = uu(:,:,:,Nrhs) + ext_uf(:,:,:)
      ! vv(:,:,:,Nrhs) = vv(:,:,:,Nrhs) + ext_vf(:,:,:)
      !
      ! Outputs results
      ! CALL iom_put( 'ext_psiu_mle', ext_psiu )
      ! CALL iom_put( 'ext_psiv_mle', ext_psiv )
      !
   ! END SUBROUTINE update_from_mle_c20

END MODULE pyfld
