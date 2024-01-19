!***********************************************************************
!* =====================================================================
!*
!*                               by David Forster)
!*  DATE                       : 2008-01-25 (original: 2006-11-23)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters (+ Array Constructor
!*                               Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement default
!*                               initialization of derived type _component_
!*                               (intrinsics)
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : intrinsic, component, default initialisation,
!*                               default initialization
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Create a derived type with intrinsic array compoments; initialise these
!*  with a variety of AC's.
!*
!*  Changes:
!*  2007-02-12 dforster  constants modified to avoid inevitable roundoff error
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint57mod

  implicit none

  character(9), parameter :: charConsts(2) = [character(9):: 'prolixity', 'obviate']
  real(8), parameter :: realConsts(4) = [real(8) :: 9.67646821932d+08, -4.07229178932d+05, 11.97143634432d0, 3.141592653589793d+012]
  complex(16), parameter :: zConsts(2) = [complex(16) :: (9.81521024907605128983541q11, -5.79670837908828653852503q9), &
       & (-7.8870270068097745637302q+034, -1.5707963267948966192313216916398q+026)]
  integer :: i,j
  logical, parameter :: lConsts(5) = [(j<=3, j=1,5)]

  type derivedExplicit (kderivedExplicit_1,kderivedExplicit_2,kderivedExplicit_3,kderivedExplicit_4,lderivedExplicit_1,lderivedExplicit_2) ! kderivedExplicit_1,kderivedExplicit_2,kderivedExplicit_3,kderivedExplicit_4,lderivedExplicit_1,lderivedExplicit_2=1,4,8,2,9,6
     integer, kind :: kderivedExplicit_1,kderivedExplicit_2,kderivedExplicit_3,kderivedExplicit_4
     integer, len :: lderivedExplicit_1,lderivedExplicit_2
     ! random values:
     integer (kderivedExplicit_4)      :: iarr(2)  = [1955, 6444]
     real(kderivedExplicit_3)          :: rarr(2)  = [9.81521024907605128983541d11, -5.79670837908828653852503d9]
     complex(kderivedExplicit_2)       :: zarr(2)  = [(9.67646821932e+08, -4.07229178932e+05), (11.97143634432, 3.141592653589793e+012)]
     logical(kderivedExplicit_1)       :: larr(lderivedExplicit_2)  = [.true., .false., .true., .false., .true., .true.]
     double precision :: dparr(2) = [double precision:: -7.887027000000001d+084, -1.16395046d+076]
     double complex   :: dzarr(2) = [(-6.29742670042620296d+024, -1.0387858299999999d+027), &
          & (-5.39523550003193546d+057, -8.69775020011333044d+094)]
     character(lderivedExplicit_1)     :: carr(2)  = [character(9)::     'prolixity', 'obviation']
  end type derivedExplicit

  type derivedImplied (kderivedImplied_1,kderivedImplied_2,kderivedImplied_3,kderivedImplied_4,lderivedImplied_1,lderivedImplied_2) ! kderivedImplied_1,kderivedImplied_2,kderivedImplied_3,kderivedImplied_4,lderivedImplied_1,lderivedImplied_2=1,8,4,2,5,9
     integer, kind :: kderivedImplied_1,kderivedImplied_2,kderivedImplied_3,kderivedImplied_4
     integer, len :: lderivedImplied_1,lderivedImplied_2! use implied-do's, change the order:
     character(lderivedImplied_2)     :: carr(2)  = charConsts
     complex(kderivedImplied_3)       :: zarr(2)  = [cmplx(sin(realConsts(1)), cos(realConsts(1))), cmplx(sin(realConsts(3)), cos(realConsts(3)))]
     double complex   :: dzarr(2) = zConsts
     double precision :: dparr(2) = dsqrt([7.887027000000001d+084, 2*7.887027000000001d+084])
     integer (kderivedImplied_3)      :: iarr(2)  = [1955+ 6444*1, 1955+6444*2]
     logical(kderivedImplied_1)       :: larr(lderivedImplied_1)!  = lConsts
     real(kderivedImplied_2)          :: rarr(2)  = [real(zConsts(2)), aimag(zConsts(2))]
  end type derivedImplied

end module acetint57mod


program acetint57kl

  use acetint57mod
  implicit none
  type (derivedExplicit(1,4,8,2,9,6)) :: dtex ! tcx: (1,4,8,2,9,6)
  type (derivedExplicit(1,4,8,2,:,:)), allocatable :: dtexa ! tcx: (1,4,8,2,:,:)

  type (derivedImplied(1,8,4,2,5,9)) :: dtimp ! tcx: (1,8,4,2,5,9)
  type (derivedImplied(1,8,4,2,:,:)), allocatable :: dtimpa ! tcx: (1,8,4,2,:,:)

  dtimp%larr = lConsts
  write (*,'(2i5,2d15.5,4e15.5,6l2,2d15.5,4d15.5,2a10)') dtex
  allocate(derivedExplicit(1,4,8,2,9,6) :: dtexa)
  write (*,'(2i5,2d15.5,4e15.5,6l2,2d15.5,4d15.5,2a10)') dtexa

  write (*, '(2a10,8d15.5, 2d15.5,2i5, 5l3, 2d15.5)') dtimp
  allocate(derivedImplied(1,8,4,2,5,9) :: dtimpa)

  dtimpa%larr = lConsts
  write (*, '(2a10,8d15.5, 2d15.5,2i5, 5l3, 2d15.5)') dtimpa

end program acetint57kl

