!**********************************************************************
!* ====================================================================
!*
!*  TEST CASE NAME             : d346415
!*
!*  DATE                       : 2008-01-28
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM: INTEXP: ACE: AC-IMPDO: ICE: In
!*                               xlfentry ("array_ld()")
!*
!*  DESCRIPTION                :
!*  When compiled, the Reduced Code (below) causes the Compiler to ICE in
!*  "xlfentry" (refer to the Traceback below).  The ICE no longer occurs
!*  if Line 10 is modified as follows:
!*
!*          ((realConsts(1), realConsts(2)), i=0,0)]
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint57mod

  real(8), parameter :: realConsts(2) = &
    [real(8) :: 9.67646821932d+08, -4.07229178932d+05]

  type derivedImplied (kderivedImplied_3)
     integer, kind :: kderivedImplied_3
     complex(kderivedImplied_3) ::              &
        zarr(1) = [complex(kderivedImplied_3):: &
            ((realConsts(i+1), realConsts(i+2)), i=0,0)] ! <= Line 10 - ICE
  end type derivedImplied

end module acetint57mod

program d346415
  use acetint57mod

  type (derivedImplied(4)), allocatable :: dtimpa ! tcx: (1,8,4,2,:,:)

end program d346415
