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
!*  DEFECT ABSTRACT            : DTPARAM: INTEXP: ACE: 1516-044 Incorrect Output
!*
!*  DESCRIPTION                :
!*  NOTE:  This Defect may be related (or a duplicate) of Defect: 346309.
!*
!*  When the Reduced Code (below) is compiled, the Compiler emits the
!*  following Diagnostic:
!*
!*  1516-044 (S) A conversion from type COMPLEX*16 is not permitted.
!*
!*  If Line 8 is modified to replace the Derived Type Parameter with a
!*  hard-coded Kind value, the Diagnostic will reference Line 10:
!*
!*   complex(4) ::              &
!*      zarr(1) = [complex(kderivedImplied_3):: &
!*                 (realConsts(1), realConsts(2)) ]
!*
!*           10 |              (realConsts(1), realConsts(2))]
!*              ...............a..............................
!*  a - "acetint57kl.f", line 10.15: 1516-044 (S) A conversion from type COMPLEX*16 is not permitted.
!*
!*  If Line 9 is modified to replace the Derived Type Parameter with a
!*  hard-coded Kind value, this Diagnostic will no longer be emitted by
!*  the Compiler:
!*
!*   complex(kderivedImplied_3) ::              &
!*      zarr(1) = [complex(4):: &
!*                 (realConsts(1), realConsts(2)) ]
!*
!*  As well, if both Lines 8 and 9 are modified (as described above), the
!*  Compiler no longer emits this Diagnostic:
!*
!*   complex(4) ::              &
!*      zarr(1) = [complex(4):: &
!*                 (realConsts(1), realConsts(2)) ]
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint57mod

  real(8), parameter :: realConsts(2) = &
    [real(8) :: 9.67646821932d+08, -4.07229178932d+05]

  type derivedImplied (kderivedImplied_3)
     integer, kind :: kderivedImplied_3
     complex(kderivedImplied_3) ::              &
        zarr(1) = [complex(kderivedImplied_3):: &        ! <= Line 9
                   (realConsts(1), realConsts(2)) ]
  end type derivedImplied

end module acetint57mod

program acetint57kl
  use acetint57mod

  type (derivedImplied(4)), allocatable :: dtimpa ! tcx: (1,8,4,2,:,:)

end program acetint57kl
