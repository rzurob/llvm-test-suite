! GB DTP extension using:
! ftcx_dtp -qreuse=self /tstdev/F2003/volatile/test/volatileSubobject01.f
! opt variations: -qck -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : subobject, VOLATILE
!*
!*  DESCRIPTION                : functional TC
!*
!*     5.1.2.16
!*        an object may have the volatile attribute in a particular scoping
!*        unit without necessarily having it in other scoping units. If
!*        an object has the Volatile attribute, then all of its subobject
!*        also have the VOLATILE attribute.
!* ===================================================================

  program volatileSubobject01

    type base(k1,n1)    ! (4,2)
       integer, kind :: k1
       integer, len  :: n1
       integer(k1)      x
       complex(k1)      y
       character(n1)    z
    end type base

    type(base(4,2)), VOLATILE::a

  end program volatileSubobject01
