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

    type base
       integer      x
       complex      y
       character*2  z
    end type base

    type(base), VOLATILE::a

  end program volatileSubobject01
