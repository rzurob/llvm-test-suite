!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp structConstr002.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: The derived-type-spec shall not specify an abstract type (C401)
!*                                        Structure Constructor as Source in Allocate statement (for pointer/allocatable)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
      real :: rid
   end type

end module

program structConstr002
   use m
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   type(child), allocatable :: c1
   type(child), pointer     :: c2
   class(*), allocatable    :: u1
   class(*), pointer        :: u2

   allocate(b1,source=base(4))
   allocate(b2,source=base(5))
   allocate(c1,source=child(base=base(1),rid=5.6))
   allocate(c2,source=child(base=base(2),rid=7.8))
   allocate(u1,source=child(base=base(3),rid=9.1))
   allocate(u2,source=child(base=base(4),rid=2.3))

end program
