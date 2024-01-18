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
! %POSTCMD: dcomp structConstr003.f
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
!*                                        Structure Constructor as Array Constructor (for pointer/allocatable)
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

program structConstr003
   use m
   class(base), allocatable, dimension(:) :: b1
   type(child), pointer :: c1(:)
   class(*), allocatable :: u1(:)

   allocate(b1(3), source = (/ base(2), base(3), base(4) /) )
   allocate(c1(2), source = (/ child(base=base(2),rid=5.5), child(base=base(3),rid=5.7) /) )
   allocate(u1(3), source = (/ base(2), base(3), base(4) /) )
end program