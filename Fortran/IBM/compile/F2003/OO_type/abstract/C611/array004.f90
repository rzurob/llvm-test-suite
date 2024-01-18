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
! %POSTCMD: dcomp array004.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        polymorphic abstract type data-ref assigned data, pointer assignment
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

   type :: otherbase
      class(base), dimension(:), pointer :: ptr
   end type

   type, extends(otherbase) :: otherchild
   end type


end module

program array004
   use m

   type(child), target :: c1(5)
   type(otherbase) :: ob1
   class(otherbase), pointer :: ob2
   type(otherchild), allocatable, target :: oc1

   allocate (oc1)
   ob2 => oc1

   ob1%ptr => c1
   ob1%ptr => c1(1:3)%base
   ob1%ptr => c1(1:0)%base

   ob2%ptr  => c1
   ob2%ptr  => c1(3:5)%base
   ob2%ptr  => c1(1:0)%base

   oc1%ptr  => c1
   oc1%ptr  => c1(2:3)%base
   oc1%ptr  => c1(1:0)%base

end program