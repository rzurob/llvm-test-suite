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
! %POSTCMD: dcomp array005.f
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
!*                                        polymorphic abstract type data-ref assigned data, allocate statement
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
      class(base), dimension(:), allocatable :: ptr
   end type

   type, extends(otherbase) :: otherchild
   end type


end module

program array005
   use m

   type(child), target :: c1(5)
   type(otherbase) :: ob1
   class(otherbase), pointer :: ob2
   type(otherchild), allocatable, target :: oc1

   allocate (oc1)
   ob2 => oc1


   allocate(ob1%ptr(5), source = c1 )
      deallocate(ob1%ptr)
   allocate(ob1%ptr(5), source = c1%base )
      deallocate(ob1%ptr)
   allocate(ob1%ptr(3), source = c1(1:3)%base )
      deallocate(ob1%ptr)
   allocate(ob1%ptr(0), source = c1(1:0)%base )
      deallocate(ob1%ptr)

   allocate(ob2%ptr(5), source = c1 )
      deallocate(ob2%ptr)
   allocate(ob2%ptr(5), source = c1%base )
      deallocate(ob2%ptr)
   allocate(ob2%ptr(3), source = c1(1:3)%base )
      deallocate(ob2%ptr)
   allocate(ob2%ptr(0), source = c1(1:0)%base )
      deallocate(ob2%ptr)

   allocate(oc1%ptr(5), source = c1 )
      deallocate(oc1%ptr)
   allocate(oc1%ptr(5), source = c1%base )
      deallocate(oc1%ptr)
   allocate(oc1%ptr(3), source = c1(1:3)%base )
      deallocate(oc1%ptr)
   allocate(oc1%ptr(0), source = c1(1:0)%base )
      deallocate(oc1%ptr)



end program