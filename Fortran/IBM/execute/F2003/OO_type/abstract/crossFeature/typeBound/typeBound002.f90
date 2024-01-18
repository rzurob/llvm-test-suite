!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound002.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedures: extension type's component is polymorphic abstract
!*                               type and calling abstract type's type bound
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
      integer:: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child
      class(child), pointer :: next
   end type

contains

   integer function printbase(a)
      class(base), intent(in) :: a
      printbase = a%i
   end function

end module

program typeBound002
   use m

   class(child), pointer :: c1
   class(child), pointer :: c2
   type(child), target :: c3

   c3 = child(3,null())

   allocate(c2,c1)
   c2%next => c3
   c1%next => c2

   if (( c1%print() .ne. 5 ) .or. ( c2%print() .ne. 5 ))   error stop 1_4
   if (( c1%next%print() .ne. 5 ) .or. ( c2%next%print() .ne. 3 ))   error stop 2_4
   if ( c1%next%next%print() .ne. 3 ) error stop 3_4
end program

