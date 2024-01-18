!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: typeBound001.f
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
!*  DESCRIPTION                : Testing: Type-bound procedure - Polymorphic abstract type call its own type bound
!*                                        Extension type of the abstract type calling the abstract type's non-deferred type bound
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
   type , abstract:: base
      integer :: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child
   end type

   type, extends(child), abstract :: gen3
     integer :: j = 15
   contains
      procedure, pass :: printgen3 => printgen3
   end type

   type, extends(gen3) :: gen4
   end type

contains

   integer function printbase(a)
      class(base), intent(in) :: a
      printbase = a%i
   end function

   integer function printgen3(a)
      class(gen3), intent(in) :: a
      printgen3 = a%j
   end function

end module

program typeBound001
   use m

   class(base), allocatable, target :: b1
   class(base), pointer             :: b2
   class(gen3), allocatable, target :: g1

   allocate (b1, source = child(7) )
   allocate (b2, source = gen4(1,2))

   allocate(g1, source = gen4(4,9) )

   if ( b1%print() .ne. 7 )      error stop 1_4
   if ( b2%print() .ne. 1 )      error stop 2_4
   if ( g1%print() .ne. 4 )      error stop 3_4
   if ( g1%printgen3() .ne. 9 )  error stop 4_4

end program


