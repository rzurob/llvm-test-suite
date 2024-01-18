!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: funcRetrn009.f
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
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic abstract base type array in subfunction
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
   end type


contains

   class(base) function foo(a)
      pointer :: foo(:)
      class(base), intent(in) :: a(:)
      allocate(foo(size(a)), source=innerfoo(a) )
   contains
      function innerfoo(a) result(boo)
         class(base), pointer :: boo(:)
         class(base), intent(in) :: a(:)
         allocate(boo(size(a)), source=a)
      end function
   end function

end module

program funcRetrn009
   use m

   class(base), allocatable :: c(:)
   class(base), allocatable :: b1(:)
   allocate (b1(2), source = (/ child(4), child(5) /))
   allocate ( c(2),source=foo(b1) )

   if ( c(1)%id .ne. 4) error stop 1_4
   if ( c(2)%id .ne. 5) error stop 2_4

end program

