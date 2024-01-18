!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType005.f
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
!*  DESCRIPTION                : Select Type Construct
!*      Unlimited polymorphic allocated to be poly abstract type or nonpoly extension of abstract type
!*      i.   CLASS is abstract type
!*      ii.  CLASS is abstract type and CLASS is extension type, and put zzrc in CLASS is abstract type
!*      iii. CLASS DEFAULT, access the derived type component
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   type, abstract :: base
      integer :: i = 5
   contains
      procedure, nopass :: print => printbase
   end type

   type, extends(base) :: child
   contains
      procedure, nopass :: print => printchild
   end type

contains

   integer function printbase()
      printbase = 1
   end function

   integer function printchild()
      printchild = 2
   end function

end module

program selectType005
   use m

   class(*), allocatable :: u1
   class(*), pointer :: u2

   class(base), pointer :: b1
   type(child), allocatable, target :: c1

   allocate ( c1, source = child() )
   b1 => c1

   allocate (u1, source = b1 )
   allocate (u2, source = c1 )

   select type ( b => u1 )
      class is (base)
         if (b%print() .ne. 2) error stop 1_4
         if (b%i .ne. 5) error stop 2_4
   end select

   select type ( b => u1 )
      class is (base)
         error stop 3_4
      class is (child)
         if (b%print() .ne. 2) error stop 4_4
         if (b%i .ne. 5) error stop 5_4
   end select

   select type ( u2 )
      class is (base)
         if (u2%print() .ne. 2) error stop 6_4
         if (u2%i .ne. 5)       error stop 7_4
   end select

   select type ( u2 )
      class is (base)
         error stop 8_4
      class is (child)
         if (u2%print() .ne. 2) error stop 9_4
         if (u2%i .ne. 5) error stop 10_4
   end select

end program
