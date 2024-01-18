!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: arrayConstr002.f
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
!*  DESCRIPTION                : Testing: array constructor with variable
!*                                        Construct array of polymorphic abstract type
!*                                        Construct array of polymorphic abstract type with zero size
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

   type, abstract :: b1
      integer :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b1) :: b2
   contains
      procedure, nopass :: print => printb2
   end type

   type, extends(b2) :: b3
      contains
      procedure, nopass :: print => printb3
   end type

   interface
      integer function printif()
      end function
   end interface

contains

   integer function printb2()
      printb2 = 2
   end function

   integer function printb3()
      printb3 = 3
   end function

end module

program arrayConstr002
   use m

   class(b1), dimension(:), allocatable :: b11
   class(b1), dimension(:), pointer :: b12

   class(b1), allocatable :: c1
   class(b1), pointer :: c2

   allocate (c1, source = b2(3))
   allocate (c2, source = b3(4))


   allocate(b11(2), source = (/  c1,c1 /) )
   allocate(b12(3), source = (/ (c2,i=1,2),c2 /) )

   if ( (b11(1)%id .ne. 3) .or. (b11(2)%id .ne. 3) ) error stop 1_4
   if ( (b12(1)%id .ne. 4) .or. (b12(2)%id .ne. 4) .or. (b12(3)%id .ne. 4) ) error stop 2_4
   if ( b11%print() .ne. 2 ) error stop 3_4
   if ( b12%print() .ne. 3 ) error stop 4_4

end program