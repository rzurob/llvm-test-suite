!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: deferred004.f
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
!*  DESCRIPTION                : Testing: if the type definition contains or inherits
!*                                        a deferred binding, ABSTRACT shall appear. (C427)
!*                                        iv)	Type definition contains and inherits deferred bindings,
!*                                              ABSTRACT defined
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m1

   type, abstract :: b1(k1)
      integer, kind :: k1
      integer(k1) :: id
   contains
      procedure(printif1), pass, deferred :: print
   end type

   interface
      integer function printif1(a)
         import b1
         class(b1(4)), intent(in) :: a
      end function
   end interface

end module

module m2
   use m1

   type, extends(b1), abstract :: b2(k2)
      integer, kind :: k2
   end type

   type, extends(b2) :: b3(k3)
      integer, kind :: k3
   contains
      procedure, pass :: print => printb3
   end type

contains

   integer function printb3(a)
      class(b3(4,4,4)), intent(in) :: a
      printb3=a%id
   end function

end module

program deferred004
   use m2

   class(b1(4)), pointer :: b11
   class(b2(4,4)), allocatable :: b21

   allocate (b11, source = b3(4,4,4)(2) )
   allocate (b21, source = b3(4,4,4)(3) )

   if ( b11%print() .ne. 2 ) error stop 1_4
   if ( b21%print() .ne. 3 ) error stop 2_4

end program
