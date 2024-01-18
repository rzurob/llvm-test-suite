!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
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
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
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
   
   type, abstract :: b1
      integer :: id
   contains
      procedure(printif1), pass, deferred :: print
   end type

   interface
      integer function printif1(a)
         import b1
         class(b1), intent(in) :: a
      end function
   end interface
   
end module

module m2
   use m1

   type, extends(b1), abstract :: b2
   end type
   
   type, extends(b2) :: b3
   contains
      procedure, pass :: print => printb3
   end type   

contains

   integer function printb3(a)
      class(b3), intent(in) :: a
      printb3=a%id
   end function
    
end module

program deferred004
   use m2
   
   class(b1), pointer :: b11
   class(b2), allocatable :: b21
   
   allocate (b11, source = b3(2) )
   allocate (b21, source = b3(3) )
   
   if ( b11%print() .ne. 2 ) error stop 1_4
   if ( b21%print() .ne. 3 ) error stop 2_4
   
end program