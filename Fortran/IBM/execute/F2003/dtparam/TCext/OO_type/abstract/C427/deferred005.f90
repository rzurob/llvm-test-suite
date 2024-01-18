! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C427/deferred005.f
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
! %GROUP: deferred005.f
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
!*                                        v)	Type defintion contains and inherits deferred bindings, 
!*                                              ABSTRACT defined with different interface (but same interface properties)
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
   
   type, abstract :: b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
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
   use :: m1, newb1 => b1

   type, extends(newb1), abstract :: b2(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   contains
      procedure(printif2), pass, deferred :: print
   end type
   
   type, extends(b2) :: b3(k3,n2)    ! (4,4,20,4,20)
       integer, kind :: k3
       integer, len  :: n2
   contains
      procedure, pass :: print => printb3
   end type   

   interface
      integer function printif2(a)
         import b2
         class(b2(4,4,*)), intent(in) :: a
      end function
   end interface
   
contains

   integer function printb3(a)
      class(b3(4,4,*,4,*)), intent(in) :: a
      printb3=a%id
   end function
    
end module

program deferred005
   use m2
   
   class(newb1(4)), pointer :: b11
   class(b2(4,4,20)), allocatable :: b21
   
   allocate (b11, source = b3(4,4,20,4,20)(2) )
   allocate (b21, source = b3(4,4,20,4,20)(3) )
   
   if ( b11%print() .ne. 2 ) error stop 1_4
   if ( b21%print() .ne. 3 ) error stop 2_4
   
end program
