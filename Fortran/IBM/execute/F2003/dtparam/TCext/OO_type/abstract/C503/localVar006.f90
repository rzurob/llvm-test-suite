! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C503/localVar006.f
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
! %GROUP: localVar006.f
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
!*  DESCRIPTION                : Testing: polymorphic abstract type entities in functions and subroutines (scalar, array, pointer, allocatable)
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
   type , abstract:: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = 5
   end type
   
   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type
   
   interface 
      subroutine foo2 ()
      end subroutine
   end interface
   
contains

   integer function foo1()
      class(base(4)), pointer :: b3
      class(base(4)), allocatable, dimension(:) :: b4
      
      allocate(child(4,4,20)::b4(100))
      allocate(b3, source =child(4,4,20)(1) )
      
      call foo2()
      foo1 = 5
      
      if (b3%i .ne. 1 )      error stop 3_4
      if (size(b4) .ne. 100) error stop 4_4
      
   end function
   
end module

program localVar006
   use m
   
   implicit integer (i)
   
   i = foo1()
   call foo2()
            
end program
   
subroutine foo2 ()
   use m, only: base, child
   class(base(4)), pointer :: b1
   class(base(4)), allocatable, dimension(:) :: b2
   
   allocate(b1, source = child(4,4,20)(4) )   
   allocate(child(4,4,20)::b2(4) )
   
   if (b1%i .ne. 4 ) error stop 1_4
   if (size(b2) .ne. 4 ) error stop 2_4
   
end subroutine
