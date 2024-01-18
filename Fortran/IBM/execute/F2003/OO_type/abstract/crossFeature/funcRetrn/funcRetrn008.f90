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
! %GROUP: funcRetrn008.f
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
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), function return cannot be abstract type, class(abstract type)
!*                                        returns array of extension of abstract type
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

   type(child) function foo(a)
      pointer :: foo(:)
      type(child), intent(in) :: a(:)
      allocate(foo(size(a)), source=a) 
   end function
   
   function foo1(a) result (boo)
      class(child), pointer :: boo(:)
      type(child), intent(in) :: a(:)
      allocate(boo(size(a)), source=a ) 
   end function
  
end module

program funcRetrn008
   use m   
   
   class(base), allocatable :: c(:)
   class(child), allocatable :: c1(:)
   
   allocate (c1(2), source = (/child(4), child(5)/) )
   
   allocate ( c(2),source=foo(c1) )
   if (c(1)%id .ne. 4) error stop 1_4
   if (c(2)%id .ne. 5) error stop 2_4
   
   deallocate (c)
   
   allocate ( c(3), source=foo1((/(child(i),i=3,5)/)) ) 
   
   if (c(1)%id .ne. 3) error stop 3_4
   if (c(2)%id .ne. 4) error stop 4_4
   if (c(3)%id .ne. 5) error stop 5_4
end program

