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
! %GROUP: typeBound003.f
! %VERIFY: typeBound003.out:typeBound003.vf
! %STDIN:
! %STDOUT: typeBound003.out
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
!*  DESCRIPTION                : Testing: Type-bound procedures: extension type of the abstract 
!*                                        type calling the abstract type's non-deferred elemental type bound
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
   
   type, extends(base) :: child1
   end type
   
   type, extends(base) :: child2
   end type

contains

   integer elemental function printbase(a)
      class(base), intent(in) :: a
      printbase = a%i
   end function
   
end module

program typeBound003
   use m
   
   class(base), allocatable, dimension(:) :: b1
   class(child1), allocatable, dimension(:) :: c1
   type(child2) :: c2(3) = (/child2(1), child2(2), child2() /)
   
   allocate (b1(2), source = (/(child1(i),i=1,2)/) )
   allocate (c1(2), source = (/(child1(i),i=1,2)/) )
   
   print *, b1%print()
   print *, c1%print()
   print *, c2%print()
      
end program
   
   