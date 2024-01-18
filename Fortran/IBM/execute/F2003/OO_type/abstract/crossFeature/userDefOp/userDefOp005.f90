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
! %GROUP: userDefOp005.f
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
!*  DESCRIPTION                : Testing:  User-defined operator and assignment
!*                               a) both operands are polymorphic abstract type for the operator in the interface and supplying
!*                                  5) array abstract declared type and extension declared type
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
      integer :: id
   end type
   
   type, extends(base) :: child
   end type
   
   interface operator(+)
      function myAdd1(a,b)
         import base, child
         class(base), intent(in), dimension(:) :: a, b
         type(child) :: myAdd1 (size(a))
      end function
   end interface
   
   interface assignment(=)
      subroutine myAsgn1(a,b)
         import base, child
         class(base), intent(out), dimension(:) :: a
         class(base), intent(in), dimension(:)  :: b
      end subroutine
   end interface

end module

program userDefOp005
   use m
   
   class(base), dimension(:), allocatable :: c1, c3, c4
   type(child), dimension(:), allocatable :: c2
   
   allocate(c1(2), source=(/ child(3), child(3) /) )
   allocate(c2(2), source=(/ child(1), child(1) /)  )
   allocate(c3(2), source=(c1+c2) )
   allocate(c4(2), source=(c1+c2+c3) )
      
   if ( ( c3(1)%id .ne. 4 ) .or. ( c3(2)%id .ne. 4 ) ) error stop 1_4
   if ( ( c4(1)%id .ne. 8 ) .or. ( c4(2)%id .ne. 8 ) ) error stop 2_4

end program


function myAdd1(a,b)
   use m, only: base, child
   class(base), intent(in), dimension(:) :: a, b
   type(child) :: myAdd1(size(a))
   
   if ( size(a) .eq. size(b) ) then
      myAdd1%id = a%id + b%id
   else
      error stop 3_4
   end if
end function

subroutine myAsgn1(a,b)
   use m, only: base, child
   class(base), intent(out), dimension(:) :: a
   class(base), intent(in), dimension(:)  :: b

   if ( size(a) .eq. size(b) ) then
      a%id = b%id
   else
      error stop 4_4
   end if
end subroutine
