!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: userDefOp009.f
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
!*  DESCRIPTION                : Testing:  User-defined operator and assignment
!*                               User-defined assignment
!*                                  a) both left and right hand side of the assignment are poly abstract type
!*                                     Left         Right
!*                                     Abstract     Abstract
!*                                     Abstract     non-Abstract
!*                                     non-Abstract Abstract
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

program userDefOp009
   use m

   class(base), allocatable, dimension(:) :: b1, b2, b3
   type(child), allocatable, dimension(:) :: c1
   class(child), allocatable, dimension(:) :: c2

   allocate( b1(2), source=(/child(1), child(1) /) )
   allocate( b2(2), source=(/child(2), child(2) /) )
   allocate( b3(2), source=(/child(3), child(3) /) )
   allocate( c1(2), source=(/child(4), child(4) /) )
   allocate( c2(2), source=(/child(5), child(5) /) )

   b1 = b2 + b3
   b2 = c1 + c2

   c1 = c1 + b3
   c2 = c1 + c2

   if ( (b1(1)%id .ne. 5 ) .or. (b1(2)%id .ne. 5 ) ) error stop 1_4
   if ( (b2(1)%id .ne. 9 ) .or. (b2(2)%id .ne. 9 ) ) error stop 2_4
   if ( (c1(1)%id .ne. 7 ) .or. (c1(2)%id .ne. 7 ) ) error stop 3_4
   if ( (c2(1)%id .ne. 12) .or. (c2(2)%id .ne. 12) ) error stop 4_4

end program

function myAdd1(a,b)
   use m, only: base, child
   class(base), intent(in), dimension(:) :: a, b
   type(child) :: myAdd1(size(a))

   if ( size(a) .eq. size(b) ) then
      myAdd1%id = a%id + b%id
   else
      error stop 5_4
   end if
end function

subroutine myAsgn1(a,b)
   use m, only: base, child
   class(base), intent(out), dimension(:) :: a
   class(base), intent(in), dimension(:)  :: b

   if ( size(a) .eq. size(b) ) then
      a%id = b%id
   else
      error stop 6_4
   end if
end subroutine