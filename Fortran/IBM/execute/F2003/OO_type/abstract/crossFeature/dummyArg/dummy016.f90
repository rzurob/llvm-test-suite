 !#######################################################################
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummy016.f
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
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					   dummy argument defined to be abstract type.
!*                                         iii) polymorphic abstract type dummy argument (pointer or allocatable) with array and with
!*                                              a) polymorphic abstract type actual argument
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
      real :: rid
   end type

contains

   subroutine foo(a,b)
      class(base), pointer :: a(:)
      class(base) :: b(2,*)

      if (.not. associated(a) )                                error stop 1_4
      if ((a(1)%id .ne. 3) .or. (a(2)%id .ne. 6))              error stop 2_4
      if ((b(1,1)%id .ne. 3) .or. (b(2,1)%id .ne. 6))          error stop 3_4

   end subroutine

   integer function boo(a)
      class(base), allocatable :: a(:)
      boo = a(1)%id + a(2)%id
   end function

end module

program dummy016
   use m
   class(base), allocatable, target :: b1(:)
   class(base), pointer :: b2(:)

   allocate ( b1(2), source = (/child(3,4.5), child(6,7.8) /) )

   b2 => b1

   call foo(b2,b1)

   if ( boo(b1) .ne. 9 ) error stop 4_4

end program