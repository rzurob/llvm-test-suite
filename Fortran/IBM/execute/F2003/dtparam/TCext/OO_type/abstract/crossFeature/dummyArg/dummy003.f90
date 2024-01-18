! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy003.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: dummy003.f
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
!*                                         iii) polymorphic abstract type dummy argument (pointer or allocatable) with
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

contains

   subroutine foo(a)
      class(base(4)), pointer :: a
      if (.not. associated(a) ) error stop 1_4
      if (a%id .ne. 3)          error stop 2_4
   end subroutine

   integer function boo(a)
      class(base(4)), allocatable :: a
      boo = a%id
   end function

end module

program dummy003
   use m
   class(base(4)), allocatable, target :: b1
   class(base(4)), pointer :: b2

   allocate ( b1, source = child(4,4,20)(3) )

   b2 => b1

   call foo(b2)

   if ( boo(b1) .ne. 3 ) error stop 3_4

end program
