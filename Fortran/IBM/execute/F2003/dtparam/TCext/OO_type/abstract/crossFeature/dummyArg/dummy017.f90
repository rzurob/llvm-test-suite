! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy017.f
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
! %GROUP: dummy017.f
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
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type		    
!*					   dummy argument defined to be abstract type.
!*                                         polymorphic non-abstract type dummy argument (non-pointer and non-allocatable) with array with
!*                                         a)polymorphic non-abstract type actual argument
!*                                         b)polymorphic extension type (abstract) of non-abstract type actual argument
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   
   type :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type
   
   type, abstract, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type
   
   type, extends(child) :: gen3(k3,n1)    ! (4,4,4,20)
       integer, kind :: k3
       integer, len  :: n1
   end type

contains

   subroutine foo(a,b)
      class(base(4)) :: a(:)
      class(base(4)) :: b(2,*)
      
      if ((a(1)%id .ne. 3) .or. (a(2)%id .ne. 6))              error stop 1_4
      if ((b(1,1)%id .ne. 3) .or. (b(2,1)%id .ne. 6))          error stop 2_4
   end subroutine

   integer function boo(a)
      class(base(4)) :: a(:)
      boo = a(1)%id + a(2)%id
   end function

end module

program dummy017
   use m
   
   class(base(4)), allocatable :: b1(:)
   class(child(4,4)), allocatable, target :: c1(:)
   class(child(4,4)), pointer :: c2(:)
   
   allocate (b1(2), source = (/ base(4)(3), base(4)(6) /))
   allocate (c1(2), source = (/ gen3(4,4,4,20)(3,4.5), gen3(4,4,4,20)(6,7.8) /))
   c2 => c1
      
   call foo(b1,b1)
   call foo(c1,c1)
   call foo(c2,c2)
   
   if ( boo(b1) .ne. 9 ) error stop 3_4
   if ( boo(c1) .ne. 9 ) error stop 4_4
   if ( boo(c2) .ne. 9 ) error stop 5_4
   
end program
