! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy010.f
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
! %GROUP: dummy010.f
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
!*                                         c) OPTIONAL attribute with polymorphic abstract type (pointer or allocatable) 
!*                                            2) if actual argument is associated, try
!*                                               i) polymorphic abstract type actual argument
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
   
   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type

contains

   subroutine foo(a, b)
      class(base(4)) :: a
      class(base(4)), optional, allocatable :: b
      
      if (a%id .ne. 3) error stop 1_4
      if (present(b) ) then
         if (b%id .ne. 3) error stop 2_4
      else
         error stop 3_4
      end if
      
   end subroutine

   integer function boo(a, b)
      class(base(4)) :: a
      class(base(4)), optional, pointer :: b
      if ( present(b) ) then
         boo = a%id + b%id
      else
         boo = 30
      end if
   end function

end module

program dummy010
   use m
   
   class(base(4)), allocatable :: b1
   class(base(4)), pointer :: b2
   type(child(4,4)), target :: c1 = child(4,4)(3,3.4)
      
   allocate (b1, source = child(4,4)(3,2.3))
   allocate (b2, source = child(4,4)(3,2.3))
     
   call foo(b1, b1)
   call foo(c1, b1)
   
   if ( boo(c1, b2) .ne. 6 ) error stop 4_4
   if ( boo(b1, b2) .ne. 6 ) error stop 5_4
   
end program
