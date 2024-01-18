! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy006.f
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
! %GROUP: dummy006.f
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
!*                                         unlimited polymorphic dummy argument (non-pointer and non-allocatable) with
!*                                         d) unlimited actual argument
!*                                            1) of non-abstract dynamic type 
!*                                            
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
      integer(k2)   :: rid
   end type

contains

   subroutine foo(a)
      class(*) :: a
      select type (a)
         class is (base(4))
            error stop 1_4
         class is (child(4,4))
            if (a%id .ne. 1)    error stop 2_4
            if (a%rid .ne. 2)    error stop 3_4
      end select
   end subroutine

   integer function boo(a)
      class(*) :: a
      select type (a)
         class is (base(4))
            boo=1
         class is (child(4,4))
            boo=2
      end select
   end function

end module

program dummy006
   use m
   
   class(*), allocatable, target :: u1
   class(*), pointer :: u2
   
   class(base(4)), allocatable :: b1
   allocate (b1, source = child(4,4)(1,2))
  
   allocate (u1, source = b1)
   u2 => u1
      
   call foo(u1)
   call foo(u2)
      
   if ( boo(u1) .ne. 2 ) error stop 4_4
   if ( boo(u2) .ne. 2 ) error stop 5_4
   
end program
