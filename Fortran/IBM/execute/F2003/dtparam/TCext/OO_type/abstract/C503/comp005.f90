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
! %GROUP: comp005.f
! %VERIFY: comp005.out:comp005.vf
! %STDIN:
! %STDOUT: comp005.out
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
!*  DESCRIPTION                : Testing: C503 The TYPE(derived-type-spec) shall not specify an abstract type		    
!*					  polymorphic abstract type components(default init, or private, public)
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
   
   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   end type
   
   type, extends(base) :: child(k2)
      integer, kind :: k2
   end type
   
   type :: b1(k3)
      integer, kind :: k3
      class(base(k3)), pointer :: bptr => null()
   end type

   type :: b2(k4)
      integer, kind :: k4
      class(base(k4)), allocatable, dimension(:) :: balloc
   end type
   
end module

program comp005
   use m
   
   type(b1(4)) :: b11
   class(b2(4)), pointer :: b21
   
   type(child(4,4)), target :: c1 = child(4,4)(5)
    
   b11 = b1(4)(c1) 
   allocate(b21, source = b2(4)((/child(4,4)(3), child(4,4)(4)/)) )
   
   if (.not. associated(b11%bptr) ) error stop 1_4
   if ( size(b21%balloc) .ne. 2)    error stop 2_4
   
   print *, b21%balloc%id
   
end program
