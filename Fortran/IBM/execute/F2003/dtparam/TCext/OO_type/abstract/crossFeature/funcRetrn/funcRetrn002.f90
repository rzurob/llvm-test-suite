! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn002.f
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
! %GROUP: funcRetrn002.f
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
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic scalar abstract non-base type
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
   
   type, abstract:: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type
   
   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type
   
   type, extends(child), abstract :: gen3(k3,n2)    ! (4,4,20,4,20)
       integer, kind :: k3
       integer, len  :: n2
   end type
   
   type, extends(gen3) :: gen4(k4,n3)    ! (4,4,20,4,20,4,20)
       integer, kind :: k4
       integer, len  :: n3
   end type

contains

   class(gen3(4,4,:,4,:)) function foo(a)
      pointer :: foo
      class(gen3(4,4,*,4,*)), intent(in) :: a
      allocate(foo, source = a) 
   end function
   
   function foo1(a) result (boo)
      class(gen3(4,4,*,4,*)), intent(in) :: a      
      class(gen3(4,4,a%n1,4,a%n2)), pointer :: boo
      allocate(boo, source=a) 
   end function
  
end module

program funcRetrn002
   use m   
   
   class(base(4)), pointer :: c
   class(gen3(4,4,20,4,20)), allocatable :: g1
   
   allocate(g1, source = gen4(4,4,20,4,20,4,20)(5))
   
   c => foo1(g1)
   if (c%id .ne. 5) error stop 1_4
   
   deallocate(c)
   
   allocate ( c, source = foo(gen4(4,4,20,4,20,4,20)(4)))
   if (c%id .ne. 4) error stop 2_4    
   
end program

