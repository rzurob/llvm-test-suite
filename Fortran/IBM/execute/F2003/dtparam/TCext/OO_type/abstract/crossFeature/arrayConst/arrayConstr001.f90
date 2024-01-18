! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/arrayConst/arrayConstr001.f
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
! %GROUP: arrayConstr001.f
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
!*  DESCRIPTION                : Testing: array constructor with structure constructor
!*                                        Construct array of polymorphic abstract type
!*                                        Construct array of polymorphic abstract type with zero size  
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
   
   type, abstract :: b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type
   
   type, extends(b1) :: b2(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   contains
      procedure, nopass :: print => printb2
   end type
   
   type, extends(b2) :: b3(k3,n2)    ! (4,4,20,4,20)
       integer, kind :: k3
       integer, len  :: n2
      contains
      procedure, nopass :: print => printb3
   end type
   
   interface
      integer function printif()
      end function
   end interface
   
contains
   
   integer function printb2()
      printb2 = 2
   end function
   
   integer function printb3()
      printb3 = 3
   end function
   
end module

program arrayConstr001
   use m  
   
   class(b1(4)), dimension(:), allocatable :: b11
   class(b1(4)), dimension(:), pointer :: b12
   
   allocate(b11(2), source = (/  b2(4,4,20)(5), b2(4,4,20)(4) /) )
   allocate(b12(2), source = (/ (b3(4,4,20,4,20)(i),i=5,4,-1) /) )
   
   if ( (b11(1)%id .ne. 5) .or. (b11(2)%id .ne. 4) ) error stop 1_4
   if ( (b12(1)%id .ne. 5) .or. (b12(2)%id .ne. 4) ) error stop 2_4
   if ( b11%print() .ne. 2 ) error stop 3_4
   if ( b12%print() .ne. 3 ) error stop 4_4
   
   deallocate (b11, b12)
   
   allocate (b11(0), source =(/ ( b3(4,4,20,4,20)(i), i=1,0) /) )
   allocate (b12(0), source =(/ ( b2(4,4,20)(i), i=1,0) /) )
   
   if ( b11%print() .ne. 3 ) error stop 5_4
   if ( b12%print() .ne. 2 ) error stop 6_4
   
end program
