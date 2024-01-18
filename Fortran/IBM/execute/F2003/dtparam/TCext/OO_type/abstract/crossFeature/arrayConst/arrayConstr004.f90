! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/arrayConst/arrayConstr004.f
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
! %GROUP: arrayConstr004.f
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
!*  DESCRIPTION                : Testing: array constructor
!*                                        Construct array of unlimited polymorphic type with poly abstract variable
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
   
   interface 
      integer function printif()
      end function
   end interface
      
contains

   integer function printb2()
      printb2 = 2
   end function
      
end module

program arrayConstr004
   use m  
   
   class(*), dimension(:), allocatable :: b11
   
   class(b1(4)), allocatable :: c1
   allocate (c1, source = b2(4,4,20)(1) )
   
   allocate(b11(2), source = (/ c1, c1 /) )
   
   select type (b11)
      class is (b1(4))
         if ( (b11(1)%id .ne. 1) .or. (b11(2)%id .ne. 1) ) error stop 1_4
         if ( (b11(1)%print() .ne. 2 ) .or. (b11(2)%print() .ne. 2) ) error stop 2_4
      class default
         error stop 3_4
   end select
   
end program
