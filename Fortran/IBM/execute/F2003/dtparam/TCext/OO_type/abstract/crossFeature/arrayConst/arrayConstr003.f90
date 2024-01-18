! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/arrayConst/arrayConstr003.f
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
! %GROUP: arrayConstr003.f
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
!*                                        Construct array of non-polymorphic type with poly abstract variable
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
   
   type :: b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   contains
      procedure, nopass :: print => printb1
   end type
   
   type, extends(b1), abstract :: b2(k2,n1)    ! (4,4,20)
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

   integer function printb1()
      printb1 = 1
   end function
   
   integer function printb2()
      printb2 = 2
   end function
   
   integer function printb3()
      printb3 = 3
   end function
   
end module

program arrayConstr003
   use m  
   
   class(b1(4)), dimension(:), allocatable :: b11
   class(b1(4)), dimension(:), pointer     :: b12
   
   class(b2(4,4,20)), allocatable :: c1
   class(b1(4)), pointer :: c2
   
   allocate (c1, source = b3(4,4,20,4,20)(1) )
   allocate (c2, source = b3(4,4,20,4,20)(3) )
   
   allocate(b11(2), source = (/ c1, c1 /) )
   allocate(b12(3), source = (/ b11((/2,1/)), c2 /) )
   
   if ( (b11(1)%id .ne. 1) .or. (b11(2)%id .ne. 1) ) error stop 1_4
   if ( (b12(1)%id .ne. 1) .or. (b12(2)%id .ne. 1) .or. (b12(3)%id .ne. 3))  error stop 2_4
  
   if (( b11(1)%print() .ne. 3 ) .or. (b11(2)%print() .ne. 3) )error stop 3_4
   if ( ( b12(1)%print() .ne. 3 ) .or. ( b12(2)%print() .ne. 3 ) .or.( b12(3)%print() .ne. 3 ) ) error stop 4_4
   
end program
