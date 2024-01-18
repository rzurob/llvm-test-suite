! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/allocate/alloc004.f
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
! %GROUP: alloc004.f
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
!*  DESCRIPTION                : Testing: Allocate statement - type-spec cannot be non-poly abstract type
!*                                        non-abstract polymorphic entity to be allocated with abstract polymorphic type
!*                                        and non-abstract types
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
   
   type :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type
   
   type, extends(base), abstract :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type
   
   type, extends(child) :: gen3(k3,n2)    ! (4,4,20,4,20)
       integer, kind :: k3
       integer, len  :: n2
   end type
     
end module

program alloc004
   use m
   
   class(base(4)), allocatable    :: b1
   class(base(4)), allocatable, dimension(:) :: b2

   class(child(4,4,20)), allocatable :: c1
   class(child(4,4,20)), allocatable, dimension(:) :: c2

   class(gen3(4,4,20,4,20)), allocatable :: g1
   type(gen3(4,4,20,4,20))               :: g2 = gen3(4,4,20,4,20)(2)
   
   allocate(g1, source = gen3(4,4,20,4,20)(1) )
   
   allocate(c1, source=g2)
   allocate(c2(2), source=(/gen3(4,4,20,4,20)(1), g2 /))
   
   if ( c1%id .ne. 2 ) error stop 1_4
   if (( c2(1)%id .ne. 1 ) .or. (c2(2)%id .ne. 2)) error stop 2_4
   
   allocate( b1, source = c1 )
   allocate( b2(2), source = c2 )
   
   if ( b1%id .ne. 2 ) error stop 3_4
   if (( b2(1)%id .ne. 1 ) .or. (b2(2)%id .ne. 2)) error stop 4_4
   
   deallocate(b1, b2)

   allocate( b1, source = g1 )
   allocate( b2(2), source = (/g1,g2/) )
   
   if ( b1%id .ne. 1 ) error stop 5_4
   if (( b2(1)%id .ne. 1 ) .or. (b2(2)%id .ne. 2)) error stop 6_4
   
end program
