!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: alloc002.f
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
!*  DESCRIPTION                : Testing: Allocate statement - type-spec cannot be non-poly abstract type
!*                                        polymorphic abstract entity to be allocated with abstract polymorphic
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type

end module

program alloc002
   use m
   class(base), allocatable    :: b1
   class(base), allocatable, dimension(:) :: b2
   class(base), allocatable    :: b3

   allocate( b3, source = child(1) )
   allocate( b1, source = b3 )
   allocate( b2(2), source = (/b3,b3/) )

   if (( b1%id .ne. 1 ))  error stop 1_4
   if (( b2(1)%id .ne. 1 ) .or. ( b2(2)%id .ne. 1 )) error stop 2_4
   if ( b3%id .ne. 1 ) error stop 3_4

end program