! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/extends/extends003.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Extends keyword, ensure structure components and bindings are inherited for
!*                                        abstract types
!*                                        abstract extends non-abstract
!*
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
   type :: base(k1,k2)    ! (4,2)
      integer, kind :: k1,k2
      integer(k1)   :: id
      integer(k2)   :: id2
   contains
      procedure, nopass :: print => printbase
      procedure, pass :: printid
   end type

   type, extends(base), abstract :: child(k3,n1)    ! (4,2,4,20)
       integer, kind :: k3
       integer, len  :: n1
   end type

   type, extends(child) :: gen3(k4,n2)    ! (4,2,4,20,4,20)
       integer, kind :: k4
       integer, len  :: n2
   contains
      procedure, nopass :: print => printgen3
   end type

   class(child(4,2,4,20)), allocatable :: c1

contains

   subroutine printid(a)
      class(base(4,2)), intent(in) :: a
      print *,a%id, a%id2
   end subroutine

   subroutine printbase()
      print *, "base"
   end subroutine

   subroutine printgen3()
      print *, "gen3"
   end subroutine

end module


program extends003
   use m

   call c1%print()
   allocate(c1, source = gen3(4,2,4,20,4,20)(1,2))

   call c1%printid()
   call c1%print()

end program
