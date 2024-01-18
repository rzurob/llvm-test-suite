! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/extends/extends001.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: extends001.f
! %VERIFY: extends001.out:extends001.vf
! %STDIN:
! %STDOUT: extends001.out
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
!*  DESCRIPTION                : Testing: Extends keyword
!*                                        ensure structure components and bindings are inherited for
!*                                        abstract types (abstract extends abstract and non-abstract
!*                                        extends abstract)
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

module m1

   type, abstract :: base(k1,k2)    ! (4,4)
      integer, kind        :: k1,k2
      integer(k1)          :: id = 5
      integer(k2), pointer :: ptr => null()
   contains
      procedure, nopass :: print => printbase
   end type

contains

   subroutine printbase()
      print *,'base'
   end subroutine

end module

module m2
   use m1

   type, extends(base), abstract :: child(k3)    ! (4,4,4)
      integer, kind :: k3
      integer(k3)   :: rid = 4
   contains
      procedure, nopass :: print => printchild
   end type

   type, extends(child) :: gen3(k4,n1)    ! (4,4,4,4,20)
       integer, kind :: k4
       integer, len  :: n1
   end type

   class(base(4,4)), allocatable :: b1
   class(child(4,4,4)), allocatable :: c1
   class(gen3(4,4,4,4,20)), pointer :: g1

contains
   subroutine printchild()
      print *,'child'
   end subroutine
end module


program extends001
   use m2

   call b1%print()
   call c1%print()
   call g1%print()

   allocate (b1, source = gen3(4,4,4,4,20)(5, null(), 6))
   allocate (c1, source = gen3(4,4,4,4,20)(6, null(), 7))
   allocate (g1, source = gen3(4,4,4,4,20)())

   allocate (g1%ptr, source = 45)
   c1%ptr => g1%ptr
   b1%ptr => c1%ptr

   print *, b1%id, c1%id, c1%rid, g1%id, g1%rid, b1%ptr, c1%ptr, g1%ptr

end program
