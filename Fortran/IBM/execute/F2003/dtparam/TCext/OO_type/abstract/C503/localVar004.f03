! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C503/localVar004.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: polymorphic abstract type entities in main program (scalar, array, pointer, allocatable)
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
   type , abstract:: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = 5
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

end module

program localVar004
   use m

   class(base(4)), pointer :: b3
   class(base(4)), allocatable, dimension(:) :: b4

   allocate( child(4,4,20) :: b4(4) )
   allocate(b3, source =child(4,4,20)(4) )

   if (b3%i .ne. 4 ) error stop 1_4
   if (size(b4) .ne. 4 ) error stop 2_4

end program


