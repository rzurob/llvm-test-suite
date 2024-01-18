! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/selectType/selectType006.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: selectType006.f
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
!*  DESCRIPTION                : Select Type Construct
!*                               if several CLASS IS type guard statements
!*                               match the selector, one of these statements
!*                               must specify a type that is an extension of
!*                               all the types specified in the others;
!*                               the block following that statement is executed.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: b1(k1)    ! (4)
       integer, kind :: k1
       integer(k1)   :: i
   end type

   type, abstract, extends(b1) :: b2(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

   type, abstract, extends(b2) :: b3(k3,n2)    ! (4,4,20,4,20)
       integer, kind :: k3
       integer, len  :: n2
   end type

   type, extends(b3) :: b4(k4,n3)    ! (4,4,20,4,20,4,20)
       integer, kind :: k4
       integer, len  :: n3
   end type

end module

program selectType006
   use m

   class(b1(4)), allocatable :: b11

   allocate (b11, source = b4(4,4,20,4,20,4,20)(5))

   select type( b => b11 )
      class is (b1(4))
         error stop 1_4
      class is (b2(4,4,*))
         error stop 2_4
      class is (b3(4,4,*,4,*))
         error stop 3_4
      class is (b4(4,4,*,4,*,4,*))
         if (b%i .ne. 5) error stop 4_4
      class default
         error stop 5_4
   end select

end program
